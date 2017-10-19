// -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c++ et

#include "crc_nif.h"
#include "crc_model.h"
#include "checksum_xor.h"
#include "crc_8.h"
#include "crc_16.h"
#include "crc_32.h"
#include "xnif_slice.h"

static ErlNifResourceType *crc_nif_resource_type = NULL;

static ERL_NIF_TERM ATOM_check;
static ERL_NIF_TERM ATOM_error;
static ERL_NIF_TERM ATOM_false;
static ERL_NIF_TERM ATOM_init;
static ERL_NIF_TERM ATOM_key;
static ERL_NIF_TERM ATOM_name;
static ERL_NIF_TERM ATOM_nil;
static ERL_NIF_TERM ATOM_normal;
static ERL_NIF_TERM ATOM_ok;
static ERL_NIF_TERM ATOM_poly;
static ERL_NIF_TERM ATOM_refin;
static ERL_NIF_TERM ATOM_refout;
static ERL_NIF_TERM ATOM_residue;
static ERL_NIF_TERM ATOM_sick;
static ERL_NIF_TERM ATOM_true;
static ERL_NIF_TERM ATOM_type;
static ERL_NIF_TERM ATOM_width;
static ERL_NIF_TERM ATOM_xorout;

static ERL_NIF_TERM
crc_nif_crc_info_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    const crc_context_t *c = NULL;

    if (argc < 1 || !enif_get_resource(env, argv[0], crc_nif_resource_type, (void **)&c)) {
        return enif_make_badarg(env);
    }

    ERL_NIF_TERM key;
    ERL_NIF_TERM val;
    ERL_NIF_TERM map = enif_make_new_map(env);

    key = ATOM_type;
    val = (c->model->type == CRC_MODEL_TYPE_NORMAL) ? ATOM_normal : ATOM_sick;
    (void)enif_make_map_put(env, map, key, val, &map);

    key = ATOM_key;
    val = c->model->key;
    (void)enif_make_map_put(env, map, key, val, &map);

    key = ATOM_name;
    size_t name_len = strnlen(c->model->name, sizeof(c->model->name));
    if (name_len == 0) {
        val = ATOM_nil;
    } else {
        unsigned char *name_buf = enif_make_new_binary(env, name_len, &val);
        (void)memcpy(name_buf, c->model->name, name_len);
    }
    (void)enif_make_map_put(env, map, key, val, &map);

    if (c->model->type != CRC_MODEL_TYPE_NORMAL) {
        return map;
    }

    const crc_params_t *p = (void *)c->model;

    key = ATOM_width;
    val = enif_make_uint(env, (unsigned int)p->width);
    (void)enif_make_map_put(env, map, key, val, &map);

    key = ATOM_poly;
    val = enif_make_uint64(env, (ErlNifUInt64)p->poly);
    (void)enif_make_map_put(env, map, key, val, &map);

    key = ATOM_init;
    val = enif_make_uint64(env, (ErlNifUInt64)p->init);
    (void)enif_make_map_put(env, map, key, val, &map);

    key = ATOM_refin;
    val = (p->refin) ? ATOM_true : ATOM_false;
    (void)enif_make_map_put(env, map, key, val, &map);

    key = ATOM_refout;
    val = (p->refout) ? ATOM_true : ATOM_false;
    (void)enif_make_map_put(env, map, key, val, &map);

    key = ATOM_xorout;
    val = enif_make_uint64(env, (ErlNifUInt64)p->xorout);
    (void)enif_make_map_put(env, map, key, val, &map);

    key = ATOM_check;
    val = enif_make_uint64(env, (ErlNifUInt64)p->check);
    (void)enif_make_map_put(env, map, key, val, &map);

    key = ATOM_residue;
    val = enif_make_uint64(env, (ErlNifUInt64)p->residue);
    (void)enif_make_map_put(env, map, key, val, &map);

    return map;
}

static ERL_NIF_TERM crc_nif_crc_init_1_atom(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM crc_nif_crc_init_1_map(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM crc_nif_crc_init_1_ref(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM crc_nif_crc_init_1_tuple(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

static ERL_NIF_TERM
crc_nif_crc_init_1_atom(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    const crc_model_t *model = NULL;

    if (!crc_model_get(argv[0], &model)) {
        return enif_make_badarg(env);
    }

    if (model->type != CRC_MODEL_TYPE_NORMAL) {
        return enif_make_badarg(env);
    }

    crc_params_t *p = (void *)model;
    crc_context_t *c = (void *)enif_alloc_resource(crc_nif_resource_type, sizeof(crc_context_t));
    if (c == NULL) {
        return enif_make_badarg(env);
    }
    c->model = (const crc_model_t *)p;
    c->value = p->init;

    ERL_NIF_TERM out_term;
    out_term = enif_make_resource(env, (void *)c);
    (void)enif_release_resource((void *)c);

    return out_term;
}

static ERL_NIF_TERM
crc_nif_crc_init_1_map(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    size_t arity;

    if (argc != 1 || !enif_get_map_size(env, argv[0], &arity) || arity < 8) {
        return enif_make_badarg(env);
    }

    ERL_NIF_TERM width;
    ERL_NIF_TERM poly;
    ERL_NIF_TERM init;
    ERL_NIF_TERM refin;
    ERL_NIF_TERM refout;
    ERL_NIF_TERM xorout;
    ERL_NIF_TERM check;
    ERL_NIF_TERM residue;

    if (!enif_get_map_value(env, argv[0], ATOM_width, &width) || !enif_get_map_value(env, argv[0], ATOM_poly, &poly) ||
        !enif_get_map_value(env, argv[0], ATOM_init, &init) || !enif_get_map_value(env, argv[0], ATOM_refin, &refin) ||
        !enif_get_map_value(env, argv[0], ATOM_refout, &refout) || !enif_get_map_value(env, argv[0], ATOM_xorout, &xorout) ||
        !enif_get_map_value(env, argv[0], ATOM_check, &check) || !enif_get_map_value(env, argv[0], ATOM_residue, &residue)) {
        return enif_make_badarg(env);
    }

    ERL_NIF_TERM newargv[1];
    newargv[0] = enif_make_tuple8(env, width, poly, init, refin, refout, xorout, check, residue);
    return crc_nif_crc_init_1_tuple(env, 1, newargv);
}

static ERL_NIF_TERM
crc_nif_crc_init_1_ref(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    const crc_context_t *c = NULL;

    if (argc < 1 || !enif_get_resource(env, argv[0], crc_nif_resource_type, (void **)&c)) {
        return enif_make_badarg(env);
    }

    if (c->model->type != CRC_MODEL_TYPE_NORMAL) {
        return enif_make_badarg(env);
    }

    if (c->model->key != ATOM_nil) {
        crc_context_t *new_c = (void *)enif_alloc_resource(crc_nif_resource_type, sizeof(crc_context_t));
        if (new_c == NULL) {
            return enif_make_badarg(env);
        }
        new_c->model = c->model;
        new_c->value = ((const crc_params_t *)new_c->model)->init;

        ERL_NIF_TERM out_term;
        out_term = enif_make_resource(env, (void *)new_c);
        (void)enif_release_resource((void *)new_c);

        return out_term;
    }

    crc_context_t *new_c = (void *)enif_alloc_resource(crc_nif_resource_type, sizeof(crc_context_t) + sizeof(crc_params_t));
    if (new_c == NULL) {
        return enif_make_badarg(env);
    }
    crc_params_t *p = (void *)(((uint8_t *)new_c) + sizeof(crc_context_t));
    (void)memcpy(p, c->model, sizeof(crc_params_t));
    new_c->model = (const crc_model_t *)p;
    new_c->value = p->init;

    ERL_NIF_TERM out_term;
    out_term = enif_make_resource(env, (void *)new_c);
    (void)enif_release_resource((void *)new_c);

    return out_term;
}

static ERL_NIF_TERM
crc_nif_crc_init_1_tuple(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    int arity;
    const ERL_NIF_TERM *tuple;

    if (argc != 1 || !enif_get_tuple(env, argv[0], &arity, &tuple)) {
        return enif_make_badarg(env);
    }

    unsigned int width;
    ErlNifUInt64 poly;
    ErlNifUInt64 init;
    ErlNifUInt64 xorout;
    ErlNifUInt64 check;
    ErlNifUInt64 residue;

    if (arity != 8 || !enif_get_uint(env, tuple[0], &width) || width > 0x40 || !enif_get_uint64(env, tuple[1], &poly) ||
        !enif_get_uint64(env, tuple[2], &init) || !(tuple[3] == ATOM_false || tuple[3] == ATOM_true) ||
        !(tuple[4] == ATOM_false || tuple[4] == ATOM_true) || !enif_get_uint64(env, tuple[5], &xorout) ||
        !enif_get_uint64(env, tuple[6], &check) || !enif_get_uint64(env, tuple[7], &residue)) {
        return enif_make_badarg(env);
    }

    ErlNifUInt64 max = ((1 << width) - 1);

    // XNIF_TRACE_F("max = %llu, poly = %llu, init = %llu, xorout = %llu, check = %llu, residue = %llu\n", max, poly, init, xorout,
    // check, residue);
    if (poly > max || init > max || xorout > max || check > max || residue > max) {
        return enif_make_badarg(env);
    }

    crc_params_t p_buff;
    crc_params_t *p = &p_buff;

    p->model.key = ATOM_nil;
    p->model.type = CRC_MODEL_TYPE_NORMAL;
    p->model.keystring[0] = '\0';
    p->model.name[0] = '\0';
    p->table = NULL;
    p->width = (uint8_t)width;
    p->poly = (uint64_t)poly;
    p->init = (uint64_t)init;
    p->refin = (tuple[4] == ATOM_true) ? true : false;
    p->refout = (tuple[5] == ATOM_true) ? true : false;
    p->xorout = (uint64_t)xorout;
    p->check = (uint64_t)check;
    p->residue = (uint64_t)residue;

    const crc_model_t *model = NULL;

    if (crc_model_match(p, &model)) {
        p = (void *)model;
        crc_context_t *c = (void *)enif_alloc_resource(crc_nif_resource_type, sizeof(crc_context_t));
        if (c == NULL) {
            return enif_make_badarg(env);
        }
        c->model = (const crc_model_t *)p;
        c->value = p->init;

        ERL_NIF_TERM out_term;
        out_term = enif_make_resource(env, (void *)c);
        (void)enif_release_resource((void *)c);

        return out_term;
    }

    crc_context_t *c = (void *)enif_alloc_resource(crc_nif_resource_type, sizeof(crc_context_t) + sizeof(crc_params_t));
    if (c == NULL) {
        return enif_make_badarg(env);
    }
    p = (void *)(((uint8_t *)c) + sizeof(crc_context_t));
    (void)memcpy(p, &p_buff, sizeof(p_buff));
    c->model = (const crc_model_t *)p;
    c->value = p->init;

    ERL_NIF_TERM out_term;
    out_term = enif_make_resource(env, (void *)c);
    (void)enif_release_resource((void *)c);

    return out_term;
}

static ERL_NIF_TERM
crc_nif_crc_init_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 1) {
        return enif_make_badarg(env);
    }

    if (enif_is_atom(env, argv[0])) {
        return crc_nif_crc_init_1_atom(env, argc, argv);
    } else if (enif_is_map(env, argv[0])) {
        return crc_nif_crc_init_1_map(env, argc, argv);
    } else if (enif_is_tuple(env, argv[0])) {
        return crc_nif_crc_init_1_tuple(env, argc, argv);
    } else if (enif_is_ref(env, argv[0])) {
        return crc_nif_crc_init_1_ref(env, argc, argv);
    } else {
        return enif_make_badarg(env);
    }
}

static ERL_NIF_TERM
crc_nif_crc_list_0(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 0) {
        return enif_make_badarg(env);
    }
    return crc_model_list(env);
}

static ErlNifFunc crc_nif_funcs[] = {{"crc_info", 1, crc_nif_crc_info_1},
                                     {"crc_init", 1, crc_nif_crc_init_1},
                                     {"crc_list", 0, crc_nif_crc_list_0},
                                     {"checksum_xor", 1, crc_nif_checksum_xor_1},
                                     {"crc_8", 2, crc_nif_crc_8_2},
                                     {"crc_8_init", 1, crc_nif_crc_8_init_1},
                                     {"crc_8_update", 2, crc_nif_crc_8_update_2},
                                     {"crc_8_final", 1, crc_nif_crc_8_final_1},
                                     {"crc_16", 1, crc_nif_crc_16_1},
                                     {"crc_16_init", 0, crc_nif_crc_16_init_0},
                                     {"crc_16_update", 2, crc_nif_crc_16_update_2},
                                     {"crc_16_final", 1, crc_nif_crc_16_final_1},
                                     {"crc_16_ccitt", 2, crc_nif_crc_16_ccitt_2},
                                     {"crc_16_ccitt_init", 1, crc_nif_crc_16_ccitt_init_1},
                                     {"crc_16_ccitt_update", 2, crc_nif_crc_16_ccitt_update_2},
                                     {"crc_16_ccitt_final", 1, crc_nif_crc_16_ccitt_final_1},
                                     {"crc_16_dnp", 1, crc_nif_crc_16_dnp_1},
                                     {"crc_16_dnp_init", 0, crc_nif_crc_16_dnp_init_0},
                                     {"crc_16_dnp_update", 2, crc_nif_crc_16_dnp_update_2},
                                     {"crc_16_dnp_final", 1, crc_nif_crc_16_dnp_final_1},
                                     {"crc_16_kermit", 2, crc_nif_crc_16_kermit_2},
                                     {"crc_16_kermit_init", 1, crc_nif_crc_16_kermit_init_1},
                                     {"crc_16_kermit_update", 2, crc_nif_crc_16_kermit_update_2},
                                     {"crc_16_kermit_final", 1, crc_nif_crc_16_kermit_final_1},
                                     {"crc_16_modbus", 1, crc_nif_crc_16_modbus_1},
                                     {"crc_16_modbus_init", 0, crc_nif_crc_16_modbus_init_0},
                                     {"crc_16_modbus_update", 2, crc_nif_crc_16_modbus_update_2},
                                     {"crc_16_modbus_final", 1, crc_nif_crc_16_modbus_final_1},
                                     {"crc_16_sick", 1, crc_nif_crc_16_sick_1},
                                     {"crc_16_sick_init", 0, crc_nif_crc_16_sick_init_0},
                                     {"crc_16_sick_update", 2, crc_nif_crc_16_sick_update_2},
                                     {"crc_16_sick_final", 1, crc_nif_crc_16_sick_final_1},
                                     {"crc_32", 1, crc_nif_crc_32_1},
                                     {"crc_32_init", 0, crc_nif_crc_32_init_0},
                                     {"crc_32_update", 2, crc_nif_crc_32_update_2},
                                     {"crc_32_final", 1, crc_nif_crc_32_final_1}};

static int crc_nif_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info);
static int crc_nif_upgrade(ErlNifEnv *env, void **priv_data, void **old_priv_data, ERL_NIF_TERM load_info);
static void crc_nif_unload(ErlNifEnv *env, void *priv_data);

static int
crc_nif_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info)
{
    /* Open resource type */
    if (crc_nif_resource_type == NULL) {
        crc_nif_resource_type = enif_open_resource_type(env, NULL, "crc", NULL, ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);
        if (crc_nif_resource_type == NULL) {
            return -1;
        }
    }

/* Initialize common atoms */
#define ATOM(Id, Value)                                                                                                            \
    {                                                                                                                              \
        Id = enif_make_atom(env, Value);                                                                                           \
    }
    ATOM(ATOM_check, "check");
    ATOM(ATOM_error, "error");
    ATOM(ATOM_false, "false");
    ATOM(ATOM_init, "init");
    ATOM(ATOM_key, "key");
    ATOM(ATOM_name, "name");
    ATOM(ATOM_nil, "nil");
    ATOM(ATOM_normal, "normal");
    ATOM(ATOM_ok, "ok");
    ATOM(ATOM_poly, "poly");
    ATOM(ATOM_refin, "refin");
    ATOM(ATOM_refout, "refout");
    ATOM(ATOM_residue, "residue");
    ATOM(ATOM_sick, "sick");
    ATOM(ATOM_true, "true");
    ATOM(ATOM_type, "type");
    ATOM(ATOM_width, "width");
    ATOM(ATOM_xorout, "xorout");
#undef ATOM

    (void)crc_model_load(env, priv_data, load_info);

    return xnif_slice_load(env, priv_data, load_info);
}

static int
crc_nif_upgrade(ErlNifEnv *env, void **priv_data, void **old_priv_data, ERL_NIF_TERM load_info)
{
    return xnif_slice_upgrade(env, priv_data, old_priv_data, load_info);
}

static void
crc_nif_unload(ErlNifEnv *env, void *priv_data)
{
    (void)xnif_slice_unload(env, priv_data);
    return;
}

ERL_NIF_INIT(crc_nif, crc_nif_funcs, crc_nif_load, NULL, crc_nif_upgrade, crc_nif_unload);
