// -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c++ et

#include "crc_nif.h"
#include "crc_model.h"
#include "crc_resource.h"
#include "checksum_xor.h"
#include "crc_8.h"
#include "crc_16.h"
#include "crc_32.h"
#include "xnif_slice.h"

static ERL_NIF_TERM ATOM_bits;
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
static ERL_NIF_TERM ATOM_root_key;
static ERL_NIF_TERM ATOM_sick;
static ERL_NIF_TERM ATOM_true;
static ERL_NIF_TERM ATOM_type;
static ERL_NIF_TERM ATOM_value;
static ERL_NIF_TERM ATOM_width;
static ERL_NIF_TERM ATOM_xorout;

/* NIF Function Declarations */

static ERL_NIF_TERM crc_nif_debug_table_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM crc_nif_crc_info_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM crc_nif_crc_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM crc_nif_crc_init_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM crc_nif_crc_update_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM crc_nif_crc_final_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

/* crc_nif:debug_table/1 */

static ERL_NIF_TERM
crc_nif_debug_table_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    const crc_resource_t *resource = NULL;

    if (argc != 1 || !crc_resource_get(env, argv[0], &resource)) {
        return enif_make_badarg(env);
    }

    crc_model_t *m = NULL;
    size_t mlen;

    switch (resource->model->bits) {
    case 8:
        mlen = sizeof(crc_model_uint8_t);
        break;
    case 16:
        mlen = sizeof(crc_model_uint16_t);
        break;
    case 32:
        mlen = sizeof(crc_model_uint32_t);
        break;
    case 64:
        mlen = sizeof(crc_model_uint64_t);
        break;
    default:
        return ATOM_false;
    }

    m = (void *)enif_alloc(mlen);
    if (m == NULL) {
        return enif_make_badarg(env);
    }
    (void)memcpy(m, resource->model, mlen);

    if (!crc_algorithm_compile(m)) {
        (void)enif_free((void *)m);
        return ATOM_false;
    }

    ERL_NIF_TERM table[256];
    size_t i;

#define CRC_DEBUG_TABLE_VALUES(type)                                                                                               \
    do {                                                                                                                           \
        crc_model_##type##_t *model = (void *)m;                                                                                   \
        for (i = 0; i < 256; i++) {                                                                                                \
            table[i] = (m->bits > 32) ? enif_make_uint64(env, (ErlNifUInt64)model->table[i])                                       \
                                      : enif_make_uint(env, (unsigned int)model->table[i]);                                        \
        }                                                                                                                          \
    } while (0)

    switch (m->bits) {
    case 8:
        CRC_DEBUG_TABLE_VALUES(uint8);
        break;
    case 16:
        CRC_DEBUG_TABLE_VALUES(uint16);
        break;
    case 32:
        CRC_DEBUG_TABLE_VALUES(uint32);
        break;
    case 64:
        CRC_DEBUG_TABLE_VALUES(uint64);
        break;
    default:
        (void)enif_free((void *)m);
        return ATOM_false;
    }

#undef CRC_DEBUG_TABLE_VALUES

    (void)enif_free((void *)m);

    ERL_NIF_TERM list = enif_make_list_from_array(env, table, 256);
    return enif_make_tuple2(env, ATOM_true, list);
}

/* crc_nif:crc_info/1 */

static ERL_NIF_TERM
crc_nif_crc_info_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    const crc_resource_t *resource = NULL;

    if (argc != 1 || !crc_resource_get(env, argv[0], &resource)) {
        return enif_make_badarg(env);
    }

    ERL_NIF_TERM key;
    ERL_NIF_TERM val;
    ERL_NIF_TERM map = enif_make_new_map(env);

    key = ATOM_bits;
    val = enif_make_uint(env, (unsigned int)resource->model->bits);
    (void)enif_make_map_put(env, map, key, val, &map);

    key = ATOM_root_key;
    val = resource->model->root_key;
    (void)enif_make_map_put(env, map, key, val, &map);

    key = ATOM_name;
    val = enif_make_new_map(env);
    {
        ERL_NIF_TERM skey;
        ERL_NIF_TERM sval;
        const crc_linklist_t *anchor = &resource->model->_link;
        const crc_linklist_t *node = anchor->next;
        const crc_model_stub_t *stub = NULL;
        unsigned char *sval_buf = NULL;
        size_t sval_len = 0;
        while (node != anchor) {
            stub = (void *)node;
            node = node->next;
            skey = stub->key;
            sval_len = strnlen(stub->name, sizeof(stub->name));
            if (sval_len == 0) {
                sval = ATOM_nil;
            } else {
                sval_buf = enif_make_new_binary(env, sval_len, &sval);
                (void)memcpy(sval_buf, stub->name, sval_len);
            }
            (void)enif_make_map_put(env, val, skey, sval, &val);
        }
    }
    (void)enif_make_map_put(env, map, key, val, &map);

#define CRC_BUILD_INFO_MAP(type)                                                                                                   \
    do {                                                                                                                           \
        const crc_resource_##type##_t *p = (void *)resource;                                                                       \
        key = ATOM_sick;                                                                                                           \
        val = (p->model->sick) ? ATOM_true : ATOM_false;                                                                           \
        (void)enif_make_map_put(env, map, key, val, &map);                                                                         \
                                                                                                                                   \
        key = ATOM_width;                                                                                                          \
        val = enif_make_uint(env, (unsigned int)p->model->width);                                                                  \
        (void)enif_make_map_put(env, map, key, val, &map);                                                                         \
                                                                                                                                   \
        key = ATOM_poly;                                                                                                           \
        val = (resource->model->bits > 32) ? enif_make_uint64(env, (ErlNifUInt64)p->model->poly)                                   \
                                           : enif_make_uint(env, (unsigned int)p->model->poly);                                    \
        (void)enif_make_map_put(env, map, key, val, &map);                                                                         \
                                                                                                                                   \
        key = ATOM_init;                                                                                                           \
        val = (resource->model->bits > 32) ? enif_make_uint64(env, (ErlNifUInt64)p->model->init)                                   \
                                           : enif_make_uint(env, (unsigned int)p->model->init);                                    \
        (void)enif_make_map_put(env, map, key, val, &map);                                                                         \
                                                                                                                                   \
        key = ATOM_refin;                                                                                                          \
        val = (p->model->refin) ? ATOM_true : ATOM_false;                                                                          \
        (void)enif_make_map_put(env, map, key, val, &map);                                                                         \
                                                                                                                                   \
        key = ATOM_refout;                                                                                                         \
        val = (p->model->refout) ? ATOM_true : ATOM_false;                                                                         \
        (void)enif_make_map_put(env, map, key, val, &map);                                                                         \
                                                                                                                                   \
        key = ATOM_xorout;                                                                                                         \
        val = (resource->model->bits > 32) ? enif_make_uint64(env, (ErlNifUInt64)p->model->xorout)                                 \
                                           : enif_make_uint(env, (unsigned int)p->model->xorout);                                  \
        (void)enif_make_map_put(env, map, key, val, &map);                                                                         \
                                                                                                                                   \
        key = ATOM_check;                                                                                                          \
        val = (resource->model->bits > 32) ? enif_make_uint64(env, (ErlNifUInt64)p->model->check)                                  \
                                           : enif_make_uint(env, (unsigned int)p->model->check);                                   \
        (void)enif_make_map_put(env, map, key, val, &map);                                                                         \
                                                                                                                                   \
        key = ATOM_residue;                                                                                                        \
        val = (resource->model->bits > 32) ? enif_make_uint64(env, (ErlNifUInt64)p->model->residue)                                \
                                           : enif_make_uint(env, (unsigned int)p->model->residue);                                 \
        (void)enif_make_map_put(env, map, key, val, &map);                                                                         \
                                                                                                                                   \
        key = ATOM_value;                                                                                                          \
        val = (resource->model->bits > 32) ? enif_make_uint64(env, (ErlNifUInt64)p->value)                                         \
                                           : enif_make_uint(env, (unsigned int)p->value);                                          \
        (void)enif_make_map_put(env, map, key, val, &map);                                                                         \
    } while (0)

    switch (resource->model->bits) {
    case 8:
        CRC_BUILD_INFO_MAP(uint8);
        break;
    case 16:
        CRC_BUILD_INFO_MAP(uint16);
        break;
    case 32:
        CRC_BUILD_INFO_MAP(uint32);
        break;
    case 64:
        CRC_BUILD_INFO_MAP(uint64);
        break;
    default:
        break;
    }

#undef CRC_INFO

    return map;
}

/* crc_nif:crc/2 */

static int crc_nif_crc_2_work(ErlNifEnv *env, xnif_slice_t *slice, int *phasep, size_t *offsetp, size_t reductions);
static ERL_NIF_TERM crc_nif_crc_2_done(ErlNifEnv *env, xnif_slice_t *slice);

static xnif_slice_func_t crc_nif_crc_2_func = {
    crc_nif_crc_2_work, crc_nif_crc_2_done, NULL, NULL,
};

static ERL_NIF_TERM
crc_nif_crc_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    crc_resource_t *resource = NULL;
    ERL_NIF_TERM res_term;
    ErlNifBinary input;
    if (argc != 2 || !enif_inspect_iolist_as_binary(env, argv[1], &input)) {
        return enif_make_badarg(env);
    }
    res_term = crc_nif_crc_init_1(env, 1, argv);
    if (!crc_resource_get(env, res_term, (const crc_resource_t **)&resource)) {
        return res_term;
    }

    ERL_NIF_TERM out_term;

    if (input.size <= XNIF_SLICE_MAX_PER_SLICE) {
        if (!crc_resource_update_unsafe(resource, (const uint8_t *)input.data, (size_t)input.size)) {
            return enif_make_badarg(env);
        }
        ErlNifUInt64 value = 0;
        if (!crc_resource_final(resource, (void *)&value)) {
            return enif_make_badarg(env);
        }
        out_term = enif_make_uint64(env, value);
        return out_term;
    }

    xnif_slice_t *slice = xnif_slice_create("crc", &crc_nif_crc_2_func, 0, input.size);
    if (slice == NULL) {
        return enif_make_badarg(env);
    }
    ERL_NIF_TERM newargv[2];
    newargv[0] = res_term;
    newargv[1] = argv[1];
    return xnif_slice_schedule(env, slice, 2, newargv);
}

static int
crc_nif_crc_2_work(ErlNifEnv *env, xnif_slice_t *slice, int *phasep, size_t *offsetp, size_t reductions)
{
    size_t offset = *offsetp;
    crc_resource_t *resource = NULL;
    ErlNifBinary input;
    if (slice->argc != 2 || !crc_resource_get(env, slice->argv[0], (const crc_resource_t **)&resource) ||
        !enif_inspect_iolist_as_binary(env, slice->argv[1], &input)) {
        return -1;
    }
    if (!crc_resource_update_unsafe(resource, (const uint8_t *)input.data + offset, reductions)) {
        return -1;
    }
    *offsetp = offset + reductions;
    return 0;
}

static ERL_NIF_TERM
crc_nif_crc_2_done(ErlNifEnv *env, xnif_slice_t *slice)
{
    const crc_resource_t *resource = NULL;
    ERL_NIF_TERM out_term;
    ErlNifUInt64 value = 0;
    if (slice->argc != 2 || !crc_resource_get(env, slice->argv[0], &resource) || !crc_resource_final(resource, (void *)&value)) {
        return enif_make_badarg(env);
    }
    out_term = enif_make_uint64(env, value);
    return out_term;
}

/* crc_nif:crc_init/1 */

static ERL_NIF_TERM crc_nif_crc_init_1_atom(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM crc_nif_crc_init_1_map(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM crc_nif_crc_init_1_ref(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM crc_nif_crc_init_1_tuple(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

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
crc_nif_crc_init_1_atom(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    const crc_model_t *model = NULL;

    if (!crc_model_get(argv[0], &model)) {
        return enif_make_badarg(env);
    }

    crc_resource_t *resource = crc_resource_create(NULL, model);
    if (resource == NULL) {
        return enif_make_badarg(env);
    }

    ERL_NIF_TERM out_term;
    out_term = enif_make_resource(env, (void *)resource);
    (void)enif_release_resource((void *)resource);

    return out_term;
}

static ERL_NIF_TERM
crc_nif_crc_init_1_map(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    size_t arity;

    if (argc != 1 || !enif_get_map_size(env, argv[0], &arity) || arity < 8) {
        return enif_make_badarg(env);
    }

    ERL_NIF_TERM sick;
    ERL_NIF_TERM width;
    ERL_NIF_TERM poly;
    ERL_NIF_TERM init;
    ERL_NIF_TERM refin;
    ERL_NIF_TERM refout;
    ERL_NIF_TERM xorout;
    ERL_NIF_TERM check;
    ERL_NIF_TERM residue;

    if (!enif_get_map_value(env, argv[0], ATOM_sick, &sick) || !enif_get_map_value(env, argv[0], ATOM_width, &width) ||
        !enif_get_map_value(env, argv[0], ATOM_poly, &poly) || !enif_get_map_value(env, argv[0], ATOM_init, &init) ||
        !enif_get_map_value(env, argv[0], ATOM_refin, &refin) || !enif_get_map_value(env, argv[0], ATOM_refout, &refout) ||
        !enif_get_map_value(env, argv[0], ATOM_xorout, &xorout) || !enif_get_map_value(env, argv[0], ATOM_check, &check) ||
        !enif_get_map_value(env, argv[0], ATOM_residue, &residue)) {
        return enif_make_badarg(env);
    }

    ERL_NIF_TERM newargv[1];
    newargv[0] = enif_make_tuple9(env, sick, width, poly, init, refin, refout, xorout, check, residue);
    return crc_nif_crc_init_1_tuple(env, 1, newargv);
}

static ERL_NIF_TERM
crc_nif_crc_init_1_ref(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    const crc_resource_t *old_resource = NULL;

    if (argc != 1 || !crc_resource_get(env, argv[0], &old_resource)) {
        return enif_make_badarg(env);
    }

    crc_resource_t *new_resource = crc_resource_create(old_resource, old_resource->model);
    if (new_resource == NULL) {
        return enif_make_badarg(env);
    }

    ERL_NIF_TERM out_term;
    out_term = enif_make_resource(env, (void *)new_resource);
    (void)enif_release_resource((void *)new_resource);

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

    if (arity != 9 || !(tuple[0] == ATOM_false || tuple[0] == ATOM_true) || !enif_get_uint(env, tuple[1], &width) || width > 0x40 ||
        !enif_get_uint64(env, tuple[2], &poly) || !enif_get_uint64(env, tuple[3], &init) ||
        !(tuple[4] == ATOM_false || tuple[4] == ATOM_true) || !(tuple[5] == ATOM_false || tuple[5] == ATOM_true) ||
        !enif_get_uint64(env, tuple[6], &xorout) || !enif_get_uint64(env, tuple[7], &check) ||
        !enif_get_uint64(env, tuple[8], &residue)) {
        return enif_make_badarg(env);
    }

    ErlNifUInt64 msb_mask = 1;
    msb_mask <<= (width - 1);
    ErlNifUInt64 mask = 1;
    mask |= ((msb_mask - 1) << 1);

    if (poly > mask || init > mask || xorout > mask || check > mask || residue > mask) {
        return enif_make_badarg(env);
    }

    const crc_model_t *model = NULL;
    crc_resource_t *resource = NULL;
    uint8_t bits = ((width / 8) + (((width % 8) | ((~(width % 8) + 1) >> 7)) & 1)) * 8;

#define CRC_INIT_MODEL_AND_RESOURCE(type)                                                                                          \
    do {                                                                                                                           \
        crc_model_##type##_t m_buff;                                                                                               \
        crc_model_##type##_t *m = &m_buff;                                                                                         \
        m->super._link.prev = m->super._link.next = NULL;                                                                          \
        (void)crc_linklist_init_anchor(&m->super._link);                                                                           \
        m->super.is_root = true;                                                                                                   \
        m->super.root_key = ATOM_nil;                                                                                              \
        m->super.root_keystring[0] = '\0';                                                                                         \
        m->super.bits = bits;                                                                                                      \
        m->sick = (tuple[0] == ATOM_true) ? true : false;                                                                          \
        m->width = (uint8_t)width;                                                                                                 \
        m->poly = (type##_t)poly;                                                                                                  \
        m->init = (type##_t)init;                                                                                                  \
        m->refin = (tuple[4] == ATOM_true) ? true : false;                                                                         \
        m->refout = (tuple[5] == ATOM_true) ? true : false;                                                                        \
        m->xorout = (type##_t)xorout;                                                                                              \
        m->check = (type##_t)check;                                                                                                \
        m->residue = (type##_t)residue;                                                                                            \
        if (crc_model_match(&m->super, &model)) {                                                                                  \
            resource = crc_resource_create(NULL, model);                                                                           \
        } else {                                                                                                                   \
            resource = crc_resource_create(NULL, &m->super);                                                                       \
            model = (void *)resource->model;                                                                                       \
        }                                                                                                                          \
    } while (0)

    switch (bits) {
    case 8:
        CRC_INIT_MODEL_AND_RESOURCE(uint8);
        break;
    case 16:
        CRC_INIT_MODEL_AND_RESOURCE(uint16);
        break;
    case 32:
        CRC_INIT_MODEL_AND_RESOURCE(uint32);
        break;
    case 64:
        CRC_INIT_MODEL_AND_RESOURCE(uint64);
        break;
    default:
        return enif_make_badarg(env);
    }

#undef CRC_INIT_MODEL_AND_RESOURCE

    if (resource == NULL) {
        return enif_make_badarg(env);
    }

    if (model->root_key == ATOM_nil) {
        (void)crc_algorithm_compile((void *)model);
    }

    ERL_NIF_TERM out_term;
    out_term = enif_make_resource(env, (void *)resource);
    (void)enif_release_resource((void *)resource);

    return out_term;
}

/* crc_nif:crc_list/0 */

static ERL_NIF_TERM
crc_nif_crc_list_0(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 0) {
        return enif_make_badarg(env);
    }
    return crc_model_list(env);
}

/* crc_nif:crc_update/2 */

static int crc_nif_crc_update_2_work(ErlNifEnv *env, xnif_slice_t *slice, int *phasep, size_t *offsetp, size_t reductions);
static ERL_NIF_TERM crc_nif_crc_update_2_done(ErlNifEnv *env, xnif_slice_t *slice);

static xnif_slice_func_t crc_nif_crc_update_2_func = {
    crc_nif_crc_update_2_work, crc_nif_crc_update_2_done, NULL, NULL,
};

static ERL_NIF_TERM
crc_nif_crc_update_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    const crc_resource_t *old_resource = NULL;
    ErlNifBinary input;

    if (argc != 2 || !crc_resource_get(env, argv[0], &old_resource) || !enif_inspect_iolist_as_binary(env, argv[1], &input)) {
        return enif_make_badarg(env);
    }

    if (input.size == 0) {
        return argv[0];
    }

    crc_resource_t *new_resource = NULL;
    ERL_NIF_TERM out_term;

    if (input.size <= XNIF_SLICE_MAX_PER_SLICE) {
        if (!crc_resource_update(old_resource, (const uint8_t *)input.data, (size_t)input.size, &new_resource)) {
            return enif_make_badarg(env);
        }
        out_term = enif_make_resource(env, (void *)new_resource);
        (void)enif_release_resource((void *)new_resource);
        return out_term;
    }

    new_resource = crc_resource_create(old_resource, old_resource->model);
    if (new_resource == NULL) {
        return enif_make_badarg(env);
    }
    xnif_slice_t *slice = xnif_slice_create("crc_update", &crc_nif_crc_update_2_func, 0, input.size);
    if (slice == NULL) {
        (void)enif_release_resource((void *)new_resource);
        return enif_make_badarg(env);
    }
    ERL_NIF_TERM newargv[2];
    newargv[0] = enif_make_resource(env, (void *)new_resource);
    newargv[1] = argv[1];
    (void)enif_release_resource((void *)new_resource);
    return xnif_slice_schedule(env, slice, 2, newargv);
}

static int
crc_nif_crc_update_2_work(ErlNifEnv *env, xnif_slice_t *slice, int *phasep, size_t *offsetp, size_t reductions)
{
    size_t offset = *offsetp;
    crc_resource_t *resource = NULL;
    ErlNifBinary input;
    if (slice->argc != 2 || !crc_resource_get(env, slice->argv[0], (const crc_resource_t **)&resource) ||
        !enif_inspect_iolist_as_binary(env, slice->argv[1], &input)) {
        return -1;
    }
    if (!crc_resource_update_unsafe(resource, (const uint8_t *)input.data + offset, reductions)) {
        return -1;
    }
    *offsetp = offset + reductions;
    return 0;
}

static ERL_NIF_TERM
crc_nif_crc_update_2_done(ErlNifEnv *env, xnif_slice_t *slice)
{
    return slice->argv[0];
}

/* crc_nif:crc_final/1 */

static ERL_NIF_TERM
crc_nif_crc_final_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    const crc_resource_t *resource = NULL;

    if (argc != 1 || !crc_resource_get(env, argv[0], &resource)) {
        return enif_make_badarg(env);
    }

    ErlNifUInt64 value = 0;

    if (!crc_resource_final(resource, (void *)&value)) {
        return enif_make_badarg(env);
    }

    ERL_NIF_TERM out_term;
    out_term = enif_make_uint64(env, value);

    return out_term;
}

static ErlNifFunc crc_nif_funcs[] = {{"debug_table", 1, crc_nif_debug_table_1},
                                     {"crc", 2, crc_nif_crc_2},
                                     {"crc_info", 1, crc_nif_crc_info_1},
                                     {"crc_init", 1, crc_nif_crc_init_1},
                                     {"crc_list", 0, crc_nif_crc_list_0},
                                     {"crc_update", 2, crc_nif_crc_update_2},
                                     {"crc_final", 1, crc_nif_crc_final_1},
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

static void crc_nif_make_atoms(ErlNifEnv *env);
static int crc_nif_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info);
static int crc_nif_upgrade(ErlNifEnv *env, void **priv_data, void **old_priv_data, ERL_NIF_TERM load_info);
static void crc_nif_unload(ErlNifEnv *env, void *priv_data);

static void
crc_nif_make_atoms(ErlNifEnv *env)
{
#define ATOM(Id, Value)                                                                                                            \
    {                                                                                                                              \
        Id = enif_make_atom(env, Value);                                                                                           \
    }
    ATOM(ATOM_bits, "bits");
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
    ATOM(ATOM_root_key, "root_key");
    ATOM(ATOM_sick, "sick");
    ATOM(ATOM_true, "true");
    ATOM(ATOM_type, "type");
    ATOM(ATOM_value, "value");
    ATOM(ATOM_width, "width");
    ATOM(ATOM_xorout, "xorout");
#undef ATOM
}

static int
crc_nif_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info)
{
    int retval;

    /* Load CRC model, CRC resource, and XNIF slice */
    if ((retval = crc_model_load(env, priv_data, load_info)) != 0) {
        return retval;
    }
    if ((retval = crc_resource_load(env, priv_data, load_info)) != 0) {
        (void)crc_model_unload(env, priv_data);
        return retval;
    }
    if ((retval = xnif_slice_load(env, priv_data, load_info)) != 0) {
        (void)crc_resource_unload(env, priv_data);
        (void)crc_model_unload(env, priv_data);
        return retval;
    }

    /* Initialize common atoms */
    (void)crc_nif_make_atoms(env);

    return retval;
}

static int
crc_nif_upgrade(ErlNifEnv *env, void **priv_data, void **old_priv_data, ERL_NIF_TERM load_info)
{
    int retval;

    /* Upgrade CRC model, CRC resource, and XNIF slice */
    if ((retval = crc_model_upgrade(env, priv_data, old_priv_data, load_info)) != 0) {
        return retval;
    }
    if ((retval = crc_resource_upgrade(env, priv_data, old_priv_data, load_info)) != 0) {
        return retval;
    }
    if ((retval = xnif_slice_upgrade(env, priv_data, old_priv_data, load_info)) != 0) {
        return retval;
    }

    /* Initialize common atoms */
    (void)crc_nif_make_atoms(env);

    return retval;
}

static void
crc_nif_unload(ErlNifEnv *env, void *priv_data)
{
    (void)xnif_slice_unload(env, priv_data);
    (void)crc_resource_unload(env, priv_data);
    (void)crc_model_unload(env, priv_data);
    return;
}

ERL_NIF_INIT(crc_nif, crc_nif_funcs, crc_nif_load, NULL, crc_nif_upgrade, crc_nif_unload);
