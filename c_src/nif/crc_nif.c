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
static ERL_NIF_TERM ATOM_slow;
static ERL_NIF_TERM ATOM_true;
static ERL_NIF_TERM ATOM_type;
static ERL_NIF_TERM ATOM_value;
static ERL_NIF_TERM ATOM_width;
static ERL_NIF_TERM ATOM_xorout;

/* NIF Function Declarations */

static ERL_NIF_TERM crc_nif_debug_table_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM crc_nif_crc_info_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM crc_nif_crc_list_0(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM crc_nif_crc_residue_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

/* NIF Function Definitions */

#include "crc_nif_fast.c.h"
#include "crc_nif_slow.c.h"

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

    key = ATOM_slow;
    val = (resource->slow) ? ATOM_true : ATOM_false;
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

/* crc_nif:crc_list/0 */

static ERL_NIF_TERM
crc_nif_crc_list_0(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 0) {
        return enif_make_badarg(env);
    }
    return crc_model_list(env);
}

/* crc_nif:crc_residue/1 */

static ERL_NIF_TERM
crc_nif_crc_residue_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    const crc_resource_t *resource = NULL;
    ERL_NIF_TERM res_term;
    if (argc != 1) {
        return enif_make_badarg(env);
    }
    res_term = crc_nif_crc_fast_init_1(env, 1, argv);
    if (!crc_resource_get(env, res_term, &resource)) {
        return res_term;
    }
    ErlNifUInt64 value = 0;
    if (!crc_algorithm_residue(resource->model, &value)) {
        return enif_make_badarg(env);
    }
    ERL_NIF_TERM out_term;
    out_term = enif_make_uint64(env, value);
    return out_term;
}

static ErlNifFunc crc_nif_funcs[] = {{"debug_table", 1, crc_nif_debug_table_1},

                                     {"crc", 2, crc_nif_crc_fast_2},
                                     {"crc_init", 1, crc_nif_crc_fast_init_1},
                                     {"crc_update", 2, crc_nif_crc_fast_update_2},
                                     {"crc_final", 1, crc_nif_crc_fast_final_1},
                                     {"crc_fast", 2, crc_nif_crc_fast_2},
                                     {"crc_fast_init", 1, crc_nif_crc_fast_init_1},
                                     {"crc_fast_update", 2, crc_nif_crc_fast_update_2},
                                     {"crc_fast_final", 1, crc_nif_crc_fast_final_1},
                                     {"crc_slow", 2, crc_nif_crc_slow_2},
                                     {"crc_slow_init", 1, crc_nif_crc_slow_init_1},
                                     {"crc_slow_update", 2, crc_nif_crc_slow_update_2},
                                     {"crc_slow_final", 1, crc_nif_crc_slow_final_1},

                                     {"crc_info", 1, crc_nif_crc_info_1},
                                     {"crc_list", 0, crc_nif_crc_list_0},
                                     {"crc_residue", 1, crc_nif_crc_residue_1},
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
    ATOM(ATOM_slow, "slow");
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
