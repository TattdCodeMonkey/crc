// -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c++ et

#include "crc_nif.h"
#include "crc_8.h"
#include "crc_16.h"
#include "xnif_slice.h"

// extern ERL_NIF_TERM _calc_8(ErlNifEnv *env, int arc, const ERL_NIF_TERM argv[]);
// extern ERL_NIF_TERM _calc_16(ErlNifEnv *env, int arc, const ERL_NIF_TERM argv[]);
// extern ERL_NIF_TERM _calc_16_ccitt(ErlNifEnv *env, int arc, const ERL_NIF_TERM argv[]);
// extern ERL_NIF_TERM _calc_16_kermit(ErlNifEnv *env, int arc, const ERL_NIF_TERM argv[]);
// extern ERL_NIF_TERM _calc_16_modbus(ErlNifEnv *env, int arc, const ERL_NIF_TERM argv[]);

// extern int erts_fprintf(FILE *, const char *, ...);

static ErlNifFunc crc_nif_funcs[] = {{"crc_8", 2, crc_nif_crc_8_2},
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
                                     {"crc_16_kermit", 2, crc_nif_crc_16_kermit_2},
                                     {"crc_16_kermit_init", 1, crc_nif_crc_16_kermit_init_1},
                                     {"crc_16_kermit_update", 2, crc_nif_crc_16_kermit_update_2},
                                     {"crc_16_kermit_final", 1, crc_nif_crc_16_kermit_final_1},
                                     {"crc_16_modbus", 1, crc_nif_crc_16_modbus_1},
                                     {"crc_16_modbus_init", 0, crc_nif_crc_16_modbus_init_0},
                                     {"crc_16_modbus_update", 2, crc_nif_crc_16_modbus_update_2},
                                     {"crc_16_modbus_final", 1, crc_nif_crc_16_modbus_final_1}};

// static ErlNifFunc nif_funcs[] = {{"_calc_8", 2, _calc_8},
//                                  {"_calc_16", 1, _calc_16},
//                                  {"_calc_16_ccitt", 2, _calc_16_ccitt},
//                                  {"_calc_16_kermit", 2, _calc_16_kermit},
//                                  {"_calc_16_modbus", 1, _calc_16_modbus}};

static int crc_nif_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info);
static int crc_nif_upgrade(ErlNifEnv *env, void **priv_data, void **old_priv_data, ERL_NIF_TERM load_info);
static void crc_nif_unload(ErlNifEnv *env, void *priv_data);

static int
crc_nif_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info)
{
    return xnif_slice_load(env, priv_data, load_info);
    // crc_nif_priv_data_t *p = (void *)enif_alloc(sizeof(crc_nif_priv_data_t));
    // if (p == NULL) {
    // 	return ENOMEM;
    // }
    // p->crc_8 = enif_open_resource_type(env, NULL, "crc_8", NULL, ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);
    // p->crc_16 = enif_open_resource_type(env, NULL, "crc_16", NULL, ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);
    // p->crc_16_ccitt = enif_open_resource_type(env, NULL, "crc_16_ccitt", NULL, ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);
    // p->crc_16_kermit = enif_open_resource_type(env, NULL, "crc_16_kermit", NULL, ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);
    // p->crc_16_modbus = enif_open_resource_type(env, NULL, "crc_16_modbus", NULL, ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);
    // *priv_data = (void *)p;
    // return 0;
}

static int
crc_nif_upgrade(ErlNifEnv *env, void **priv_data, void **old_priv_data, ERL_NIF_TERM load_info)
{
    return xnif_slice_upgrade(env, priv_data, old_priv_data, load_info);
    // int retval = crc_nif_load(env, priv_data, load_info);
    // crc_nif_priv_data_t *p = (void *)*old_priv_data;
    // if (p != NULL) {
    // 	(void)crc_nif_unload(env, (void *)p);
    // }
    // return retval;
}

static void
crc_nif_unload(ErlNifEnv *env, void *priv_data)
{
    (void)xnif_slice_unload(env, priv_data);
    return;
    // crc_nif_priv_data_t *p = (void *)priv_data;
    // if (p != NULL) {
    // 	(void)enif_free((void *)p);
    // }
    //    return;
}

ERL_NIF_INIT(crc_nif, crc_nif_funcs, crc_nif_load, NULL, crc_nif_upgrade, crc_nif_unload);

// static int
// load(ErlNifEnv *env, void **priv, ERL_NIF_TERM info)
// {
//     return 0;
// }

// static int
// reload(ErlNifEnv *env, void **priv, ERL_NIF_TERM info)
// {
//     return 0;
// }

// static int
// upgrade(ErlNifEnv *env, void **priv, void **old_priv, ERL_NIF_TERM info)
// {
//     return load(env, priv, info);
// }

// static void
// unload(ErlNifEnv *env, void *priv)
// {
//     enif_free(priv);
// }

// ERL_NIF_INIT(Elixir.CRC, nif_funcs, &load, &reload, &upgrade, &unload)
