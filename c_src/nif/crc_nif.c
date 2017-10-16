// -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c++ et

#include "crc_nif.h"
#include "checksum_xor.h"
#include "crc_8.h"
#include "crc_16.h"
#include "xnif_slice.h"

static ErlNifFunc crc_nif_funcs[] = {{"checksum_xor", 1, crc_nif_checksum_xor_1},
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
                                     {"crc_16_kermit", 2, crc_nif_crc_16_kermit_2},
                                     {"crc_16_kermit_init", 1, crc_nif_crc_16_kermit_init_1},
                                     {"crc_16_kermit_update", 2, crc_nif_crc_16_kermit_update_2},
                                     {"crc_16_kermit_final", 1, crc_nif_crc_16_kermit_final_1},
                                     {"crc_16_modbus", 1, crc_nif_crc_16_modbus_1},
                                     {"crc_16_modbus_init", 0, crc_nif_crc_16_modbus_init_0},
                                     {"crc_16_modbus_update", 2, crc_nif_crc_16_modbus_update_2},
                                     {"crc_16_modbus_final", 1, crc_nif_crc_16_modbus_final_1}};

static int crc_nif_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info);
static int crc_nif_upgrade(ErlNifEnv *env, void **priv_data, void **old_priv_data, ERL_NIF_TERM load_info);
static void crc_nif_unload(ErlNifEnv *env, void *priv_data);

static int
crc_nif_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info)
{
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
