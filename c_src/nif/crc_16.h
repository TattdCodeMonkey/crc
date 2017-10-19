// -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c++ et

#ifndef CRC_16_H
#define CRC_16_H

#include <stdlib.h>
#include <stdint.h>

#include "crc_nif.h"

#ifdef __cplusplus
extern "C" {
#endif

extern uint16_t crc_16_init(void);
extern uint16_t crc_16_update(uint16_t ctx, const unsigned char *buf, size_t len);
extern uint16_t crc_16_final(uint16_t ctx);
static uint16_t crc_16(const unsigned char *buf, size_t len);

inline uint16_t
crc_16(const unsigned char *buf, size_t len)
{
    uint16_t ctx;
    ctx = crc_16_init();
    ctx = crc_16_update(ctx, buf, len);
    return crc_16_final(ctx);
}

extern uint16_t crc_16_ccitt_init(uint16_t seed);
extern uint16_t crc_16_ccitt_update(uint16_t ctx, const unsigned char *buf, size_t len);
extern uint16_t crc_16_ccitt_final(uint16_t ctx);
static uint16_t crc_16_ccitt(uint16_t seed, const unsigned char *buf, size_t len);

inline uint16_t
crc_16_ccitt(uint16_t seed, const unsigned char *buf, size_t len)
{
    uint16_t ctx;
    ctx = crc_16_ccitt_init(seed);
    ctx = crc_16_ccitt_update(ctx, buf, len);
    return crc_16_ccitt_final(ctx);
}

extern uint16_t crc_16_dnp_init(void);
extern uint16_t crc_16_dnp_update(uint16_t ctx, const unsigned char *buf, size_t len);
extern uint16_t crc_16_dnp_final(uint16_t ctx);
static uint16_t crc_16_dnp(const unsigned char *buf, size_t len);

inline uint16_t
crc_16_dnp(const unsigned char *buf, size_t len)
{
    uint16_t ctx;
    ctx = crc_16_dnp_init();
    ctx = crc_16_dnp_update(ctx, buf, len);
    return crc_16_dnp_final(ctx);
}

extern uint16_t crc_16_kermit_init(uint16_t seed);
extern uint16_t crc_16_kermit_update(uint16_t ctx, const unsigned char *buf, size_t len);
extern uint16_t crc_16_kermit_final(uint16_t ctx);
static uint16_t crc_16_kermit(uint16_t seed, const unsigned char *buf, size_t len);

inline uint16_t
crc_16_kermit(uint16_t seed, const unsigned char *buf, size_t len)
{
    uint16_t ctx;
    ctx = crc_16_kermit_init(seed);
    ctx = crc_16_kermit_update(ctx, buf, len);
    return crc_16_kermit_final(ctx);
}

extern uint16_t crc_16_modbus_init(void);
extern uint16_t crc_16_modbus_update(uint16_t ctx, const unsigned char *buf, size_t len);
extern uint16_t crc_16_modbus_final(uint16_t ctx);
static uint16_t crc_16_modbus(const unsigned char *buf, size_t len);

inline uint16_t
crc_16_modbus(const unsigned char *buf, size_t len)
{
    uint16_t ctx;
    ctx = crc_16_modbus_init();
    ctx = crc_16_modbus_update(ctx, buf, len);
    return crc_16_modbus_final(ctx);
}

extern uint16_t crc_16_sick_init(void);
extern uint16_t crc_16_sick_update(uint16_t ctx, const unsigned char *buf, size_t len);
extern uint16_t crc_16_sick_final(uint16_t ctx);
static uint16_t crc_16_sick(const unsigned char *buf, size_t len);

inline uint16_t
crc_16_sick(const unsigned char *buf, size_t len)
{
    uint16_t ctx;
    ctx = crc_16_sick_init();
    ctx = crc_16_sick_update(ctx, buf, len);
    return crc_16_sick_final(ctx);
}

/* NIF Functions */

extern ERL_NIF_TERM crc_nif_crc_16_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM crc_nif_crc_16_init_0(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM crc_nif_crc_16_update_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM crc_nif_crc_16_final_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

extern ERL_NIF_TERM crc_nif_crc_16_ccitt_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM crc_nif_crc_16_ccitt_init_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM crc_nif_crc_16_ccitt_update_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM crc_nif_crc_16_ccitt_final_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

extern ERL_NIF_TERM crc_nif_crc_16_dnp_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM crc_nif_crc_16_dnp_init_0(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM crc_nif_crc_16_dnp_update_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM crc_nif_crc_16_dnp_final_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

extern ERL_NIF_TERM crc_nif_crc_16_kermit_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM crc_nif_crc_16_kermit_init_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM crc_nif_crc_16_kermit_update_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM crc_nif_crc_16_kermit_final_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

extern ERL_NIF_TERM crc_nif_crc_16_modbus_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM crc_nif_crc_16_modbus_init_0(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM crc_nif_crc_16_modbus_update_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM crc_nif_crc_16_modbus_final_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

extern ERL_NIF_TERM crc_nif_crc_16_sick_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM crc_nif_crc_16_sick_init_0(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM crc_nif_crc_16_sick_update_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM crc_nif_crc_16_sick_final_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

#ifdef __cplusplus
}
#endif

#endif
