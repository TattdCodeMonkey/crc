// -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c++ et

#ifndef CRC_32_H
#define CRC_32_H

#include <stdlib.h>
#include <stdint.h>

#include "crc_nif.h"

#ifdef __cplusplus
extern "C" {
#endif

extern uint32_t crc_32_init(void);
extern uint32_t crc_32_update(uint32_t ctx, const unsigned char *buf, size_t len);
extern uint32_t crc_32_final(uint32_t ctx);
static uint32_t crc_32(const unsigned char *buf, size_t len);

inline uint32_t
crc_32(const unsigned char *buf, size_t len)
{
    uint32_t ctx;
    ctx = crc_32_init();
    ctx = crc_32_update(ctx, buf, len);
    return crc_32_final(ctx);
}

/* NIF Functions */

extern ERL_NIF_TERM crc_nif_crc_32_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM crc_nif_crc_32_init_0(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM crc_nif_crc_32_update_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM crc_nif_crc_32_final_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

#ifdef __cplusplus
}
#endif

#endif
