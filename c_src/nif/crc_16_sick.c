// -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c++ et

#include "crc_16.h"
#include "xnif_slice.h"

uint16_t
crc_16_sick_init(void)
{
    return 0x0000;
}

uint16_t
crc_16_sick_update(uint16_t ctx, const unsigned char *buf, size_t len)
{
    uint16_t result = ctx;
    size_t size = len;
    uint16_t c;
    uint16_t p = 0;

    while (size--) {
        c = 0x00FF & (uint16_t)*buf++;
        if (result & 0x8000) {
            result = (result << 1) ^ 0x8005;
        } else {
            result = (result << 1);
        }
        result ^= (c | p);
        p = c << 8;
    }

    return result;
}

uint16_t
crc_16_sick_final(uint16_t ctx)
{
    uint16_t low_byte;
    uint16_t high_byte;
    low_byte = (ctx & 0xFF00) >> 8;
    high_byte = (ctx & 0x00FF) << 8;
    ctx = low_byte | high_byte;
    return ctx;
}

/* NIF Functions */

static int crc_nif_crc_16_sick_1_work(ErlNifEnv *env, xnif_slice_t *slice, int *phasep, size_t *offsetp, size_t reductions);
static ERL_NIF_TERM crc_nif_crc_16_sick_1_done(ErlNifEnv *env, xnif_slice_t *slice);

static xnif_slice_func_t crc_nif_crc_16_sick_1_func = {
    crc_nif_crc_16_sick_1_work, crc_nif_crc_16_sick_1_done, NULL, NULL,
};

ERL_NIF_TERM
crc_nif_crc_16_sick_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary input;

    if (argc != 1 || !enif_inspect_binary(env, argv[0], &input)) {
        return enif_make_badarg(env);
    }

    if (input.size <= XNIF_SLICE_MAX_PER_SLICE) {
        uint16_t crc;
        crc = crc_16_sick(input.data, input.size);
        return enif_make_uint(env, crc);
    }

    xnif_slice_t *slice = xnif_slice_create("crc_16_sick", &crc_nif_crc_16_sick_1_func, 0, input.size);
    if (slice == NULL) {
        return enif_make_badarg(env);
    }
    uint16_t ctx = crc_16_sick_init();
    ERL_NIF_TERM newargv[2];
    newargv[0] = enif_make_uint(env, ctx);
    newargv[1] = argv[0];
    return xnif_slice_schedule(env, slice, 2, newargv);
}

static int
crc_nif_crc_16_sick_1_work(ErlNifEnv *env, xnif_slice_t *slice, int *phasep, size_t *offsetp, size_t reductions)
{
    size_t offset = *offsetp;
    uint16_t ctx;
    ErlNifBinary input;
    if (slice->argc != 2 || !enif_get_uint(env, slice->argv[0], (unsigned int *)&ctx) ||
        !enif_inspect_binary(env, slice->argv[1], &input)) {
        return -1;
    }
    ctx = crc_16_sick_update(ctx, input.data + offset, reductions);
    slice->argv[0] = enif_make_uint(env, ctx);
    *offsetp = offset + reductions;
    return 0;
}

static ERL_NIF_TERM
crc_nif_crc_16_sick_1_done(ErlNifEnv *env, xnif_slice_t *slice)
{
    uint16_t ctx;
    if (slice->argc != 2 || !enif_get_uint(env, slice->argv[0], (unsigned int *)&ctx)) {
        return enif_make_badarg(env);
    }
    return enif_make_uint(env, crc_16_sick_final(ctx));
}

ERL_NIF_TERM
crc_nif_crc_16_sick_init_0(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{

    if (argc != 0) {
        return enif_make_badarg(env);
    }

    uint16_t ctx = crc_16_sick_init();
    return enif_make_uint(env, ctx);
}

static int crc_nif_crc_16_sick_update_2_work(ErlNifEnv *env, xnif_slice_t *slice, int *phasep, size_t *offsetp, size_t reductions);
static ERL_NIF_TERM crc_nif_crc_16_sick_update_2_done(ErlNifEnv *env, xnif_slice_t *slice);

static xnif_slice_func_t crc_nif_crc_16_sick_update_2_func = {
    crc_nif_crc_16_sick_update_2_work, crc_nif_crc_16_sick_update_2_done, NULL, NULL,
};

ERL_NIF_TERM
crc_nif_crc_16_sick_update_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    unsigned int ctx;
    ErlNifBinary input;

    if (argc != 2 || !enif_get_uint(env, argv[0], &ctx) || ctx > 0xffff || !enif_inspect_binary(env, argv[1], &input)) {
        return enif_make_badarg(env);
    }

    if (input.size <= XNIF_SLICE_MAX_PER_SLICE) {
        ctx = crc_16_sick_update((uint16_t)ctx, input.data, input.size);
        return enif_make_uint(env, ctx);
    }

    xnif_slice_t *slice = xnif_slice_create("crc_16_sick_update", &crc_nif_crc_16_sick_update_2_func, 0, input.size);
    if (slice == NULL) {
        return enif_make_badarg(env);
    }
    ERL_NIF_TERM newargv[2];
    newargv[0] = argv[0];
    newargv[1] = argv[1];
    return xnif_slice_schedule(env, slice, 2, newargv);
}

static int
crc_nif_crc_16_sick_update_2_work(ErlNifEnv *env, xnif_slice_t *slice, int *phasep, size_t *offsetp, size_t reductions)
{
    size_t offset = *offsetp;
    uint16_t ctx;
    ErlNifBinary input;
    if (slice->argc != 2 || !enif_get_uint(env, slice->argv[0], (unsigned int *)&ctx) ||
        !enif_inspect_binary(env, slice->argv[1], &input)) {
        return -1;
    }
    ctx = crc_16_sick_update(ctx, input.data + offset, reductions);
    slice->argv[0] = enif_make_uint(env, ctx);
    *offsetp = offset + reductions;
    return 0;
}

static ERL_NIF_TERM
crc_nif_crc_16_sick_update_2_done(ErlNifEnv *env, xnif_slice_t *slice)
{
    uint16_t ctx;
    if (slice->argc != 2 || !enif_get_uint(env, slice->argv[0], (unsigned int *)&ctx)) {
        return enif_make_badarg(env);
    }
    return enif_make_uint(env, ctx);
}

ERL_NIF_TERM
crc_nif_crc_16_sick_final_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    unsigned int ctx;

    if (argc != 1 || !enif_get_uint(env, argv[0], &ctx) || ctx > 0xffff) {
        return enif_make_badarg(env);
    }

    uint16_t crc = crc_16_sick_final((uint16_t)ctx);
    return enif_make_uint(env, crc);
}
