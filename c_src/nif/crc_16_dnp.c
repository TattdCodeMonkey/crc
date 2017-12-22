// -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c++ et

#include "crc_16.h"
#include "xnif_slice.h"

static const uint16_t crc_16_dnp_table[256] = {
    0x0000, 0x3D65, 0x7ACA, 0x47AF, 0xF594, 0xC8F1, 0x8F5E, 0xB23B, 0xD64D, 0xEB28, 0xAC87, 0x91E2, 0x23D9, 0x1EBC, 0x5913, 0x6476,
0x91FF, 0xAC9A, 0xEB35, 0xD650, 0x646B, 0x590E, 0x1EA1, 0x23C4, 0x47B2, 0x7AD7, 0x3D78, 0x001D, 0xB226, 0x8F43, 0xC8EC, 0xF589,
0x1E9B, 0x23FE, 0x6451, 0x5934, 0xEB0F, 0xD66A, 0x91C5, 0xACA0, 0xC8D6, 0xF5B3, 0xB21C, 0x8F79, 0x3D42, 0x0027, 0x4788, 0x7AED,
0x8F64, 0xB201, 0xF5AE, 0xC8CB, 0x7AF0, 0x4795, 0x003A, 0x3D5F, 0x5929, 0x644C, 0x23E3, 0x1E86, 0xACBD, 0x91D8, 0xD677, 0xEB12,
0x3D36, 0x0053, 0x47FC, 0x7A99, 0xC8A2, 0xF5C7, 0xB268, 0x8F0D, 0xEB7B, 0xD61E, 0x91B1, 0xACD4, 0x1EEF, 0x238A, 0x6425, 0x5940,
0xACC9, 0x91AC, 0xD603, 0xEB66, 0x595D, 0x6438, 0x2397, 0x1EF2, 0x7A84, 0x47E1, 0x004E, 0x3D2B, 0x8F10, 0xB275, 0xF5DA, 0xC8BF,
0x23AD, 0x1EC8, 0x5967, 0x6402, 0xD639, 0xEB5C, 0xACF3, 0x9196, 0xF5E0, 0xC885, 0x8F2A, 0xB24F, 0x0074, 0x3D11, 0x7ABE, 0x47DB,
0xB252, 0x8F37, 0xC898, 0xF5FD, 0x47C6, 0x7AA3, 0x3D0C, 0x0069, 0x641F, 0x597A, 0x1ED5, 0x23B0, 0x918B, 0xACEE, 0xEB41, 0xD624,
0x7A6C, 0x4709, 0x00A6, 0x3DC3, 0x8FF8, 0xB29D, 0xF532, 0xC857, 0xAC21, 0x9144, 0xD6EB, 0xEB8E, 0x59B5, 0x64D0, 0x237F, 0x1E1A,
0xEB93, 0xD6F6, 0x9159, 0xAC3C, 0x1E07, 0x2362, 0x64CD, 0x59A8, 0x3DDE, 0x00BB, 0x4714, 0x7A71, 0xC84A, 0xF52F, 0xB280, 0x8FE5,
0x64F7, 0x5992, 0x1E3D, 0x2358, 0x9163, 0xAC06, 0xEBA9, 0xD6CC, 0xB2BA, 0x8FDF, 0xC870, 0xF515, 0x472E, 0x7A4B, 0x3DE4, 0x0081,
0xF508, 0xC86D, 0x8FC2, 0xB2A7, 0x009C, 0x3DF9, 0x7A56, 0x4733, 0x2345, 0x1E20, 0x598F, 0x64EA, 0xD6D1, 0xEBB4, 0xAC1B, 0x917E,
0x475A, 0x7A3F, 0x3D90, 0x00F5, 0xB2CE, 0x8FAB, 0xC804, 0xF561, 0x9117, 0xAC72, 0xEBDD, 0xD6B8, 0x6483, 0x59E6, 0x1E49, 0x232C,
0xD6A5, 0xEBC0, 0xAC6F, 0x910A, 0x2331, 0x1E54, 0x59FB, 0x649E, 0x00E8, 0x3D8D, 0x7A22, 0x4747, 0xF57C, 0xC819, 0x8FB6, 0xB2D3,
0x59C1, 0x64A4, 0x230B, 0x1E6E, 0xAC55, 0x9130, 0xD69F, 0xEBFA, 0x8F8C, 0xB2E9, 0xF546, 0xC823, 0x7A18, 0x477D, 0x00D2, 0x3DB7,
0xC83E, 0xF55B, 0xB2F4, 0x8F91, 0x3DAA, 0x00CF, 0x4760, 0x7A05, 0x1E73, 0x2316, 0x64B9, 0x59DC, 0xEBE7, 0xD682, 0x912D, 0xAC48};

uint16_t
crc_16_dnp_init(void)
{
    return 0x0000;
}

uint16_t
crc_16_dnp_update(uint16_t ctx, const unsigned char *buf, size_t len)
{
    while (len--) {
        ctx = (ctx >> 8) ^ crc_16_dnp_table[(ctx ^ (uint16_t)*buf++) & 0x00FF];
    }
    return ctx;
}

uint16_t
crc_16_dnp_final(uint16_t ctx)
{
    uint16_t low_byte;
    uint16_t high_byte;
    ctx = ~ctx;
    low_byte = (ctx & 0xFF00) >> 8;
    high_byte = (ctx & 0x00FF) << 8;
    ctx = low_byte | high_byte;
    return ctx;
}

/* NIF Functions */

static int crc_nif_crc_16_dnp_1_work(ErlNifEnv *env, xnif_slice_t *slice, int *phasep, size_t *offsetp, size_t reductions);
static ERL_NIF_TERM crc_nif_crc_16_dnp_1_done(ErlNifEnv *env, xnif_slice_t *slice);

static xnif_slice_func_t crc_nif_crc_16_dnp_1_func = {
    crc_nif_crc_16_dnp_1_work, crc_nif_crc_16_dnp_1_done, NULL, NULL,
};

ERL_NIF_TERM
crc_nif_crc_16_dnp_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary input;

    if (argc != 1 || !enif_inspect_binary(env, argv[0], &input)) {
        return enif_make_badarg(env);
    }

    if (input.size <= XNIF_SLICE_MAX_PER_SLICE) {
        uint16_t crc;
        crc = crc_16_dnp(input.data, input.size);
        return enif_make_uint(env, crc);
    }

    xnif_slice_t *slice = xnif_slice_create("crc_16_dnp", &crc_nif_crc_16_dnp_1_func, 0, input.size);
    if (slice == NULL) {
        return enif_make_badarg(env);
    }
    uint16_t ctx = crc_16_dnp_init();
    ERL_NIF_TERM newargv[2];
    newargv[0] = enif_make_uint(env, ctx);
    newargv[1] = argv[0];
    return xnif_slice_schedule(env, slice, 2, newargv);
}

static int
crc_nif_crc_16_dnp_1_work(ErlNifEnv *env, xnif_slice_t *slice, int *phasep, size_t *offsetp, size_t reductions)
{
    size_t offset = *offsetp;
    uint16_t ctx;
    ErlNifBinary input;
    if (slice->argc != 2 || !enif_get_uint(env, slice->argv[0], (unsigned int *)&ctx) ||
        !enif_inspect_binary(env, slice->argv[1], &input)) {
        return -1;
    }
    ctx = crc_16_dnp_update(ctx, input.data + offset, reductions);
    slice->argv[0] = enif_make_uint(env, ctx);
    *offsetp = offset + reductions;
    return 0;
}

static ERL_NIF_TERM
crc_nif_crc_16_dnp_1_done(ErlNifEnv *env, xnif_slice_t *slice)
{
    uint16_t ctx;
    if (slice->argc != 2 || !enif_get_uint(env, slice->argv[0], (unsigned int *)&ctx)) {
        return enif_make_badarg(env);
    }
    return enif_make_uint(env, crc_16_dnp_final(ctx));
}

ERL_NIF_TERM
crc_nif_crc_16_dnp_init_0(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{

    if (argc != 0) {
        return enif_make_badarg(env);
    }

    uint16_t ctx = crc_16_dnp_init();
    return enif_make_uint(env, ctx);
}

static int crc_nif_crc_16_dnp_update_2_work(ErlNifEnv *env, xnif_slice_t *slice, int *phasep, size_t *offsetp, size_t reductions);
static ERL_NIF_TERM crc_nif_crc_16_dnp_update_2_done(ErlNifEnv *env, xnif_slice_t *slice);

static xnif_slice_func_t crc_nif_crc_16_dnp_update_2_func = {
    crc_nif_crc_16_dnp_update_2_work, crc_nif_crc_16_dnp_update_2_done, NULL, NULL,
};

ERL_NIF_TERM
crc_nif_crc_16_dnp_update_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    unsigned int ctx;
    ErlNifBinary input;

    if (argc != 2 || !enif_get_uint(env, argv[0], &ctx) || ctx > 0xffff || !enif_inspect_binary(env, argv[1], &input)) {
        return enif_make_badarg(env);
    }

    if (input.size <= XNIF_SLICE_MAX_PER_SLICE) {
        ctx = crc_16_dnp_update((uint16_t)ctx, input.data, input.size);
        return enif_make_uint(env, ctx);
    }

    xnif_slice_t *slice = xnif_slice_create("crc_16_dnp_update", &crc_nif_crc_16_dnp_update_2_func, 0, input.size);
    if (slice == NULL) {
        return enif_make_badarg(env);
    }
    ERL_NIF_TERM newargv[2];
    newargv[0] = argv[0];
    newargv[1] = argv[1];
    return xnif_slice_schedule(env, slice, 2, newargv);
}

static int
crc_nif_crc_16_dnp_update_2_work(ErlNifEnv *env, xnif_slice_t *slice, int *phasep, size_t *offsetp, size_t reductions)
{
    size_t offset = *offsetp;
    uint16_t ctx;
    ErlNifBinary input;
    if (slice->argc != 2 || !enif_get_uint(env, slice->argv[0], (unsigned int *)&ctx) ||
        !enif_inspect_binary(env, slice->argv[1], &input)) {
        return -1;
    }
    ctx = crc_16_dnp_update(ctx, input.data + offset, reductions);
    slice->argv[0] = enif_make_uint(env, ctx);
    *offsetp = offset + reductions;
    return 0;
}

static ERL_NIF_TERM
crc_nif_crc_16_dnp_update_2_done(ErlNifEnv *env, xnif_slice_t *slice)
{
    uint16_t ctx;
    if (slice->argc != 2 || !enif_get_uint(env, slice->argv[0], (unsigned int *)&ctx)) {
        return enif_make_badarg(env);
    }
    return enif_make_uint(env, ctx);
}

ERL_NIF_TERM
crc_nif_crc_16_dnp_final_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    unsigned int ctx;

    if (argc != 1 || !enif_get_uint(env, argv[0], &ctx) || ctx > 0xffff) {
        return enif_make_badarg(env);
    }

    uint16_t crc = crc_16_dnp_final((uint16_t)ctx);
    return enif_make_uint(env, crc);
}
