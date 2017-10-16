// -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c++ et

#include "crc_32.h"
#include "xnif_slice.h"

static const uint32_t crc_32_table[256] = {
    0x00000000ul, 0x77073096ul, 0xEE0E612Cul, 0x990951BAul, 0x076DC419ul, 0x706AF48Ful, 0xE963A535ul, 0x9E6495A3ul, 0x0EDB8832ul,
    0x79DCB8A4ul, 0xE0D5E91Eul, 0x97D2D988ul, 0x09B64C2Bul, 0x7EB17CBDul, 0xE7B82D07ul, 0x90BF1D91ul, 0x1DB71064ul, 0x6AB020F2ul,
    0xF3B97148ul, 0x84BE41DEul, 0x1ADAD47Dul, 0x6DDDE4EBul, 0xF4D4B551ul, 0x83D385C7ul, 0x136C9856ul, 0x646BA8C0ul, 0xFD62F97Aul,
    0x8A65C9ECul, 0x14015C4Ful, 0x63066CD9ul, 0xFA0F3D63ul, 0x8D080DF5ul, 0x3B6E20C8ul, 0x4C69105Eul, 0xD56041E4ul, 0xA2677172ul,
    0x3C03E4D1ul, 0x4B04D447ul, 0xD20D85FDul, 0xA50AB56Bul, 0x35B5A8FAul, 0x42B2986Cul, 0xDBBBC9D6ul, 0xACBCF940ul, 0x32D86CE3ul,
    0x45DF5C75ul, 0xDCD60DCFul, 0xABD13D59ul, 0x26D930ACul, 0x51DE003Aul, 0xC8D75180ul, 0xBFD06116ul, 0x21B4F4B5ul, 0x56B3C423ul,
    0xCFBA9599ul, 0xB8BDA50Ful, 0x2802B89Eul, 0x5F058808ul, 0xC60CD9B2ul, 0xB10BE924ul, 0x2F6F7C87ul, 0x58684C11ul, 0xC1611DABul,
    0xB6662D3Dul, 0x76DC4190ul, 0x01DB7106ul, 0x98D220BCul, 0xEFD5102Aul, 0x71B18589ul, 0x06B6B51Ful, 0x9FBFE4A5ul, 0xE8B8D433ul,
    0x7807C9A2ul, 0x0F00F934ul, 0x9609A88Eul, 0xE10E9818ul, 0x7F6A0DBBul, 0x086D3D2Dul, 0x91646C97ul, 0xE6635C01ul, 0x6B6B51F4ul,
    0x1C6C6162ul, 0x856530D8ul, 0xF262004Eul, 0x6C0695EDul, 0x1B01A57Bul, 0x8208F4C1ul, 0xF50FC457ul, 0x65B0D9C6ul, 0x12B7E950ul,
    0x8BBEB8EAul, 0xFCB9887Cul, 0x62DD1DDFul, 0x15DA2D49ul, 0x8CD37CF3ul, 0xFBD44C65ul, 0x4DB26158ul, 0x3AB551CEul, 0xA3BC0074ul,
    0xD4BB30E2ul, 0x4ADFA541ul, 0x3DD895D7ul, 0xA4D1C46Dul, 0xD3D6F4FBul, 0x4369E96Aul, 0x346ED9FCul, 0xAD678846ul, 0xDA60B8D0ul,
    0x44042D73ul, 0x33031DE5ul, 0xAA0A4C5Ful, 0xDD0D7CC9ul, 0x5005713Cul, 0x270241AAul, 0xBE0B1010ul, 0xC90C2086ul, 0x5768B525ul,
    0x206F85B3ul, 0xB966D409ul, 0xCE61E49Ful, 0x5EDEF90Eul, 0x29D9C998ul, 0xB0D09822ul, 0xC7D7A8B4ul, 0x59B33D17ul, 0x2EB40D81ul,
    0xB7BD5C3Bul, 0xC0BA6CADul, 0xEDB88320ul, 0x9ABFB3B6ul, 0x03B6E20Cul, 0x74B1D29Aul, 0xEAD54739ul, 0x9DD277AFul, 0x04DB2615ul,
    0x73DC1683ul, 0xE3630B12ul, 0x94643B84ul, 0x0D6D6A3Eul, 0x7A6A5AA8ul, 0xE40ECF0Bul, 0x9309FF9Dul, 0x0A00AE27ul, 0x7D079EB1ul,
    0xF00F9344ul, 0x8708A3D2ul, 0x1E01F268ul, 0x6906C2FEul, 0xF762575Dul, 0x806567CBul, 0x196C3671ul, 0x6E6B06E7ul, 0xFED41B76ul,
    0x89D32BE0ul, 0x10DA7A5Aul, 0x67DD4ACCul, 0xF9B9DF6Ful, 0x8EBEEFF9ul, 0x17B7BE43ul, 0x60B08ED5ul, 0xD6D6A3E8ul, 0xA1D1937Eul,
    0x38D8C2C4ul, 0x4FDFF252ul, 0xD1BB67F1ul, 0xA6BC5767ul, 0x3FB506DDul, 0x48B2364Bul, 0xD80D2BDAul, 0xAF0A1B4Cul, 0x36034AF6ul,
    0x41047A60ul, 0xDF60EFC3ul, 0xA867DF55ul, 0x316E8EEFul, 0x4669BE79ul, 0xCB61B38Cul, 0xBC66831Aul, 0x256FD2A0ul, 0x5268E236ul,
    0xCC0C7795ul, 0xBB0B4703ul, 0x220216B9ul, 0x5505262Ful, 0xC5BA3BBEul, 0xB2BD0B28ul, 0x2BB45A92ul, 0x5CB36A04ul, 0xC2D7FFA7ul,
    0xB5D0CF31ul, 0x2CD99E8Bul, 0x5BDEAE1Dul, 0x9B64C2B0ul, 0xEC63F226ul, 0x756AA39Cul, 0x026D930Aul, 0x9C0906A9ul, 0xEB0E363Ful,
    0x72076785ul, 0x05005713ul, 0x95BF4A82ul, 0xE2B87A14ul, 0x7BB12BAEul, 0x0CB61B38ul, 0x92D28E9Bul, 0xE5D5BE0Dul, 0x7CDCEFB7ul,
    0x0BDBDF21ul, 0x86D3D2D4ul, 0xF1D4E242ul, 0x68DDB3F8ul, 0x1FDA836Eul, 0x81BE16CDul, 0xF6B9265Bul, 0x6FB077E1ul, 0x18B74777ul,
    0x88085AE6ul, 0xFF0F6A70ul, 0x66063BCAul, 0x11010B5Cul, 0x8F659EFFul, 0xF862AE69ul, 0x616BFFD3ul, 0x166CCF45ul, 0xA00AE278ul,
    0xD70DD2EEul, 0x4E048354ul, 0x3903B3C2ul, 0xA7672661ul, 0xD06016F7ul, 0x4969474Dul, 0x3E6E77DBul, 0xAED16A4Aul, 0xD9D65ADCul,
    0x40DF0B66ul, 0x37D83BF0ul, 0xA9BCAE53ul, 0xDEBB9EC5ul, 0x47B2CF7Ful, 0x30B5FFE9ul, 0xBDBDF21Cul, 0xCABAC28Aul, 0x53B39330ul,
    0x24B4A3A6ul, 0xBAD03605ul, 0xCDD70693ul, 0x54DE5729ul, 0x23D967BFul, 0xB3667A2Eul, 0xC4614AB8ul, 0x5D681B02ul, 0x2A6F2B94ul,
    0xB40BBE37ul, 0xC30C8EA1ul, 0x5A05DF1Bul, 0x2D02EF8Dul};

uint32_t
crc_32_init(void)
{
    return 0xFFFFFFFFul;
}

uint32_t
crc_32_update(uint32_t ctx, const unsigned char *buf, size_t len)
{
    while (len--) {
        ctx = (ctx >> 8) ^ crc_32_table[(ctx ^ (uint32_t)*buf++) & 0x000000FFul];
    }
    return ctx;
}

uint32_t
crc_32_final(uint32_t ctx)
{
    return (ctx ^ 0xFFFFFFFFul);
}

/* NIF Functions */

static int crc_nif_crc_32_1_work(ErlNifEnv *env, xnif_slice_t *slice, int *phasep, size_t *offsetp, size_t reductions);
static ERL_NIF_TERM crc_nif_crc_32_1_done(ErlNifEnv *env, xnif_slice_t *slice);

static xnif_slice_func_t crc_nif_crc_32_1_func = {
    crc_nif_crc_32_1_work, crc_nif_crc_32_1_done, NULL, NULL,
};

ERL_NIF_TERM
crc_nif_crc_32_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary input;

    if (argc != 1 || !enif_inspect_binary(env, argv[0], &input)) {
        return enif_make_badarg(env);
    }

    if (input.size <= XNIF_SLICE_MAX_PER_SLICE) {
        uint32_t crc;
        crc = crc_32(input.data, input.size);
        return enif_make_uint(env, crc);
    }

    xnif_slice_t *slice = xnif_slice_create("crc_32", &crc_nif_crc_32_1_func, 0, input.size);
    if (slice == NULL) {
        return enif_make_badarg(env);
    }
    uint32_t ctx = crc_32_init();
    ERL_NIF_TERM newargv[2];
    newargv[0] = enif_make_uint(env, ctx);
    newargv[1] = argv[0];
    return xnif_slice_schedule(env, slice, 2, newargv);
}

static int
crc_nif_crc_32_1_work(ErlNifEnv *env, xnif_slice_t *slice, int *phasep, size_t *offsetp, size_t reductions)
{
    size_t offset = *offsetp;
    uint32_t ctx;
    ErlNifBinary input;
    if (slice->argc != 2 || !enif_get_uint(env, slice->argv[0], (unsigned int *)&ctx) ||
        !enif_inspect_binary(env, slice->argv[1], &input)) {
        return -1;
    }
    ctx = crc_32_update(ctx, input.data + offset, reductions);
    slice->argv[0] = enif_make_uint(env, ctx);
    *offsetp = offset + reductions;
    return 0;
}

static ERL_NIF_TERM
crc_nif_crc_32_1_done(ErlNifEnv *env, xnif_slice_t *slice)
{
    uint32_t ctx;
    if (slice->argc != 2 || !enif_get_uint(env, slice->argv[0], (unsigned int *)&ctx)) {
        return enif_make_badarg(env);
    }
    return enif_make_uint(env, crc_32_final(ctx));
}

ERL_NIF_TERM
crc_nif_crc_32_init_0(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{

    if (argc != 0) {
        return enif_make_badarg(env);
    }

    uint32_t ctx = crc_32_init();
    return enif_make_uint(env, ctx);
}

static int crc_nif_crc_32_update_2_work(ErlNifEnv *env, xnif_slice_t *slice, int *phasep, size_t *offsetp, size_t reductions);
static ERL_NIF_TERM crc_nif_crc_32_update_2_done(ErlNifEnv *env, xnif_slice_t *slice);

static xnif_slice_func_t crc_nif_crc_32_update_2_func = {
    crc_nif_crc_32_update_2_work, crc_nif_crc_32_update_2_done, NULL, NULL,
};

ERL_NIF_TERM
crc_nif_crc_32_update_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    unsigned int ctx;
    ErlNifBinary input;

    if (argc != 2 || !enif_get_uint(env, argv[0], &ctx) || ctx > 0xffffffff || !enif_inspect_binary(env, argv[1], &input)) {
        return enif_make_badarg(env);
    }

    if (input.size <= XNIF_SLICE_MAX_PER_SLICE) {
        ctx = crc_32_update((uint32_t)ctx, input.data, input.size);
        return enif_make_uint(env, ctx);
    }

    xnif_slice_t *slice = xnif_slice_create("crc_32_update", &crc_nif_crc_32_update_2_func, 0, input.size);
    if (slice == NULL) {
        return enif_make_badarg(env);
    }
    ERL_NIF_TERM newargv[2];
    newargv[0] = argv[0];
    newargv[1] = argv[1];
    return xnif_slice_schedule(env, slice, 2, newargv);
}

static int
crc_nif_crc_32_update_2_work(ErlNifEnv *env, xnif_slice_t *slice, int *phasep, size_t *offsetp, size_t reductions)
{
    size_t offset = *offsetp;
    uint32_t ctx;
    ErlNifBinary input;
    if (slice->argc != 2 || !enif_get_uint(env, slice->argv[0], (unsigned int *)&ctx) ||
        !enif_inspect_binary(env, slice->argv[1], &input)) {
        return -1;
    }
    ctx = crc_32_update(ctx, input.data + offset, reductions);
    slice->argv[0] = enif_make_uint(env, ctx);
    *offsetp = offset + reductions;
    return 0;
}

static ERL_NIF_TERM
crc_nif_crc_32_update_2_done(ErlNifEnv *env, xnif_slice_t *slice)
{
    uint32_t ctx;
    if (slice->argc != 2 || !enif_get_uint(env, slice->argv[0], (unsigned int *)&ctx)) {
        return enif_make_badarg(env);
    }
    return enif_make_uint(env, ctx);
}

ERL_NIF_TERM
crc_nif_crc_32_final_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    unsigned int ctx;

    if (argc != 1 || !enif_get_uint(env, argv[0], &ctx) || ctx > 0xffffffff) {
        return enif_make_badarg(env);
    }

    uint32_t crc = crc_32_final((uint32_t)ctx);
    return enif_make_uint(env, crc);
}
