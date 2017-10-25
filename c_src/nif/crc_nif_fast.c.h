// -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c++ et

static ERL_NIF_TERM crc_nif_crc_fast_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM crc_nif_crc_fast_init_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM crc_nif_crc_fast_update_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM crc_nif_crc_fast_final_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

/* crc_nif:crc_fast/2 */

static int crc_nif_crc_fast_2_work(ErlNifEnv *env, xnif_slice_t *slice, int *phasep, size_t *offsetp, size_t reductions);
static ERL_NIF_TERM crc_nif_crc_fast_2_done(ErlNifEnv *env, xnif_slice_t *slice);

static xnif_slice_func_t crc_nif_crc_fast_2_func = {
    crc_nif_crc_fast_2_work, crc_nif_crc_fast_2_done, NULL, NULL,
};

static ERL_NIF_TERM
crc_nif_crc_fast_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    crc_resource_t *resource = NULL;
    ERL_NIF_TERM res_term;
    ErlNifBinary input;
    if (argc != 2 || !enif_inspect_iolist_as_binary(env, argv[1], &input)) {
        return enif_make_badarg(env);
    }
    res_term = crc_nif_crc_fast_init_1(env, 1, argv);
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

    xnif_slice_t *slice = xnif_slice_create("crc_fast", &crc_nif_crc_fast_2_func, 0, input.size);
    if (slice == NULL) {
        return enif_make_badarg(env);
    }
    ERL_NIF_TERM newargv[2];
    newargv[0] = res_term;
    newargv[1] = argv[1];
    return xnif_slice_schedule(env, slice, 2, newargv);
}

static int
crc_nif_crc_fast_2_work(ErlNifEnv *env, xnif_slice_t *slice, int *phasep, size_t *offsetp, size_t reductions)
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
crc_nif_crc_fast_2_done(ErlNifEnv *env, xnif_slice_t *slice)
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

/* crc_nif:crc_fast_init/1 */

static ERL_NIF_TERM crc_nif_crc_fast_init_1_atom(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM crc_nif_crc_fast_init_1_map(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM crc_nif_crc_fast_init_1_ref(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM crc_nif_crc_fast_init_1_tuple(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

static ERL_NIF_TERM
crc_nif_crc_fast_init_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 1) {
        return enif_make_badarg(env);
    }

    if (enif_is_atom(env, argv[0])) {
        return crc_nif_crc_fast_init_1_atom(env, argc, argv);
    } else if (enif_is_map(env, argv[0])) {
        return crc_nif_crc_fast_init_1_map(env, argc, argv);
    } else if (enif_is_tuple(env, argv[0])) {
        return crc_nif_crc_fast_init_1_tuple(env, argc, argv);
    } else if (enif_is_ref(env, argv[0])) {
        return crc_nif_crc_fast_init_1_ref(env, argc, argv);
    } else {
        return enif_make_badarg(env);
    }
}

static ERL_NIF_TERM
crc_nif_crc_fast_init_1_atom(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    const crc_model_t *model = NULL;

    if (!crc_model_get(argv[0], &model)) {
        return enif_make_badarg(env);
    }

    crc_resource_t *resource = crc_resource_create(NULL, model, false);
    if (resource == NULL) {
        return enif_make_badarg(env);
    }

    ERL_NIF_TERM out_term;
    out_term = enif_make_resource(env, (void *)resource);
    (void)enif_release_resource((void *)resource);

    return out_term;
}

static ERL_NIF_TERM
crc_nif_crc_fast_init_1_map(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
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
    return crc_nif_crc_fast_init_1_tuple(env, 1, newargv);
}

static ERL_NIF_TERM
crc_nif_crc_fast_init_1_ref(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    const crc_resource_t *old_resource = NULL;

    if (argc != 1 || !crc_resource_get(env, argv[0], &old_resource)) {
        return enif_make_badarg(env);
    }

    crc_resource_t *new_resource = crc_resource_create(old_resource, old_resource->model, false);
    if (new_resource == NULL) {
        return enif_make_badarg(env);
    }

    ERL_NIF_TERM out_term;
    out_term = enif_make_resource(env, (void *)new_resource);
    (void)enif_release_resource((void *)new_resource);

    return out_term;
}

static ERL_NIF_TERM
crc_nif_crc_fast_init_1_tuple(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
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
    ErlNifUInt64 crc_mask = 1;
    crc_mask |= ((msb_mask - 1) << 1);

    if (poly > crc_mask || init > crc_mask || xorout > crc_mask || check > crc_mask || residue > crc_mask) {
        return enif_make_badarg(env);
    }

    const crc_model_t *model = NULL;
    crc_resource_t *resource = NULL;
    uint8_t bits = ((width / 8) + (((width % 8) | ((~(width % 8) + 1) >> 7)) & 1)) * 8;
    switch (bits) {
    case 8:
    case 16:
    case 32:
    case 64:
        break;
    default:
        if (bits > 8 && bits < 16) {
            bits = 16;
        } else if (bits > 16 && bits < 32) {
            bits = 32;
        } else if (bits > 32 && bits < 64) {
            bits = 64;
        } else {
            return enif_make_badarg(env);
        }
        break;
    }

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
            resource = crc_resource_create(NULL, model, false);                                                                    \
        } else {                                                                                                                   \
            (void)crc_model_init(&m->super);                                                                                       \
            resource = crc_resource_create(NULL, &m->super, false);                                                                \
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

/* crc_nif:crc_fast_update/2 */

static int crc_nif_crc_fast_update_2_work(ErlNifEnv *env, xnif_slice_t *slice, int *phasep, size_t *offsetp, size_t reductions);
static ERL_NIF_TERM crc_nif_crc_fast_update_2_done(ErlNifEnv *env, xnif_slice_t *slice);

static xnif_slice_func_t crc_nif_crc_fast_update_2_func = {
    crc_nif_crc_fast_update_2_work, crc_nif_crc_fast_update_2_done, NULL, NULL,
};

static ERL_NIF_TERM
crc_nif_crc_fast_update_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    const crc_resource_t *old_resource = NULL;
    ErlNifBinary input;

    if (argc != 2 || !crc_resource_get(env, argv[0], &old_resource) || old_resource->slow ||
        !enif_inspect_iolist_as_binary(env, argv[1], &input)) {
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

    new_resource = crc_resource_clone(old_resource);
    if (new_resource == NULL) {
        return enif_make_badarg(env);
    }
    xnif_slice_t *slice = xnif_slice_create("crc_fast_update", &crc_nif_crc_fast_update_2_func, 0, input.size);
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
crc_nif_crc_fast_update_2_work(ErlNifEnv *env, xnif_slice_t *slice, int *phasep, size_t *offsetp, size_t reductions)
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
crc_nif_crc_fast_update_2_done(ErlNifEnv *env, xnif_slice_t *slice)
{
    return slice->argv[0];
}

/* crc_nif:crc_fast_final/1 */

static ERL_NIF_TERM
crc_nif_crc_fast_final_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    const crc_resource_t *resource = NULL;

    if (argc != 1 || !crc_resource_get(env, argv[0], &resource) || resource->slow) {
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
