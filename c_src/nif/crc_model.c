// -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c++ et

#include "crc_model.h"

#ifndef kcalloc
#define kcalloc(N, Z) memset(enif_alloc((N) * (Z)), 0, (N) * (Z))
#endif
#ifndef kmalloc
#define kmalloc(Z) enif_alloc(Z)
#endif
#ifndef krealloc
#define krealloc(P, Z) enif_realloc(P, Z)
#endif
#ifndef kfree
#define kfree(P) enif_free(P)
#endif

#include "khash.h"

#if ULONG_MAX == ULLONG_MAX
KHASH_MAP_INIT_INT64(crc_models, const crc_model_t *)
#else
KHASH_MAP_INIT_INT(crc_models, const crc_model_t *)
#endif

static khash_t(crc_models) *crc_nif_models = NULL;

static crc_params_t crc_8_params = {
    {0, CRC_MODEL_TYPE_NORMAL, "crc_8", "CRC-8"}, NULL, 8, 0x07, 0x00, false, false, 0x00, 0xf4, 0x00,
};

static const crc_model_t *crc_nif_model_list[] = {
    &crc_8_params.model, NULL,
};

int
crc_model_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info)
{
    khash_t(crc_models) *models = NULL;
    models = kh_init(crc_models);
    if (models == NULL) {
        return ENOMEM;
    }

    khiter_t iter;
    int r;
    crc_model_t *model = NULL;

    size_t i = 0;
    while (crc_nif_model_list[i] != NULL) {
        model = (crc_model_t *)crc_nif_model_list[i++];
        model->key = enif_make_atom(env, model->keystring);
        iter = kh_put(crc_models, models, model->key, &r);
        switch (r) {
        case 1: // the bucket is empty (never used)
        case 2: // the element in the bucket has been deleted
            kh_val(models, iter) = model;
            break;
        default:
            kh_destroy(crc_models, models);
            return r;
        }
    }

    if (crc_nif_models != NULL) {
        kh_destroy(crc_models, crc_nif_models);
        crc_nif_models = NULL;
    }

    crc_nif_models = models;

    return 0;
}

int
crc_model_get(ERL_NIF_TERM key, const crc_model_t **model)
{
    khiter_t iter;
    if (model != NULL) {
        *model = NULL;
    }
    iter = kh_get(crc_models, crc_nif_models, key);
    if (iter == kh_end(crc_nif_models)) {
        return 0;
    }
    if (model != NULL) {
        *model = (void *)kh_val(crc_nif_models, iter);
    }
    return 1;
}

int
crc_model_match(const crc_params_t *params, const crc_model_t **model)
{
    khint_t i;
    const crc_params_t *p = NULL;
    if (model != NULL) {
        *model = NULL;
    }
    for (i = kh_begin(crc_nif_models); i != kh_end(crc_nif_models); ++i) {
        if (!kh_exist(crc_nif_models, i)) {
            continue;
        }
        p = (const crc_params_t *)kh_val(crc_nif_models, i);
        if (p->model.type != CRC_MODEL_TYPE_NORMAL) {
            p = NULL;
            continue;
        }
        if (params->width == p->width && params->poly == p->poly && params->init == p->init && params->refin == p->refin &&
            params->refout == p->refout && params->xorout == p->xorout && params->check == p->check &&
            params->residue == p->residue) {
            if (model != NULL) {
                *model = &p->model;
            }
            return 1;
        }
    }
    return 0;
}

ERL_NIF_TERM
crc_model_list(ErlNifEnv *env)
{
    ERL_NIF_TERM out;
    out = enif_make_list(env, 0);
    ERL_NIF_TERM key;
    khint_t i;
    for (i = kh_begin(crc_nif_models); i != kh_end(crc_nif_models); ++i) {
        if (!kh_exist(crc_nif_models, i)) {
            continue;
        }
        key = (ERL_NIF_TERM)kh_key(crc_nif_models, i);
        out = enif_make_list_cell(env, key, out);
    }
    return out;
}
