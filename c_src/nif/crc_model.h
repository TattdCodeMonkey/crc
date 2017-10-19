// -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c++ et

#ifndef CRC_MODEL_H
#define CRC_MODEL_H

#include "crc_nif.h"

typedef enum crc_model_type_t { CRC_MODEL_TYPE_NORMAL = 0, CRC_MODEL_TYPE_SICK } crc_model_type_t;

typedef struct crc_model_s {
    ERL_NIF_TERM key;
    crc_model_type_t type;
    char keystring[256];
    char name[256];
} crc_model_t;

typedef struct crc_params_s {
    crc_model_t model;
    void *table;
    uint8_t width;
    uint64_t poly;
    uint64_t init;
    bool refin;
    bool refout;
    uint64_t xorout;
    uint64_t check;
    uint64_t residue;
} crc_params_t;

// typedef struct crc_function_s {
//     int init(const void *params, void **context);
//     int fastupdate(void *context, const unsigned char *buf, size_t len);
//     int update(const void *old_context, void **new_context, const unsigned char *buf, size_t len);
//     int final(const void *context, ErlNifEnv *env, ERL_NIF_TERM *out_term);
// } crc_function_t;

typedef struct crc_context_s {
    const crc_model_t *model;
    uint64_t value;
} crc_context_t;

// int
// crc_init(const void *params, void **context)
// {
//     crc_context_t *c = (void *)enif_alloc_resource(crc_resource_type, sizeof(*c));
//     if (c == NULL) {
//         return -1;
//     }
//     c->params = (const crc_params_t *)params;
//     c->value = c->params->init;
//     *context = (void *)c;
//     return 0;
// }

// int
// crc_fastupdate(void *context, const unsigned char *buf, size_t len)
// {
//     crc_context_t *c = (void *)context;
//     uint64_t val = c->value;
//     if (c->params->table != NULL) {
//         while (len--) {
//             val = (val >> 8) ^ c->params->table[(val ^ (uint8_t)*buf++) & 0xFF];
//         }
//         c->value = val;
//     } else {
//         // the slower calculation here
//     }
//     return 0;
// }

// int
// crc_update(const void *old_context, void **new_context, const unsigned char *buf, size_t len)
// {
//     const crc_context_t *old_c = (const void *)old_context;
//     crc_context_t *new_c = (void *)enif_alloc_resource(crc_resource_type, sizeof(*new_c));
//     if (new_c == NULL) {
//         return -1;
//     }
//     (void)memcpy(new_c, old_c, sizeof(*new_c));
//     int retval = crc_fastupdate(new_c, buf, len);
//     if (retval != 0) {
//         (void)enif_release_resource((void *)new_c);
//         return retval;
//     }
//     *new_context = (void *)new_c;
//     return 0;
// }

// int
// crc_final(const void *context, ErlNifEnv *env, ERL_NIF_TERM *out_term)
// {
//     const crc_context_t *c = (const void *)context;
//     // reflect out or xorout if necessary
//     *out_term = enif_make_uint(env, c->value);
//     return 0;
// }

#ifdef __cplusplus
extern "C" {
#endif

extern int crc_model_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info);
extern int crc_model_get(ERL_NIF_TERM key, const crc_model_t **model);
extern int crc_model_match(const crc_params_t *params, const crc_model_t **model);
extern ERL_NIF_TERM crc_model_list(ErlNifEnv *env);

#ifdef __cplusplus
}
#endif

#endif
