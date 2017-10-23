// -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c++ et

#include "crc_resource.h"

static ErlNifResourceType *crc_resource_type = NULL;

static void
crc_resource_dtor(ErlNifEnv *env, void *obj)
{
    if (obj == NULL) {
        return;
    }
    crc_resource_t *resource = (void *)obj;
    if (resource->parent != NULL) {
        (void)enif_release_resource((void *)resource->parent);
        resource->parent = NULL;
    }
    return;
}

int
crc_resource_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info)
{
    /* Open resource type */
    if (crc_resource_type == NULL) {
        crc_resource_type =
            enif_open_resource_type(env, NULL, "crc_resource", crc_resource_dtor, ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);
        if (crc_resource_type == NULL) {
            return -1;
        }
    }
    return 0;
}

int
crc_resource_upgrade(ErlNifEnv *env, void **priv_data, void **old_priv_data, ERL_NIF_TERM load_info)
{
    return crc_resource_load(env, priv_data, load_info);
}

void
crc_resource_unload(ErlNifEnv *env, void **priv_data)
{
    crc_resource_type = NULL;
}

static crc_resource_t *
crc_resource_create_child(const crc_resource_t *parent)
{
    crc_resource_t *resource = NULL;

#define RESOURCE_CREATE_PARENT(type)                                                                                               \
    do {                                                                                                                           \
        crc_resource_uint8_t *p = (void *)enif_alloc_resource(crc_resource_type, sizeof(*p));                                      \
        if (p == NULL) {                                                                                                           \
            return NULL;                                                                                                           \
        }                                                                                                                          \
        (void)enif_keep_resource((void *)parent);                                                                                  \
        p->parent = (void *)parent;                                                                                                \
        p->model = (void *)parent->model;                                                                                          \
        p->value = p->model->init;                                                                                                 \
        resource = (void *)p;                                                                                                      \
    } while (0)

    switch (parent->model->bits) {
    case 8: {
        RESOURCE_CREATE_PARENT(uint8);
        break;
    } break;
    case 16:
        RESOURCE_CREATE_PARENT(uint16);
        break;
    case 32:
        RESOURCE_CREATE_PARENT(uint32);
        break;
    case 64:
        RESOURCE_CREATE_PARENT(uint64);
        break;
    default:
        return NULL;
    }

#undef RESOURCE_CREATE_PARENT

    return resource;
}

static crc_resource_t *
crc_resource_create_reference(const crc_model_t *model)
{
    crc_resource_t *resource = NULL;

#define RESOURCE_CREATE_REFERENCE(type)                                                                                            \
    do {                                                                                                                           \
        crc_resource_##type##_t *p = (void *)enif_alloc_resource(crc_resource_type, sizeof(*p));                                   \
        if (p == NULL) {                                                                                                           \
            return NULL;                                                                                                           \
        }                                                                                                                          \
        p->parent = NULL;                                                                                                          \
        p->model = (void *)model;                                                                                                  \
        p->value = p->model->init;                                                                                                 \
        resource = (void *)p;                                                                                                      \
    } while (0)

    switch (model->bits) {
    case 8: {
        RESOURCE_CREATE_REFERENCE(uint8);
        break;
    } break;
    case 16:
        RESOURCE_CREATE_REFERENCE(uint16);
        break;
    case 32:
        RESOURCE_CREATE_REFERENCE(uint32);
        break;
    case 64:
        RESOURCE_CREATE_REFERENCE(uint64);
        break;
    default:
        return NULL;
    }

#undef RESOURCE_CREATE_REFERENCE

    return resource;
}

crc_resource_t *
crc_resource_create(const crc_resource_t *parent, const crc_model_t *model)
{
    if (model->root_keystring[0] != '\0') {
        return crc_resource_create_reference(model);
    }
    if (parent != NULL && parent->model == model) {
        return crc_resource_create_child(parent);
    }
    crc_resource_t *resource = NULL;

#define RESOURCE_CREATE_EMBED(type)                                                                                                \
    do {                                                                                                                           \
        crc_resource_embed_##type##_t *p = (void *)enif_alloc_resource(crc_resource_type, sizeof(*p));                             \
        if (p == NULL) {                                                                                                           \
            return NULL;                                                                                                           \
        }                                                                                                                          \
        (void)memcpy(&p->model, model, sizeof(p->model));                                                                          \
        p->super.parent = NULL;                                                                                                    \
        p->super.model = &p->model;                                                                                                \
        p->super.value = p->model.init;                                                                                            \
        resource = (void *)p;                                                                                                      \
    } while (0)

    switch (model->bits) {
    case 8:
        RESOURCE_CREATE_EMBED(uint8);
        break;
    case 16:
        RESOURCE_CREATE_EMBED(uint16);
        break;
    case 32:
        RESOURCE_CREATE_EMBED(uint32);
        break;
    case 64:
        RESOURCE_CREATE_EMBED(uint64);
        break;
    default:
        return NULL;
    }

#undef RESOURCE_CREATE_EMBED

    return resource;
}

int
crc_resource_get(ErlNifEnv *env, ERL_NIF_TERM resource_term, const crc_resource_t **resource)
{
    if (resource != NULL) {
        *resource = NULL;
    }
    const crc_resource_t *p = NULL;
    if (!enif_get_resource(env, resource_term, crc_resource_type, (void **)&p)) {
        return 0;
    }
    if (resource != NULL) {
        *resource = p;
    }
    return 1;
}

int
crc_resource_update(const crc_resource_t *old_resource, const uint8_t *buf, size_t len, crc_resource_t **new_resource)
{
    if (new_resource == NULL) {
        return 0;
    }
    *new_resource = NULL;
    crc_resource_t *resource = crc_resource_create(old_resource, old_resource->model);
    if (resource == NULL) {
        return 0;
    }
    if (!crc_resource_update_unsafe(resource, buf, len)) {
        (void)enif_release_resource((void *)resource);
        return 0;
    }
    *new_resource = resource;
    return 1;
}

int
crc_resource_update_unsafe(crc_resource_t *resource, const uint8_t *buf, size_t len)
{
#define CRC_RESOURCE_UPDATE_CALL(type)                                                                                             \
    do {                                                                                                                           \
        crc_resource_##type##_t *r = (void *)resource;                                                                             \
        return crc_algorithm_update(resource->model, buf, len, (void *)&r->value);                                                 \
    } while (0)

    switch (resource->model->bits) {
    case 8:
        CRC_RESOURCE_UPDATE_CALL(uint8);
        break;
    case 16:
        CRC_RESOURCE_UPDATE_CALL(uint16);
        break;
    case 32:
        CRC_RESOURCE_UPDATE_CALL(uint32);
        break;
    case 64:
        CRC_RESOURCE_UPDATE_CALL(uint64);
        break;
    default:
        break;
    }

#undef CRC_RESOURCE_UPDATE_CALL

    return 0;
}

int
crc_resource_final(const crc_resource_t *resource, void *value)
{
#define CRC_RESOURCE_FINAL_CALL(type)                                                                                              \
    do {                                                                                                                           \
        crc_resource_##type##_t *r = (void *)resource;                                                                             \
        type##_t *new_value = (type##_t *)value;                                                                                   \
        *new_value = r->value;                                                                                                     \
        return crc_algorithm_final(resource->model, new_value);                                                                    \
    } while (0)

    switch (resource->model->bits) {
    case 8:
        CRC_RESOURCE_FINAL_CALL(uint8);
        break;
    case 16:
        CRC_RESOURCE_FINAL_CALL(uint16);
        break;
    case 32:
        CRC_RESOURCE_FINAL_CALL(uint32);
        break;
    case 64:
        CRC_RESOURCE_FINAL_CALL(uint64);
        break;
    default:
        break;
    }

#undef CRC_RESOURCE_FINAL_CALL

    return 0;
}
