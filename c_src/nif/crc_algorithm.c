// -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c++ et

#include "crc_algorithm.h"

/* crc_algorithm_reflect/3 */

static uint8_t crc_algorithm_uint8_reflect(uint8_t reg, uint8_t width);
static uint16_t crc_algorithm_uint16_reflect(uint16_t reg, uint8_t width);
static uint32_t crc_algorithm_uint32_reflect(uint32_t reg, uint8_t width);
static uint64_t crc_algorithm_uint64_reflect(uint64_t reg, uint8_t width);

int
crc_algorithm_reflect(const crc_model_t *model, void *value, uint8_t width)
{
#define CRC_ALGORITHM_REFLECT_CALL(type)                                                                                           \
    do {                                                                                                                           \
        type##_t *p = (type##_t *)value;                                                                                           \
        type##_t old_value = *p;                                                                                                   \
        type##_t new_value = crc_algorithm_##type##_reflect(old_value, width);                                                     \
        *p = new_value;                                                                                                            \
    } while (0)

    switch (model->bits) {
    case 8:
        CRC_ALGORITHM_REFLECT_CALL(uint8);
        break;
    case 16:
        CRC_ALGORITHM_REFLECT_CALL(uint16);
        break;
    case 32:
        CRC_ALGORITHM_REFLECT_CALL(uint32);
        break;
    case 64:
        CRC_ALGORITHM_REFLECT_CALL(uint64);
        break;
    default:
        return 0;
    }

#undef CRC_ALGORITHM_REFLECT_CALL

    return 1;
}

#define CRC_ALGORITHM_REFLECT_DEF(type)                                                                                            \
    inline type##_t crc_algorithm_##type##_reflect(type##_t reg, uint8_t width)                                                    \
    {                                                                                                                              \
        size_t i;                                                                                                                  \
        type##_t res;                                                                                                              \
        res = reg & 0x01;                                                                                                          \
        for (i = 0; i < (width - 1); i++) {                                                                                        \
            reg >>= 1;                                                                                                             \
            res <<= 1;                                                                                                             \
            res |= (reg & 0x01);                                                                                                   \
        }                                                                                                                          \
        return res;                                                                                                                \
    }

CRC_ALGORITHM_REFLECT_DEF(uint8)
CRC_ALGORITHM_REFLECT_DEF(uint16)
CRC_ALGORITHM_REFLECT_DEF(uint32)
CRC_ALGORITHM_REFLECT_DEF(uint64)

#undef CRC_ALGORITHM_REFLECT_DEF

/* crc_algorithm_compile/1 */

static int crc_algorithm_uint8_compile(crc_model_uint8_t *model);
static int crc_algorithm_uint16_compile(crc_model_uint16_t *model);
static int crc_algorithm_uint32_compile(crc_model_uint32_t *model);
static int crc_algorithm_uint64_compile(crc_model_uint64_t *model);

int
crc_algorithm_compile(crc_model_t *model)
{
    switch (model->bits) {
    case 8:
        return crc_algorithm_uint8_compile((void *)model);
    case 16:
        return crc_algorithm_uint16_compile((void *)model);
    case 32:
        return crc_algorithm_uint32_compile((void *)model);
    case 64:
        return crc_algorithm_uint64_compile((void *)model);
    default:
        return 0;
    }
}

#define CRC_ALGORITHM_COMPILE_DEF(type)                                                                                            \
    inline int crc_algorithm_##type##_compile(crc_model_##type##_t *model)                                                         \
    {                                                                                                                              \
        if (model->sick) {                                                                                                         \
            return 0;                                                                                                              \
        }                                                                                                                          \
        uint8_t crc_shift = (model->width < 8) ? 8 - model->width : 0;                                                             \
        type##_t msb_mask = 1;                                                                                                     \
        msb_mask <<= (model->width - 1);                                                                                           \
        type##_t mask = 1;                                                                                                         \
        mask |= ((msb_mask - 1) << 1);                                                                                             \
        uint8_t tbl_idx_width = 8;                                                                                                 \
        size_t table_length = 1 << tbl_idx_width;                                                                                  \
        (void)memset(model->table, 0, table_length * sizeof(type##_t));                                                            \
        size_t i;                                                                                                                  \
        size_t j;                                                                                                                  \
        type##_t reg;                                                                                                              \
        for (i = 0; i < table_length; i++) {                                                                                       \
            reg = i;                                                                                                               \
            if (model->refin) {                                                                                                    \
                reg = crc_algorithm_##type##_reflect(reg, tbl_idx_width);                                                          \
            }                                                                                                                      \
            reg <<= (model->width - tbl_idx_width + crc_shift);                                                                    \
            for (j = 0; j < tbl_idx_width; j++) {                                                                                  \
                if ((reg & (msb_mask << crc_shift)) != 0) {                                                                        \
                    reg <<= 1;                                                                                                     \
                    reg ^= (type##_t)(model->poly << crc_shift);                                                                   \
                } else {                                                                                                           \
                    reg <<= 1;                                                                                                     \
                }                                                                                                                  \
            }                                                                                                                      \
            if (model->refin) {                                                                                                    \
                reg = crc_algorithm_##type##_reflect((type##_t)(reg >> crc_shift), model->width);                                  \
                reg <<= crc_shift;                                                                                                 \
            }                                                                                                                      \
            model->table[i] = (type##_t)(reg >> crc_shift) & mask;                                                                 \
        }                                                                                                                          \
        return 1;                                                                                                                  \
    }

CRC_ALGORITHM_COMPILE_DEF(uint8)
CRC_ALGORITHM_COMPILE_DEF(uint16)
CRC_ALGORITHM_COMPILE_DEF(uint32)
CRC_ALGORITHM_COMPILE_DEF(uint64)

#undef CRC_ALGORITHM_COMPILE_DEF

/* crc_algorithm_update/4 */

static int crc_algorithm_uint8_update(const crc_model_uint8_t *model, const uint8_t *buf, size_t len, uint8_t *value);
static int crc_algorithm_uint16_update(const crc_model_uint16_t *model, const uint8_t *buf, size_t len, uint16_t *value);
static int crc_algorithm_uint32_update(const crc_model_uint32_t *model, const uint8_t *buf, size_t len, uint32_t *value);
static int crc_algorithm_uint64_update(const crc_model_uint64_t *model, const uint8_t *buf, size_t len, uint64_t *value);

int
crc_algorithm_update(const crc_model_t *model, const uint8_t *buf, size_t len, void *value)
{
    if (len == 0) {
        return 1;
    }
    switch (model->bits) {
    case 8:
        return crc_algorithm_uint8_update((void *)model, buf, len, (uint8_t *)value);
    case 16:
        return crc_algorithm_uint16_update((void *)model, buf, len, (uint16_t *)value);
    case 32:
        return crc_algorithm_uint32_update((void *)model, buf, len, (uint32_t *)value);
    case 64:
        return crc_algorithm_uint64_update((void *)model, buf, len, (uint64_t *)value);
    default:
        return 0;
    }
}

static int crc_algorithm_uint8_update_sick(const crc_model_uint8_t *model, const uint8_t *buf, size_t len, uint8_t *value);
static int crc_algorithm_uint16_update_sick(const crc_model_uint16_t *model, const uint8_t *buf, size_t len, uint16_t *value);
static int crc_algorithm_uint32_update_sick(const crc_model_uint32_t *model, const uint8_t *buf, size_t len, uint32_t *value);
static int crc_algorithm_uint64_update_sick(const crc_model_uint64_t *model, const uint8_t *buf, size_t len, uint64_t *value);

static int crc_algorithm_uint8_update_table(const crc_model_uint8_t *model, const uint8_t *buf, size_t len, uint8_t *value);
static int crc_algorithm_uint16_update_table(const crc_model_uint16_t *model, const uint8_t *buf, size_t len, uint16_t *value);
static int crc_algorithm_uint32_update_table(const crc_model_uint32_t *model, const uint8_t *buf, size_t len, uint32_t *value);
static int crc_algorithm_uint64_update_table(const crc_model_uint64_t *model, const uint8_t *buf, size_t len, uint64_t *value);

#define CRC_ALGORITHM_UPDATE_DEF(type)                                                                                             \
    inline int crc_algorithm_##type##_update(const crc_model_##type##_t *model, const uint8_t *buf, size_t len, type##_t *value)   \
    {                                                                                                                              \
        if (model->sick) {                                                                                                         \
            return crc_algorithm_##type##_update_sick(model, buf, len, value);                                                     \
        } else {                                                                                                                   \
            return crc_algorithm_##type##_update_table(model, buf, len, value);                                                    \
        }                                                                                                                          \
    }

CRC_ALGORITHM_UPDATE_DEF(uint8)
CRC_ALGORITHM_UPDATE_DEF(uint16)
CRC_ALGORITHM_UPDATE_DEF(uint32)
CRC_ALGORITHM_UPDATE_DEF(uint64)

#undef CRC_ALGORITHM_UPDATE_DEF

/* crc_algorithm_update_sick/4 */

static int
crc_algorithm_uint8_update_sick(const crc_model_uint8_t *model, const uint8_t *buf, size_t len, uint8_t *value)
{
    return 0;
}

static int
crc_algorithm_uint16_update_sick(const crc_model_uint16_t *model, const uint8_t *buf, size_t len, uint16_t *value)
{
    uint8_t crc_shift = (model->width < 8) ? 8 - model->width : 0;
    uint16_t msb_mask = 1;
    msb_mask <<= (model->width - 1);
    uint16_t mask = 1;
    mask |= ((msb_mask - 1) << 1);
    uint16_t next_byte;
    uint16_t prev_byte = 0;
    uint16_t reg = *value;
    while (len--) {
        next_byte = 0x00ff & (uint16_t)*buf++;
        if ((reg & (msb_mask << crc_shift)) != 0) {
            reg <<= 1;
            reg ^= (uint16_t)(model->poly << crc_shift);
        } else {
            reg <<= 1;
        }
        reg ^= (next_byte | prev_byte);
        prev_byte = next_byte << 8;
    }
    *value = reg & mask;
    return 1;
}

static int
crc_algorithm_uint32_update_sick(const crc_model_uint32_t *model, const uint8_t *buf, size_t len, uint32_t *value)
{
    return 0;
}

static int
crc_algorithm_uint64_update_sick(const crc_model_uint64_t *model, const uint8_t *buf, size_t len, uint64_t *value)
{
    return 0;
}

/* crc_algorithm_update_table/4 */

static int
crc_algorithm_uint8_update_table(const crc_model_uint8_t *model, const uint8_t *buf, size_t len, uint8_t *value)
{
    uint8_t crc = *value;
    while (len--) {
        crc = model->table[crc ^ *buf++] & 0xff;
    }
    *value = crc & 0xff;
    return 1;
}

static int
crc_algorithm_uint16_update_table(const crc_model_uint16_t *model, const uint8_t *buf, size_t len, uint16_t *value)
{
    uint16_t crc = *value;
    while (len--) {
        crc = (model->table[(crc ^ *buf++) & 0xff] ^ (crc >> 8)) & 0xffff;
    }
    *value = crc & 0xffff;
    return 1;
}

static int
crc_algorithm_uint32_update_table(const crc_model_uint32_t *model, const uint8_t *buf, size_t len, uint32_t *value)
{
    uint32_t crc = *value;
    while (len--) {
        crc = (model->table[(crc ^ *buf++) & 0xff] ^ (crc >> 8)) & 0xffffffff;
    }
    *value = crc & 0xffffffff;
    return 1;
}

static int
crc_algorithm_uint64_update_table(const crc_model_uint64_t *model, const uint8_t *buf, size_t len, uint64_t *value)
{
    uint64_t crc = *value;
    while (len--) {
        crc = (model->table[(crc ^ *buf++) & 0xff] ^ (crc >> 8)) & 0xffffffffffffffff;
    }
    *value = crc & 0xffffffffffffffff;
    return 1;
}

/* crc_algorithm_final/2 */

static int crc_algorithm_uint8_final(const crc_model_uint8_t *model, uint8_t *value);
static int crc_algorithm_uint16_final(const crc_model_uint16_t *model, uint16_t *value);
static int crc_algorithm_uint32_final(const crc_model_uint32_t *model, uint32_t *value);
static int crc_algorithm_uint64_final(const crc_model_uint64_t *model, uint64_t *value);

int
crc_algorithm_final(const crc_model_t *model, void *value)
{
    switch (model->bits) {
    case 8:
        return crc_algorithm_uint8_final((void *)model, (uint8_t *)value);
    case 16:
        return crc_algorithm_uint16_final((void *)model, (uint16_t *)value);
    case 32:
        return crc_algorithm_uint32_final((void *)model, (uint32_t *)value);
    case 64:
        return crc_algorithm_uint64_final((void *)model, (uint64_t *)value);
    default:
        return 0;
    }
}

static int crc_algorithm_uint8_final_sick(const crc_model_uint8_t *model, uint8_t *value);
static int crc_algorithm_uint16_final_sick(const crc_model_uint16_t *model, uint16_t *value);
static int crc_algorithm_uint32_final_sick(const crc_model_uint32_t *model, uint32_t *value);
static int crc_algorithm_uint64_final_sick(const crc_model_uint64_t *model, uint64_t *value);

static int crc_algorithm_uint8_final_table(const crc_model_uint8_t *model, uint8_t *value);
static int crc_algorithm_uint16_final_table(const crc_model_uint16_t *model, uint16_t *value);
static int crc_algorithm_uint32_final_table(const crc_model_uint32_t *model, uint32_t *value);
static int crc_algorithm_uint64_final_table(const crc_model_uint64_t *model, uint64_t *value);

#define CRC_ALGORITHM_FINAL_DEF(type)                                                                                              \
    inline int crc_algorithm_##type##_final(const crc_model_##type##_t *model, type##_t *value)                                    \
    {                                                                                                                              \
        if (model->sick) {                                                                                                         \
            return crc_algorithm_##type##_final_sick(model, value);                                                                \
        } else {                                                                                                                   \
            return crc_algorithm_##type##_final_table(model, value);                                                               \
        }                                                                                                                          \
    }

CRC_ALGORITHM_FINAL_DEF(uint8)
CRC_ALGORITHM_FINAL_DEF(uint16)
CRC_ALGORITHM_FINAL_DEF(uint32)
CRC_ALGORITHM_FINAL_DEF(uint64)

#undef CRC_ALGORITHM_FINAL_DEF

/* crc_algorithm_final_sick/2 */

static int
crc_algorithm_uint8_final_sick(const crc_model_uint8_t *model, uint8_t *value)
{
    return 0;
}

static int
crc_algorithm_uint16_final_sick(const crc_model_uint16_t *model, uint16_t *value)
{
    uint16_t old_value = *value;
    uint16_t new_value;
    uint16_t low_byte;
    uint16_t high_byte;
    low_byte = (old_value & 0xFF00) >> 8;
    high_byte = (old_value & 0x00FF) << 8;
    new_value = low_byte | high_byte;
    *value = new_value;
    return 1;
}

static int
crc_algorithm_uint32_final_sick(const crc_model_uint32_t *model, uint32_t *value)
{
    return 0;
}

static int
crc_algorithm_uint64_final_sick(const crc_model_uint64_t *model, uint64_t *value)
{
    return 0;
}

/* crc_algorithm_final_table/2 */

static int
crc_algorithm_uint8_final_table(const crc_model_uint8_t *model, uint8_t *value)
{
    // if (model->refout) {
    //     *new_value = old_value ^ 0xff;
    // } else {
    //     *new_value = old_value;
    // }
    // *new_value = old_value;
    return 1;
}

static int
crc_algorithm_uint16_final_table(const crc_model_uint16_t *model, uint16_t *value)
{
    // if (model->refout) {
    //     *new_value = old_value ^ 0xffff;
    // } else {
    //     *new_value = old_value;
    // }
    // *new_value = old_value;
    return 1;
}

static int
crc_algorithm_uint32_final_table(const crc_model_uint32_t *model, uint32_t *value)
{
    // if (model->refout) {
    //     *new_value = old_value ^ 0xffffffff;
    // } else {
    //     *new_value = old_value;
    // }
    // *new_value = old_value;
    return 1;
}

static int
crc_algorithm_uint64_final_table(const crc_model_uint64_t *model, uint64_t *value)
{
    // if (model->refout) {
    //     *new_value = old_value ^ 0xffffffffffffffff;
    // } else {
    //     *new_value = old_value;
    // }
    // *new_value = old_value;
    return 1;
}
