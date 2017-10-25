// -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c++ et

#include "crc_algorithm.h"

/* crc_algorithm_init/2 */

static int crc_algorithm_uint8_init(const crc_model_uint8_t *model, bool slow, uint8_t *value);
static int crc_algorithm_uint16_init(const crc_model_uint16_t *model, bool slow, uint16_t *value);
static int crc_algorithm_uint32_init(const crc_model_uint32_t *model, bool slow, uint32_t *value);
static int crc_algorithm_uint64_init(const crc_model_uint64_t *model, bool slow, uint64_t *value);

int
crc_algorithm_init(const crc_model_t *model, bool slow, void *value)
{
#define CRC_ALGORITHM_INIT_CALL(type) return crc_algorithm_##type##_init((void *)model, slow, (type##_t *)value)

    switch (model->bits) {
    case 8:
        CRC_ALGORITHM_INIT_CALL(uint8);
        break;
    case 16:
        CRC_ALGORITHM_INIT_CALL(uint16);
        break;
    case 32:
        CRC_ALGORITHM_INIT_CALL(uint32);
        break;
    case 64:
        CRC_ALGORITHM_INIT_CALL(uint64);
        break;
    default:
        break;
    }

#undef CRC_ALGORITHM_INIT_CALL

    return 0;
}

static int crc_algorithm_uint8_init_bit_by_bit(const crc_model_uint8_t *model, uint8_t *value);
static int crc_algorithm_uint16_init_bit_by_bit(const crc_model_uint16_t *model, uint16_t *value);
static int crc_algorithm_uint32_init_bit_by_bit(const crc_model_uint32_t *model, uint32_t *value);
static int crc_algorithm_uint64_init_bit_by_bit(const crc_model_uint64_t *model, uint64_t *value);

static int crc_algorithm_uint8_init_sick(const crc_model_uint8_t *model, uint8_t *value);
static int crc_algorithm_uint16_init_sick(const crc_model_uint16_t *model, uint16_t *value);
static int crc_algorithm_uint32_init_sick(const crc_model_uint32_t *model, uint32_t *value);
static int crc_algorithm_uint64_init_sick(const crc_model_uint64_t *model, uint64_t *value);

static int crc_algorithm_uint8_init_table(const crc_model_uint8_t *model, uint8_t *value);
static int crc_algorithm_uint16_init_table(const crc_model_uint16_t *model, uint16_t *value);
static int crc_algorithm_uint32_init_table(const crc_model_uint32_t *model, uint32_t *value);
static int crc_algorithm_uint64_init_table(const crc_model_uint64_t *model, uint64_t *value);

#define CRC_ALGORITHM_INIT_DEF(type)                                                                                               \
    inline int crc_algorithm_##type##_init(const crc_model_##type##_t *model, bool slow, type##_t *value)                          \
    {                                                                                                                              \
        if (model->sick) {                                                                                                         \
            return crc_algorithm_##type##_init_sick(model, value);                                                                 \
        } else if (slow) {                                                                                                         \
            return crc_algorithm_##type##_init_bit_by_bit(model, value);                                                           \
        } else {                                                                                                                   \
            return crc_algorithm_##type##_init_table(model, value);                                                                \
        }                                                                                                                          \
    }

CRC_ALGORITHM_INIT_DEF(uint8)
CRC_ALGORITHM_INIT_DEF(uint16)
CRC_ALGORITHM_INIT_DEF(uint32)
CRC_ALGORITHM_INIT_DEF(uint64)

#undef CRC_ALGORITHM_INIT_DEF

#define CRC_ALGORITHM_INIT_BIT_BY_BIT_DEF(type)                                                                                    \
    inline int crc_algorithm_##type##_init_bit_by_bit(const crc_model_##type##_t *model, type##_t *value)                          \
    {                                                                                                                              \
        type##_t crc;                                                                                                              \
        unsigned int bit;                                                                                                          \
        size_t i;                                                                                                                  \
        crc = model->init;                                                                                                         \
        for (i = 0; i < model->width; i++) {                                                                                       \
            bit = crc & 0x01;                                                                                                      \
            if (bit) {                                                                                                             \
                crc ^= model->poly;                                                                                                \
            }                                                                                                                      \
            crc >>= 1;                                                                                                             \
            if (bit) {                                                                                                             \
                crc |= model->msb_mask;                                                                                            \
            }                                                                                                                      \
        }                                                                                                                          \
        crc = crc & model->crc_mask;                                                                                               \
        *value = crc;                                                                                                              \
        return 1;                                                                                                                  \
    }

CRC_ALGORITHM_INIT_BIT_BY_BIT_DEF(uint8)
CRC_ALGORITHM_INIT_BIT_BY_BIT_DEF(uint16)
CRC_ALGORITHM_INIT_BIT_BY_BIT_DEF(uint32)
CRC_ALGORITHM_INIT_BIT_BY_BIT_DEF(uint64)

#undef CRC_ALGORITHM_INIT_BIT_BY_BIT_DEF

#define CRC_ALGORITHM_INIT_SICK_DEF(type)                                                                                          \
    inline int crc_algorithm_##type##_init_sick(const crc_model_##type##_t *model, type##_t *value)                                \
    {                                                                                                                              \
        type##_t crc;                                                                                                              \
        crc = model->init;                                                                                                         \
        *value = crc;                                                                                                              \
        return 1;                                                                                                                  \
    }

CRC_ALGORITHM_INIT_SICK_DEF(uint8)
CRC_ALGORITHM_INIT_SICK_DEF(uint16)
CRC_ALGORITHM_INIT_SICK_DEF(uint32)
CRC_ALGORITHM_INIT_SICK_DEF(uint64)

#undef CRC_ALGORITHM_INIT_SICK_DEF

#define CRC_ALGORITHM_INIT_TABLE_DEF(type)                                                                                         \
    inline int crc_algorithm_##type##_init_table(const crc_model_##type##_t *model, type##_t *value)                               \
    {                                                                                                                              \
        type##_t crc;                                                                                                              \
        crc = model->init;                                                                                                         \
        *value = crc;                                                                                                              \
        return 1;                                                                                                                  \
    }

CRC_ALGORITHM_INIT_TABLE_DEF(uint8)
CRC_ALGORITHM_INIT_TABLE_DEF(uint16)
CRC_ALGORITHM_INIT_TABLE_DEF(uint32)
CRC_ALGORITHM_INIT_TABLE_DEF(uint64)

#undef CRC_ALGORITHM_INIT_TABLE_DEF

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
            reg <<= (model->width - tbl_idx_width + model->crc_shift);                                                             \
            for (j = 0; j < tbl_idx_width; j++) {                                                                                  \
                if ((reg & (model->msb_mask << model->crc_shift)) != 0) {                                                          \
                    reg <<= 1;                                                                                                     \
                    reg ^= (type##_t)(model->poly << model->crc_shift);                                                            \
                } else {                                                                                                           \
                    reg <<= 1;                                                                                                     \
                }                                                                                                                  \
            }                                                                                                                      \
            if (model->refin) {                                                                                                    \
                reg = crc_algorithm_##type##_reflect((type##_t)(reg >> model->crc_shift), model->width);                           \
                reg <<= model->crc_shift;                                                                                          \
            }                                                                                                                      \
            model->table[i] = (type##_t)(reg >> model->crc_shift) & model->crc_mask;                                               \
        }                                                                                                                          \
        return 1;                                                                                                                  \
    }

CRC_ALGORITHM_COMPILE_DEF(uint8)
CRC_ALGORITHM_COMPILE_DEF(uint16)
CRC_ALGORITHM_COMPILE_DEF(uint32)
CRC_ALGORITHM_COMPILE_DEF(uint64)

#undef CRC_ALGORITHM_COMPILE_DEF

/* crc_algorithm_update/4 */

static int crc_algorithm_uint8_update(const crc_model_uint8_t *model, bool slow, const uint8_t *buf, size_t len, uint8_t *value);
static int crc_algorithm_uint16_update(const crc_model_uint16_t *model, bool slow, const uint8_t *buf, size_t len, uint16_t *value);
static int crc_algorithm_uint32_update(const crc_model_uint32_t *model, bool slow, const uint8_t *buf, size_t len, uint32_t *value);
static int crc_algorithm_uint64_update(const crc_model_uint64_t *model, bool slow, const uint8_t *buf, size_t len, uint64_t *value);

int
crc_algorithm_update(const crc_model_t *model, bool slow, const uint8_t *buf, size_t len, void *value)
{
    if (len == 0) {
        return 1;
    }
    switch (model->bits) {
    case 8:
        return crc_algorithm_uint8_update((void *)model, slow, buf, len, (uint8_t *)value);
    case 16:
        return crc_algorithm_uint16_update((void *)model, slow, buf, len, (uint16_t *)value);
    case 32:
        return crc_algorithm_uint32_update((void *)model, slow, buf, len, (uint32_t *)value);
    case 64:
        return crc_algorithm_uint64_update((void *)model, slow, buf, len, (uint64_t *)value);
    default:
        return 0;
    }
}

static int crc_algorithm_uint8_update_bit_by_bit(const crc_model_uint8_t *model, const uint8_t *buf, size_t len, uint8_t *value);
static int crc_algorithm_uint16_update_bit_by_bit(const crc_model_uint16_t *model, const uint8_t *buf, size_t len, uint16_t *value);
static int crc_algorithm_uint32_update_bit_by_bit(const crc_model_uint32_t *model, const uint8_t *buf, size_t len, uint32_t *value);
static int crc_algorithm_uint64_update_bit_by_bit(const crc_model_uint64_t *model, const uint8_t *buf, size_t len, uint64_t *value);

static int crc_algorithm_uint8_update_sick(const crc_model_uint8_t *model, const uint8_t *buf, size_t len, uint8_t *value);
static int crc_algorithm_uint16_update_sick(const crc_model_uint16_t *model, const uint8_t *buf, size_t len, uint16_t *value);
static int crc_algorithm_uint32_update_sick(const crc_model_uint32_t *model, const uint8_t *buf, size_t len, uint32_t *value);
static int crc_algorithm_uint64_update_sick(const crc_model_uint64_t *model, const uint8_t *buf, size_t len, uint64_t *value);

static int crc_algorithm_uint8_update_table(const crc_model_uint8_t *model, const uint8_t *buf, size_t len, uint8_t *value);
static int crc_algorithm_uint16_update_table(const crc_model_uint16_t *model, const uint8_t *buf, size_t len, uint16_t *value);
static int crc_algorithm_uint32_update_table(const crc_model_uint32_t *model, const uint8_t *buf, size_t len, uint32_t *value);
static int crc_algorithm_uint64_update_table(const crc_model_uint64_t *model, const uint8_t *buf, size_t len, uint64_t *value);

#define CRC_ALGORITHM_UPDATE_DEF(type)                                                                                             \
    inline int crc_algorithm_##type##_update(const crc_model_##type##_t *model, bool slow, const uint8_t *buf, size_t len,         \
                                             type##_t *value)                                                                      \
    {                                                                                                                              \
        if (model->sick) {                                                                                                         \
            return crc_algorithm_##type##_update_sick(model, buf, len, value);                                                     \
        } else if (slow) {                                                                                                         \
            return crc_algorithm_##type##_update_bit_by_bit(model, buf, len, value);                                               \
        } else {                                                                                                                   \
            return crc_algorithm_##type##_update_table(model, buf, len, value);                                                    \
        }                                                                                                                          \
    }

CRC_ALGORITHM_UPDATE_DEF(uint8)
CRC_ALGORITHM_UPDATE_DEF(uint16)
CRC_ALGORITHM_UPDATE_DEF(uint32)
CRC_ALGORITHM_UPDATE_DEF(uint64)

#undef CRC_ALGORITHM_UPDATE_DEF

/* crc_algorithm_update_bit_by_bit/4 */

#define CRC_ALGORITHM_UPDATE_BIT_BY_BIT_DEF(type)                                                                                  \
    inline int crc_algorithm_##type##_update_bit_by_bit(const crc_model_##type##_t *model, const uint8_t *buf, size_t len,         \
                                                        type##_t *value)                                                           \
    {                                                                                                                              \
        type##_t reg = *value;                                                                                                     \
        type##_t topbit;                                                                                                           \
        type##_t octet;                                                                                                            \
        size_t i;                                                                                                                  \
        /* XNIF_TRACE_F("[0] reg   = 0x%04x\n", reg); */                                                                           \
        while (len--) {                                                                                                            \
            octet = *buf++;                                                                                                        \
            /* XNIF_TRACE_F("[0] octet = 0x%02x\n", octet); */                                                                     \
            if (model->refin) {                                                                                                    \
                octet = crc_algorithm_##type##_reflect(octet, 8);                                                                  \
                /* XNIF_TRACE_F("[0] octet = 0x%02x (reflected)\n", octet); */                                                     \
            }                                                                                                                      \
            for (i = 0; i < 8; i++) {                                                                                              \
                topbit = reg & model->msb_mask;                                                                                    \
                /* XNIF_TRACE_F("[%d] topbit = %d\n", i, topbit); */                                                               \
                /* XNIF_TRACE_F("[%d] ((0x%04x << 1) & 0x%04x) | ((0x%02x >> %d) & 0x01)\n", i, reg, model->crc_mask, octet, */    \
                /*             (7 - i)); */                                                                                        \
                reg = ((reg << 1) & model->crc_mask) | ((octet >> (7 - i)) & 0x01);                                                \
                /* reg = (reg << 1) & model->crc_mask; */                                                                          \
                /* reg |= (type##_t)((octet >> (7 - i)) & 0x01); */                                                                \
                /* XNIF_TRACE_F("[%d] reg   = 0x%04x\n", i, reg); */                                                               \
                if (topbit) {                                                                                                      \
                    reg ^= model->poly;                                                                                            \
                }                                                                                                                  \
            }                                                                                                                      \
        }                                                                                                                          \
        *value = reg;                                                                                                              \
        return 1;                                                                                                                  \
    }

CRC_ALGORITHM_UPDATE_BIT_BY_BIT_DEF(uint8)
CRC_ALGORITHM_UPDATE_BIT_BY_BIT_DEF(uint16)
CRC_ALGORITHM_UPDATE_BIT_BY_BIT_DEF(uint32)
CRC_ALGORITHM_UPDATE_BIT_BY_BIT_DEF(uint64)

#undef CRC_ALGORITHM_UPDATE_BIT_BY_BIT_DEF

/* crc_algorithm_update_sick/4 */

inline int
crc_algorithm_uint8_update_sick(const crc_model_uint8_t *model, const uint8_t *buf, size_t len, uint8_t *value)
{
    return 0;
}

inline int
crc_algorithm_uint16_update_sick(const crc_model_uint16_t *model, const uint8_t *buf, size_t len, uint16_t *value)
{
    uint16_t next_byte;
    uint16_t prev_byte = 0;
    uint16_t reg = *value;
    while (len--) {
        next_byte = 0x00ff & (uint16_t)*buf++;
        if ((reg & (model->msb_mask << model->crc_shift)) != 0) {
            reg <<= 1;
            reg ^= (uint16_t)(model->poly << model->crc_shift);
        } else {
            reg <<= 1;
        }
        reg ^= (next_byte | prev_byte);
        prev_byte = next_byte << 8;
    }
    *value = reg & model->crc_mask;
    return 1;
}

inline int
crc_algorithm_uint32_update_sick(const crc_model_uint32_t *model, const uint8_t *buf, size_t len, uint32_t *value)
{
    return 0;
}

inline int
crc_algorithm_uint64_update_sick(const crc_model_uint64_t *model, const uint8_t *buf, size_t len, uint64_t *value)
{
    return 0;
}

/* crc_algorithm_update_table/4 */

#define CRC_ALGORITHM_UPDATE_TABLE_DEF(type)                                                                                       \
    inline int crc_algorithm_##type##_update_table(const crc_model_##type##_t *model, const uint8_t *buf, size_t len,              \
                                                   type##_t *value)                                                                \
    {                                                                                                                              \
        type##_t crc = *value;                                                                                                     \
        unsigned int tbl_index;                                                                                                    \
        if (model->refin) {                                                                                                        \
            crc = crc_algorithm_##type##_reflect(crc, model->width);                                                               \
            while (len--) {                                                                                                        \
                tbl_index = (crc ^ *buf++) & 0xff;                                                                                 \
                crc = ((crc >> 8) ^ model->table[tbl_index]) & model->crc_mask;                                                    \
            }                                                                                                                      \
            crc = crc_algorithm_##type##_reflect(crc, model->width) & model->crc_mask;                                             \
        } else {                                                                                                                   \
            crc = crc << model->crc_shift;                                                                                         \
            while (len--) {                                                                                                        \
                tbl_index = ((crc >> (model->width - 8 + model->crc_shift)) ^ *buf++) & 0xff;                                      \
                crc = ((crc << (8 - model->crc_shift)) ^ (model->table[tbl_index] << model->crc_shift)) &                          \
                      (model->crc_mask << model->crc_shift);                                                                       \
            }                                                                                                                      \
            crc = crc >> model->crc_shift;                                                                                         \
        }                                                                                                                          \
        *value = crc;                                                                                                              \
        return 1;                                                                                                                  \
    }

CRC_ALGORITHM_UPDATE_TABLE_DEF(uint8)
CRC_ALGORITHM_UPDATE_TABLE_DEF(uint16)
CRC_ALGORITHM_UPDATE_TABLE_DEF(uint32)
CRC_ALGORITHM_UPDATE_TABLE_DEF(uint64)

#undef CRC_ALGORITHM_UPDATE_TABLE_DEF

/* crc_algorithm_final/2 */

static int crc_algorithm_uint8_final(const crc_model_uint8_t *model, bool slow, uint8_t *value);
static int crc_algorithm_uint16_final(const crc_model_uint16_t *model, bool slow, uint16_t *value);
static int crc_algorithm_uint32_final(const crc_model_uint32_t *model, bool slow, uint32_t *value);
static int crc_algorithm_uint64_final(const crc_model_uint64_t *model, bool slow, uint64_t *value);

int
crc_algorithm_final(const crc_model_t *model, bool slow, void *value)
{
    switch (model->bits) {
    case 8:
        return crc_algorithm_uint8_final((void *)model, slow, (uint8_t *)value);
    case 16:
        return crc_algorithm_uint16_final((void *)model, slow, (uint16_t *)value);
    case 32:
        return crc_algorithm_uint32_final((void *)model, slow, (uint32_t *)value);
    case 64:
        return crc_algorithm_uint64_final((void *)model, slow, (uint64_t *)value);
    default:
        return 0;
    }
}

static int crc_algorithm_uint8_final_bit_by_bit(const crc_model_uint8_t *model, uint8_t *value);
static int crc_algorithm_uint16_final_bit_by_bit(const crc_model_uint16_t *model, uint16_t *value);
static int crc_algorithm_uint32_final_bit_by_bit(const crc_model_uint32_t *model, uint32_t *value);
static int crc_algorithm_uint64_final_bit_by_bit(const crc_model_uint64_t *model, uint64_t *value);

static int crc_algorithm_uint8_final_sick(const crc_model_uint8_t *model, uint8_t *value);
static int crc_algorithm_uint16_final_sick(const crc_model_uint16_t *model, uint16_t *value);
static int crc_algorithm_uint32_final_sick(const crc_model_uint32_t *model, uint32_t *value);
static int crc_algorithm_uint64_final_sick(const crc_model_uint64_t *model, uint64_t *value);

static int crc_algorithm_uint8_final_table(const crc_model_uint8_t *model, uint8_t *value);
static int crc_algorithm_uint16_final_table(const crc_model_uint16_t *model, uint16_t *value);
static int crc_algorithm_uint32_final_table(const crc_model_uint32_t *model, uint32_t *value);
static int crc_algorithm_uint64_final_table(const crc_model_uint64_t *model, uint64_t *value);

#define CRC_ALGORITHM_FINAL_DEF(type)                                                                                              \
    inline int crc_algorithm_##type##_final(const crc_model_##type##_t *model, bool slow, type##_t *value)                         \
    {                                                                                                                              \
        if (model->sick) {                                                                                                         \
            return crc_algorithm_##type##_final_sick(model, value);                                                                \
        } else if (slow) {                                                                                                         \
            int retval = crc_algorithm_##type##_final_bit_by_bit(model, value);                                                    \
            if (!retval) {                                                                                                         \
                return retval;                                                                                                     \
            }                                                                                                                      \
            if (model->refout) {                                                                                                   \
                *value = crc_algorithm_##type##_reflect(*value, model->width);                                                     \
            }                                                                                                                      \
            *value = (*value ^ model->xorout) & model->crc_mask;                                                                   \
            return retval;                                                                                                         \
        } else {                                                                                                                   \
            return crc_algorithm_##type##_final_table(model, value);                                                               \
        }                                                                                                                          \
    }

CRC_ALGORITHM_FINAL_DEF(uint8)
CRC_ALGORITHM_FINAL_DEF(uint16)
CRC_ALGORITHM_FINAL_DEF(uint32)
CRC_ALGORITHM_FINAL_DEF(uint64)

#undef CRC_ALGORITHM_FINAL_DEF

/* crc_algorithm_final_bit_by_bit/2 */

#define CRC_ALGORITHM_FINAL_BIT_BY_BIT_DEF(type)                                                                                   \
    inline int crc_algorithm_##type##_final_bit_by_bit(const crc_model_##type##_t *model, type##_t *value)                         \
    {                                                                                                                              \
        type##_t reg = *value;                                                                                                     \
        type##_t topbit;                                                                                                           \
        size_t i;                                                                                                                  \
        for (i = 0; i < model->width; i++) {                                                                                       \
            /* XNIF_TRACE_F("reg & model->msb_mask = 0x%02x & 0x%02x = 0x%02x\n", reg, model->msb_mask, reg & model->msb_mask); */ \
            topbit = reg & model->msb_mask;                                                                                        \
            reg = ((reg << 1) & model->crc_mask);                                                                                  \
            if (topbit) {                                                                                                          \
                reg ^= model->poly;                                                                                                \
            }                                                                                                                      \
        }                                                                                                                          \
        *value = reg;                                                                                                              \
        return 1;                                                                                                                  \
    }

CRC_ALGORITHM_FINAL_BIT_BY_BIT_DEF(uint8)
CRC_ALGORITHM_FINAL_BIT_BY_BIT_DEF(uint16)
CRC_ALGORITHM_FINAL_BIT_BY_BIT_DEF(uint32)
CRC_ALGORITHM_FINAL_BIT_BY_BIT_DEF(uint64)

#undef CRC_ALGORITHM_FINAL_BIT_BY_BIT_DEF

/* crc_algorithm_final_sick/2 */

inline int
crc_algorithm_uint8_final_sick(const crc_model_uint8_t *model, uint8_t *value)
{
    return 0;
}

inline int
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

inline int
crc_algorithm_uint32_final_sick(const crc_model_uint32_t *model, uint32_t *value)
{
    return 0;
}

inline int
crc_algorithm_uint64_final_sick(const crc_model_uint64_t *model, uint64_t *value)
{
    return 0;
}

/* crc_algorithm_final_table/2 */

#define CRC_ALGORITHM_FINAL_TABLE_DEF(type)                                                                                        \
    inline int crc_algorithm_##type##_final_table(const crc_model_##type##_t *model, type##_t *value)                              \
    {                                                                                                                              \
        type##_t crc = *value;                                                                                                     \
        if (model->refout) {                                                                                                       \
            crc = crc_algorithm_##type##_reflect(crc, model->width);                                                               \
        }                                                                                                                          \
        *value = crc ^ model->xorout;                                                                                              \
        return 1;                                                                                                                  \
    }

CRC_ALGORITHM_FINAL_TABLE_DEF(uint8)
CRC_ALGORITHM_FINAL_TABLE_DEF(uint16)
CRC_ALGORITHM_FINAL_TABLE_DEF(uint32)
CRC_ALGORITHM_FINAL_TABLE_DEF(uint64)

#undef CRC_ALGORITHM_FINAL_TABLE_DEF

/* crc_algorithm_residue/2 */

static int crc_algorithm_uint8_residue(const crc_model_uint8_t *model, uint8_t *value);
static int crc_algorithm_uint16_residue(const crc_model_uint16_t *model, uint16_t *value);
static int crc_algorithm_uint32_residue(const crc_model_uint32_t *model, uint32_t *value);
static int crc_algorithm_uint64_residue(const crc_model_uint64_t *model, uint64_t *value);

int
crc_algorithm_residue(const crc_model_t *model, void *value)
{
    switch (model->bits) {
    case 8:
        return crc_algorithm_uint8_residue((void *)model, (uint8_t *)value);
    case 16:
        return crc_algorithm_uint16_residue((void *)model, (uint16_t *)value);
    case 32:
        return crc_algorithm_uint32_residue((void *)model, (uint32_t *)value);
    case 64:
        return crc_algorithm_uint64_residue((void *)model, (uint64_t *)value);
    default:
        return 0;
    }
}

#define CRC_ALGORITHM_RESIDUE_DEF(type)                                                                                            \
    inline int crc_algorithm_##type##_residue(const crc_model_##type##_t *model, type##_t *value)                                  \
    {                                                                                                                              \
        int retval;                                                                                                                \
        type##_t xorout = model->xorout;                                                                                           \
        type##_t residue = 0;                                                                                                      \
        /* XNIF_TRACE_F("xorout  = 0x%04x\n", xorout); */                                                                          \
        if (model->refout) {                                                                                                       \
            xorout = crc_algorithm_##type##_reflect(xorout, model->width);                                                         \
        }                                                                                                                          \
        /* NOTE: I have no idea why this is necessary for 5 bits... */                                                             \
        if (model->width == 5) {                                                                                                   \
            xorout = (xorout << model->crc_shift);                                                                                 \
        }                                                                                                                          \
        /* XNIF_TRACE_F("xorout  = 0x%04x (reversed)\n", xorout); */                                                               \
        /* XNIF_TRACE_F("residue = 0x%04x\n", residue); */                                                                         \
        size_t len = (model->super.bits / 8);                                                                                      \
        /* XNIF_TRACE_F("len     = %lu\n", len); */                                                                                \
        uint8_t *buf = (void *)&xorout;                                                                                            \
        if (model->sick) {                                                                                                         \
            retval = crc_algorithm_##type##_update_sick(model, buf, len, &residue);                                                \
        } else {                                                                                                                   \
            retval = crc_algorithm_##type##_update_bit_by_bit(model, buf, len, &residue);                                          \
        }                                                                                                                          \
        /* XNIF_TRACE_F("residue = 0x%04x\n", residue); */                                                                         \
        if (!retval) {                                                                                                             \
            return retval;                                                                                                         \
        }                                                                                                                          \
        if (model->sick) {                                                                                                         \
            retval = crc_algorithm_##type##_final_sick(model, &residue);                                                           \
        } else {                                                                                                                   \
            retval = crc_algorithm_##type##_final_bit_by_bit(model, &residue);                                                     \
        }                                                                                                                          \
        if (!retval) {                                                                                                             \
            return retval;                                                                                                         \
        }                                                                                                                          \
        /* XNIF_TRACE_F("residue = 0x%04x (final)\n", residue); */                                                                 \
        if (model->refin) {                                                                                                        \
            residue = crc_algorithm_##type##_reflect(residue, model->width);                                                       \
        }                                                                                                                          \
        /* XNIF_TRACE_F("residue = 0x%04x (reversed)\n", residue); */                                                              \
        *value = residue;                                                                                                          \
        return 1;                                                                                                                  \
    }

CRC_ALGORITHM_RESIDUE_DEF(uint8)
CRC_ALGORITHM_RESIDUE_DEF(uint16)
CRC_ALGORITHM_RESIDUE_DEF(uint32)
CRC_ALGORITHM_RESIDUE_DEF(uint64)

#undef CRC_ALGORITHM_RESIDUE_DEF
