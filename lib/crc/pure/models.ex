defmodule CRC.Pure.Models do
  @moduledoc false

  import Bitwise

  # All CRC model definitions, extracted from the C NIF model tables.
  # Each root model maps to its parameters. Aliases are stored in a separate map.

  @models %{
    # 3-bit
    crc_3_gsm: %{
      width: 3,
      poly: 0x03,
      init: 0x00,
      refin: false,
      refout: false,
      xorout: 0x07,
      check: 0x04,
      residue: 0x02,
      sick: false,
      name: "CRC-3/GSM"
    },
    crc_3_rohc: %{
      width: 3,
      poly: 0x03,
      init: 0x07,
      refin: true,
      refout: true,
      xorout: 0x00,
      check: 0x06,
      residue: 0x00,
      sick: false,
      name: "CRC-3/ROHC"
    },
    # 4-bit
    crc_4_interlaken: %{
      width: 4,
      poly: 0x03,
      init: 0x0F,
      refin: false,
      refout: false,
      xorout: 0x0F,
      check: 0x0B,
      residue: 0x02,
      sick: false,
      name: "CRC-4/INTERLAKEN"
    },
    crc_4_itu: %{
      width: 4,
      poly: 0x03,
      init: 0x00,
      refin: true,
      refout: true,
      xorout: 0x00,
      check: 0x07,
      residue: 0x00,
      sick: false,
      name: "CRC-4/ITU"
    },
    # 5-bit
    crc_5_epc: %{
      width: 5,
      poly: 0x09,
      init: 0x09,
      refin: false,
      refout: false,
      xorout: 0x00,
      check: 0x00,
      residue: 0x00,
      sick: false,
      name: "CRC-5/EPC"
    },
    crc_5_itu: %{
      width: 5,
      poly: 0x15,
      init: 0x00,
      refin: true,
      refout: true,
      xorout: 0x00,
      check: 0x07,
      residue: 0x00,
      sick: false,
      name: "CRC-5/ITU"
    },
    crc_5_usb: %{
      width: 5,
      poly: 0x05,
      init: 0x1F,
      refin: true,
      refout: true,
      xorout: 0x1F,
      check: 0x19,
      residue: 0x06,
      sick: false,
      name: "CRC-5/USB"
    },
    # 6-bit
    crc_6_cdma2000_a: %{
      width: 6,
      poly: 0x27,
      init: 0x3F,
      refin: false,
      refout: false,
      xorout: 0x00,
      check: 0x0D,
      residue: 0x00,
      sick: false,
      name: "CRC-6/CDMA2000-A"
    },
    crc_6_cdma2000_b: %{
      width: 6,
      poly: 0x07,
      init: 0x3F,
      refin: false,
      refout: false,
      xorout: 0x00,
      check: 0x3B,
      residue: 0x00,
      sick: false,
      name: "CRC-6/CDMA2000-B"
    },
    crc_6_darc: %{
      width: 6,
      poly: 0x19,
      init: 0x00,
      refin: true,
      refout: true,
      xorout: 0x00,
      check: 0x26,
      residue: 0x00,
      sick: false,
      name: "CRC-6/DARC"
    },
    crc_6_gsm: %{
      width: 6,
      poly: 0x2F,
      init: 0x00,
      refin: false,
      refout: false,
      xorout: 0x3F,
      check: 0x13,
      residue: 0x3A,
      sick: false,
      name: "CRC-6/GSM"
    },
    crc_6_itu: %{
      width: 6,
      poly: 0x03,
      init: 0x00,
      refin: true,
      refout: true,
      xorout: 0x00,
      check: 0x06,
      residue: 0x00,
      sick: false,
      name: "CRC-6/ITU"
    },
    # 7-bit
    crc_7: %{
      width: 7,
      poly: 0x09,
      init: 0x00,
      refin: false,
      refout: false,
      xorout: 0x00,
      check: 0x75,
      residue: 0x00,
      sick: false,
      name: "CRC-7"
    },
    crc_7_rohc: %{
      width: 7,
      poly: 0x4F,
      init: 0x7F,
      refin: true,
      refout: true,
      xorout: 0x00,
      check: 0x53,
      residue: 0x00,
      sick: false,
      name: "CRC-7/ROHC"
    },
    crc_7_umts: %{
      width: 7,
      poly: 0x45,
      init: 0x00,
      refin: false,
      refout: false,
      xorout: 0x00,
      check: 0x61,
      residue: 0x00,
      sick: false,
      name: "CRC-7/UMTS"
    },
    # 8-bit
    crc_8: %{
      width: 8,
      poly: 0x07,
      init: 0x00,
      refin: false,
      refout: false,
      xorout: 0x00,
      check: 0xF4,
      residue: 0x00,
      sick: false,
      name: "CRC-8"
    },
    crc_8_autosar: %{
      width: 8,
      poly: 0x2F,
      init: 0xFF,
      refin: false,
      refout: false,
      xorout: 0xFF,
      check: 0xDF,
      residue: 0x42,
      sick: false,
      name: "CRC-8/AUTOSAR"
    },
    crc_8_bluetooth: %{
      width: 8,
      poly: 0xA7,
      init: 0x00,
      refin: true,
      refout: true,
      xorout: 0x00,
      check: 0x26,
      residue: 0x00,
      sick: false,
      name: "CRC-8/BLUETOOTH"
    },
    crc_8_cdma2000: %{
      width: 8,
      poly: 0x9B,
      init: 0xFF,
      refin: false,
      refout: false,
      xorout: 0x00,
      check: 0xDA,
      residue: 0x00,
      sick: false,
      name: "CRC-8/CDMA2000"
    },
    crc_8_darc: %{
      width: 8,
      poly: 0x39,
      init: 0x00,
      refin: true,
      refout: true,
      xorout: 0x00,
      check: 0x15,
      residue: 0x00,
      sick: false,
      name: "CRC-8/DARC"
    },
    crc_8_dvb_s2: %{
      width: 8,
      poly: 0xD5,
      init: 0x00,
      refin: false,
      refout: false,
      xorout: 0x00,
      check: 0xBC,
      residue: 0x00,
      sick: false,
      name: "CRC-8/DVB-S2"
    },
    crc_8_ebu: %{
      width: 8,
      poly: 0x1D,
      init: 0xFF,
      refin: true,
      refout: true,
      xorout: 0x00,
      check: 0x97,
      residue: 0x00,
      sick: false,
      name: "CRC-8/EBU"
    },
    crc_8_gsm_a: %{
      width: 8,
      poly: 0x1D,
      init: 0x00,
      refin: false,
      refout: false,
      xorout: 0x00,
      check: 0x37,
      residue: 0x00,
      sick: false,
      name: "CRC-8/GSM-A"
    },
    crc_8_gsm_b: %{
      width: 8,
      poly: 0x49,
      init: 0x00,
      refin: false,
      refout: false,
      xorout: 0xFF,
      check: 0x94,
      residue: 0x53,
      sick: false,
      name: "CRC-8/GSM-B"
    },
    crc_8_i_code: %{
      width: 8,
      poly: 0x1D,
      init: 0xFD,
      refin: false,
      refout: false,
      xorout: 0x00,
      check: 0x7E,
      residue: 0x00,
      sick: false,
      name: "CRC-8/I-CODE"
    },
    crc_8_itu: %{
      width: 8,
      poly: 0x07,
      init: 0x00,
      refin: false,
      refout: false,
      xorout: 0x55,
      check: 0xA1,
      residue: 0xAC,
      sick: false,
      name: "CRC-8/ITU"
    },
    crc_8_koop: %{
      width: 8,
      poly: 0x4D,
      init: 0xFF,
      refin: true,
      refout: true,
      xorout: 0xFF,
      check: 0xD8,
      residue: 0x15,
      sick: false,
      name: "CRC-8/KOOP"
    },
    crc_8_lte: %{
      width: 8,
      poly: 0x9B,
      init: 0x00,
      refin: false,
      refout: false,
      xorout: 0x00,
      check: 0xEA,
      residue: 0x00,
      sick: false,
      name: "CRC-8/LTE"
    },
    crc_8_maxim: %{
      width: 8,
      poly: 0x31,
      init: 0x00,
      refin: true,
      refout: true,
      xorout: 0x00,
      check: 0xA1,
      residue: 0x00,
      sick: false,
      name: "CRC-8/MAXIM"
    },
    crc_8_opensafety: %{
      width: 8,
      poly: 0x2F,
      init: 0x00,
      refin: false,
      refout: false,
      xorout: 0x00,
      check: 0x3E,
      residue: 0x00,
      sick: false,
      name: "CRC-8/OPENSAFETY"
    },
    crc_8_rohc: %{
      width: 8,
      poly: 0x07,
      init: 0xFF,
      refin: true,
      refout: true,
      xorout: 0x00,
      check: 0xD0,
      residue: 0x00,
      sick: false,
      name: "CRC-8/ROHC"
    },
    crc_8_sae_j1850: %{
      width: 8,
      poly: 0x1D,
      init: 0xFF,
      refin: false,
      refout: false,
      xorout: 0xFF,
      check: 0x4B,
      residue: 0xC4,
      sick: false,
      name: "CRC-8/SAE-J1850"
    },
    crc_8_wcdma: %{
      width: 8,
      poly: 0x9B,
      init: 0x00,
      refin: true,
      refout: true,
      xorout: 0x00,
      check: 0x25,
      residue: 0x00,
      sick: false,
      name: "CRC-8/WCDMA"
    },
    # 10-bit
    crc_10: %{
      width: 10,
      poly: 0x0233,
      init: 0x0000,
      refin: false,
      refout: false,
      xorout: 0x0000,
      check: 0x0199,
      residue: 0x0000,
      sick: false,
      name: "CRC-10"
    },
    crc_10_cdma2000: %{
      width: 10,
      poly: 0x03D9,
      init: 0x03FF,
      refin: false,
      refout: false,
      xorout: 0x0000,
      check: 0x0233,
      residue: 0x0000,
      sick: false,
      name: "CRC-10/CDMA2000"
    },
    crc_10_gsm: %{
      width: 10,
      poly: 0x0175,
      init: 0x0000,
      refin: false,
      refout: false,
      xorout: 0x03FF,
      check: 0x012A,
      residue: 0x00C6,
      sick: false,
      name: "CRC-10/GSM"
    },
    # 11-bit
    crc_11: %{
      width: 11,
      poly: 0x0385,
      init: 0x001A,
      refin: false,
      refout: false,
      xorout: 0x0000,
      check: 0x05A3,
      residue: 0x0000,
      sick: false,
      name: "CRC-11"
    },
    crc_11_umts: %{
      width: 11,
      poly: 0x0307,
      init: 0x0000,
      refin: false,
      refout: false,
      xorout: 0x0000,
      check: 0x0061,
      residue: 0x0000,
      sick: false,
      name: "CRC-11/UMTS"
    },
    # 12-bit
    crc_12_cdma2000: %{
      width: 12,
      poly: 0x0F13,
      init: 0x0FFF,
      refin: false,
      refout: false,
      xorout: 0x0000,
      check: 0x0D4D,
      residue: 0x0000,
      sick: false,
      name: "CRC-12/CDMA2000"
    },
    crc_12_dect: %{
      width: 12,
      poly: 0x080F,
      init: 0x0000,
      refin: false,
      refout: false,
      xorout: 0x0000,
      check: 0x0F5B,
      residue: 0x0000,
      sick: false,
      name: "CRC-12/DECT"
    },
    crc_12_gsm: %{
      width: 12,
      poly: 0x0D31,
      init: 0x0000,
      refin: false,
      refout: false,
      xorout: 0x0FFF,
      check: 0x0B34,
      residue: 0x0178,
      sick: false,
      name: "CRC-12/GSM"
    },
    crc_12_umts: %{
      width: 12,
      poly: 0x080F,
      init: 0x0000,
      refin: false,
      refout: true,
      xorout: 0x0000,
      check: 0x0DAF,
      residue: 0x0000,
      sick: false,
      name: "CRC-12/UMTS"
    },
    # 13-bit
    crc_13_bbc: %{
      width: 13,
      poly: 0x1CF5,
      init: 0x0000,
      refin: false,
      refout: false,
      xorout: 0x0000,
      check: 0x04FA,
      residue: 0x0000,
      sick: false,
      name: "CRC-13/BBC"
    },
    # 14-bit
    crc_14_darc: %{
      width: 14,
      poly: 0x0805,
      init: 0x0000,
      refin: true,
      refout: true,
      xorout: 0x0000,
      check: 0x082D,
      residue: 0x0000,
      sick: false,
      name: "CRC-14/DARC"
    },
    crc_14_gsm: %{
      width: 14,
      poly: 0x202D,
      init: 0x0000,
      refin: false,
      refout: false,
      xorout: 0x3FFF,
      check: 0x30AE,
      residue: 0x031E,
      sick: false,
      name: "CRC-14/GSM"
    },
    # 15-bit
    crc_15: %{
      width: 15,
      poly: 0x4599,
      init: 0x0000,
      refin: false,
      refout: false,
      xorout: 0x0000,
      check: 0x059E,
      residue: 0x0000,
      sick: false,
      name: "CRC-15"
    },
    crc_15_mpt1327: %{
      width: 15,
      poly: 0x6815,
      init: 0x0000,
      refin: false,
      refout: false,
      xorout: 0x0001,
      check: 0x2566,
      residue: 0x6815,
      sick: false,
      name: "CRC-15/MPT1327"
    },
    # 16-bit
    crc_16: %{
      width: 16,
      poly: 0x8005,
      init: 0x0000,
      refin: true,
      refout: true,
      xorout: 0x0000,
      check: 0xBB3D,
      residue: 0x0000,
      sick: false,
      name: "CRC-16"
    },
    crc_16_a: %{
      width: 16,
      poly: 0x1021,
      init: 0xC6C6,
      refin: true,
      refout: true,
      xorout: 0x0000,
      check: 0xBF05,
      residue: 0x0000,
      sick: false,
      name: "CRC-16/A"
    },
    crc_16_aug_ccitt: %{
      width: 16,
      poly: 0x1021,
      init: 0x1D0F,
      refin: false,
      refout: false,
      xorout: 0x0000,
      check: 0xE5CC,
      residue: 0x0000,
      sick: false,
      name: "CRC-16/AUG-CCITT"
    },
    crc_16_buypass: %{
      width: 16,
      poly: 0x8005,
      init: 0x0000,
      refin: false,
      refout: false,
      xorout: 0x0000,
      check: 0xFEE8,
      residue: 0x0000,
      sick: false,
      name: "CRC-16/BUYPASS"
    },
    crc_16_ccitt: %{
      width: 16,
      poly: 0x1021,
      init: 0x0000,
      refin: true,
      refout: true,
      xorout: 0x0000,
      check: 0x2189,
      residue: 0x0000,
      sick: false,
      name: "CRC-16/CCITT"
    },
    crc_16_ccitt_false: %{
      width: 16,
      poly: 0x1021,
      init: 0xFFFF,
      refin: false,
      refout: false,
      xorout: 0x0000,
      check: 0x29B1,
      residue: 0x0000,
      sick: false,
      name: "CRC-16/CCITT-FALSE"
    },
    crc_16_cdma2000: %{
      width: 16,
      poly: 0xC867,
      init: 0xFFFF,
      refin: false,
      refout: false,
      xorout: 0x0000,
      check: 0x4C06,
      residue: 0x0000,
      sick: false,
      name: "CRC-16/CDMA2000"
    },
    crc_16_cms: %{
      width: 16,
      poly: 0x8005,
      init: 0xFFFF,
      refin: false,
      refout: false,
      xorout: 0x0000,
      check: 0xAEE7,
      residue: 0x0000,
      sick: false,
      name: "CRC-16/CMS"
    },
    crc_16_dds_110: %{
      width: 16,
      poly: 0x8005,
      init: 0x800D,
      refin: false,
      refout: false,
      xorout: 0x0000,
      check: 0x9ECF,
      residue: 0x0000,
      sick: false,
      name: "CRC-16/DDS-110"
    },
    crc_16_dect_r: %{
      width: 16,
      poly: 0x0589,
      init: 0x0000,
      refin: false,
      refout: false,
      xorout: 0x0001,
      check: 0x007E,
      residue: 0x0589,
      sick: false,
      name: "CRC-16/DECT-R"
    },
    crc_16_dect_x: %{
      width: 16,
      poly: 0x0589,
      init: 0x0000,
      refin: false,
      refout: false,
      xorout: 0x0000,
      check: 0x007F,
      residue: 0x0000,
      sick: false,
      name: "CRC-16/DECT-X"
    },
    crc_16_dnp: %{
      width: 16,
      poly: 0x3D65,
      init: 0x0000,
      refin: true,
      refout: true,
      xorout: 0xFFFF,
      check: 0xEA82,
      residue: 0x66C5,
      sick: false,
      name: "CRC-16/DNP"
    },
    crc_16_en_13757: %{
      width: 16,
      poly: 0x3D65,
      init: 0x0000,
      refin: false,
      refout: false,
      xorout: 0xFFFF,
      check: 0xC2B7,
      residue: 0xA366,
      sick: false,
      name: "CRC-16/EN-13757"
    },
    crc_16_genibus: %{
      width: 16,
      poly: 0x1021,
      init: 0xFFFF,
      refin: false,
      refout: false,
      xorout: 0xFFFF,
      check: 0xD64E,
      residue: 0x1D0F,
      sick: false,
      name: "CRC-16/GENIBUS"
    },
    crc_16_gsm: %{
      width: 16,
      poly: 0x1021,
      init: 0x0000,
      refin: false,
      refout: false,
      xorout: 0xFFFF,
      check: 0xCE3C,
      residue: 0x1D0F,
      sick: false,
      name: "CRC-16/GSM"
    },
    crc_16_lj1200: %{
      width: 16,
      poly: 0x6F63,
      init: 0x0000,
      refin: false,
      refout: false,
      xorout: 0x0000,
      check: 0xBDF4,
      residue: 0x0000,
      sick: false,
      name: "CRC-16/LJ1200"
    },
    crc_16_mcrf4xx: %{
      width: 16,
      poly: 0x1021,
      init: 0xFFFF,
      refin: true,
      refout: true,
      xorout: 0x0000,
      check: 0x6F91,
      residue: 0x0000,
      sick: false,
      name: "CRC-16/MCRF4XX"
    },
    crc_16_modbus: %{
      width: 16,
      poly: 0x8005,
      init: 0xFFFF,
      refin: true,
      refout: true,
      xorout: 0x0000,
      check: 0x4B37,
      residue: 0x0000,
      sick: false,
      name: "CRC-16/MODBUS"
    },
    crc_16_opensafety_a: %{
      width: 16,
      poly: 0x5935,
      init: 0x0000,
      refin: false,
      refout: false,
      xorout: 0x0000,
      check: 0x5D38,
      residue: 0x0000,
      sick: false,
      name: "CRC-16/OPENSAFETY-A"
    },
    crc_16_opensafety_b: %{
      width: 16,
      poly: 0x755B,
      init: 0x0000,
      refin: false,
      refout: false,
      xorout: 0x0000,
      check: 0x20FE,
      residue: 0x0000,
      sick: false,
      name: "CRC-16/OPENSAFETY-B"
    },
    crc_16_profibus: %{
      width: 16,
      poly: 0x1DCF,
      init: 0xFFFF,
      refin: false,
      refout: false,
      xorout: 0xFFFF,
      check: 0xA819,
      residue: 0xE394,
      sick: false,
      name: "CRC-16/PROFIBUS"
    },
    crc_16_riello: %{
      width: 16,
      poly: 0x1021,
      init: 0xB2AA,
      refin: true,
      refout: true,
      xorout: 0x0000,
      check: 0x63D0,
      residue: 0x0000,
      sick: false,
      name: "CRC-16/RIELLO"
    },
    crc_16_sick: %{
      width: 16,
      poly: 0x8005,
      init: 0x0000,
      refin: false,
      refout: false,
      xorout: 0x0000,
      check: 0x56A6,
      residue: 0x0000,
      sick: true,
      name: "CRC-16/SICK"
    },
    crc_16_t10_dif: %{
      width: 16,
      poly: 0x8BB7,
      init: 0x0000,
      refin: false,
      refout: false,
      xorout: 0x0000,
      check: 0xD0DB,
      residue: 0x0000,
      sick: false,
      name: "CRC-16/T10-DIF"
    },
    crc_16_teledisk: %{
      width: 16,
      poly: 0xA097,
      init: 0x0000,
      refin: false,
      refout: false,
      xorout: 0x0000,
      check: 0x0FB3,
      residue: 0x0000,
      sick: false,
      name: "CRC-16/TELEDISK"
    },
    crc_16_tms37157: %{
      width: 16,
      poly: 0x1021,
      init: 0x89EC,
      refin: true,
      refout: true,
      xorout: 0x0000,
      check: 0x26B1,
      residue: 0x0000,
      sick: false,
      name: "CRC-16/TMS37157"
    },
    crc_16_usb: %{
      width: 16,
      poly: 0x8005,
      init: 0xFFFF,
      refin: true,
      refout: true,
      xorout: 0xFFFF,
      check: 0xB4C8,
      residue: 0xB001,
      sick: false,
      name: "CRC-16/USB"
    },
    crc_16_x_25: %{
      width: 16,
      poly: 0x1021,
      init: 0xFFFF,
      refin: true,
      refout: true,
      xorout: 0xFFFF,
      check: 0x906E,
      residue: 0xF0B8,
      sick: false,
      name: "CRC-16/X-25"
    },
    crc_16_xmodem: %{
      width: 16,
      poly: 0x1021,
      init: 0x0000,
      refin: false,
      refout: false,
      xorout: 0x0000,
      check: 0x31C3,
      residue: 0x0000,
      sick: false,
      name: "CRC-16/XMODEM"
    },
    # 17-bit
    crc_17_can_fd: %{
      width: 17,
      poly: 0x0001685B,
      init: 0x00000000,
      refin: false,
      refout: false,
      xorout: 0x00000000,
      check: 0x00004F03,
      residue: 0x00000000,
      sick: false,
      name: "CRC-17/CAN-FD"
    },
    # 21-bit
    crc_21_can_fd: %{
      width: 21,
      poly: 0x00102899,
      init: 0x00000000,
      refin: false,
      refout: false,
      xorout: 0x00000000,
      check: 0x000ED841,
      residue: 0x00000000,
      sick: false,
      name: "CRC-21/CAN-FD"
    },
    # 24-bit
    crc_24: %{
      width: 24,
      poly: 0x00864CFB,
      init: 0x00B704CE,
      refin: false,
      refout: false,
      xorout: 0x00000000,
      check: 0x0021CF02,
      residue: 0x00000000,
      sick: false,
      name: "CRC-24"
    },
    crc_24_ble: %{
      width: 24,
      poly: 0x0000065B,
      init: 0x00555555,
      refin: true,
      refout: true,
      xorout: 0x00000000,
      check: 0x00C25A56,
      residue: 0x00000000,
      sick: false,
      name: "CRC-24/BLE"
    },
    crc_24_flexray_a: %{
      width: 24,
      poly: 0x005D6DCB,
      init: 0x00FEDCBA,
      refin: false,
      refout: false,
      xorout: 0x00000000,
      check: 0x007979BD,
      residue: 0x00000000,
      sick: false,
      name: "CRC-24/FLEXRAY-A"
    },
    crc_24_flexray_b: %{
      width: 24,
      poly: 0x005D6DCB,
      init: 0x00ABCDEF,
      refin: false,
      refout: false,
      xorout: 0x00000000,
      check: 0x001F23B8,
      residue: 0x00000000,
      sick: false,
      name: "CRC-24/FLEXRAY-B"
    },
    crc_24_interlaken: %{
      width: 24,
      poly: 0x00328B63,
      init: 0x00FFFFFF,
      refin: false,
      refout: false,
      xorout: 0x00FFFFFF,
      check: 0x00B4F3E6,
      residue: 0x00144E63,
      sick: false,
      name: "CRC-24/INTERLAKEN"
    },
    crc_24_lte_a: %{
      width: 24,
      poly: 0x00864CFB,
      init: 0x00000000,
      refin: false,
      refout: false,
      xorout: 0x00000000,
      check: 0x00CDE703,
      residue: 0x00000000,
      sick: false,
      name: "CRC-24/LTE-A"
    },
    crc_24_lte_b: %{
      width: 24,
      poly: 0x00800063,
      init: 0x00000000,
      refin: false,
      refout: false,
      xorout: 0x00000000,
      check: 0x0023EF52,
      residue: 0x00000000,
      sick: false,
      name: "CRC-24/LTE-B"
    },
    # 30-bit
    crc_30_cdma: %{
      width: 30,
      poly: 0x2030B9C7,
      init: 0x3FFFFFFF,
      refin: false,
      refout: false,
      xorout: 0x3FFFFFFF,
      check: 0x04C34ABF,
      residue: 0x34EFA55A,
      sick: false,
      name: "CRC-30/CDMA"
    },
    # 31-bit
    crc_31_philips: %{
      width: 31,
      poly: 0x04C11DB7,
      init: 0x7FFFFFFF,
      refin: false,
      refout: false,
      xorout: 0x7FFFFFFF,
      check: 0x0CE9E46C,
      residue: 0x4EAF26F1,
      sick: false,
      name: "CRC-31/PHILIPS"
    },
    # 32-bit
    crc_32: %{
      width: 32,
      poly: 0x04C11DB7,
      init: 0xFFFFFFFF,
      refin: true,
      refout: true,
      xorout: 0xFFFFFFFF,
      check: 0xCBF43926,
      residue: 0xDEBB20E3,
      sick: false,
      name: "CRC-32"
    },
    crc_32_autosar: %{
      width: 32,
      poly: 0xF4ACFB13,
      init: 0xFFFFFFFF,
      refin: true,
      refout: true,
      xorout: 0xFFFFFFFF,
      check: 0x1697D06A,
      residue: 0x904CDDBF,
      sick: false,
      name: "CRC-32/AUTOSAR"
    },
    crc_32_bzip2: %{
      width: 32,
      poly: 0x04C11DB7,
      init: 0xFFFFFFFF,
      refin: false,
      refout: false,
      xorout: 0xFFFFFFFF,
      check: 0xFC891918,
      residue: 0xC704DD7B,
      sick: false,
      name: "CRC-32/BZIP2"
    },
    crc_32_jamcrc: %{
      width: 32,
      poly: 0x04C11DB7,
      init: 0xFFFFFFFF,
      refin: true,
      refout: true,
      xorout: 0x00000000,
      check: 0x340BC6D9,
      residue: 0x00000000,
      sick: false,
      name: "CRC-32/JAMCRC"
    },
    crc_32_mpeg_2: %{
      width: 32,
      poly: 0x04C11DB7,
      init: 0xFFFFFFFF,
      refin: false,
      refout: false,
      xorout: 0x00000000,
      check: 0x0376E6E7,
      residue: 0x00000000,
      sick: false,
      name: "CRC-32/MPEG-2"
    },
    crc_32_posix: %{
      width: 32,
      poly: 0x04C11DB7,
      init: 0x00000000,
      refin: false,
      refout: false,
      xorout: 0xFFFFFFFF,
      check: 0x765E7680,
      residue: 0xC704DD7B,
      sick: false,
      name: "CRC-32/POSIX"
    },
    crc_32_xfer: %{
      width: 32,
      poly: 0x000000AF,
      init: 0x00000000,
      refin: false,
      refout: false,
      xorout: 0x00000000,
      check: 0xBD0BE338,
      residue: 0x00000000,
      sick: false,
      name: "CRC-32/XFER"
    },
    crc_32c: %{
      width: 32,
      poly: 0x1EDC6F41,
      init: 0xFFFFFFFF,
      refin: true,
      refout: true,
      xorout: 0xFFFFFFFF,
      check: 0xE3069283,
      residue: 0xB798B438,
      sick: false,
      name: "CRC-32C"
    },
    crc_32d: %{
      width: 32,
      poly: 0xA833982B,
      init: 0xFFFFFFFF,
      refin: true,
      refout: true,
      xorout: 0xFFFFFFFF,
      check: 0x87315576,
      residue: 0x45270551,
      sick: false,
      name: "CRC-32D"
    },
    crc_32q: %{
      width: 32,
      poly: 0x814141AB,
      init: 0x00000000,
      refin: false,
      refout: false,
      xorout: 0x00000000,
      check: 0x3010BF7F,
      residue: 0x00000000,
      sick: false,
      name: "CRC-32Q"
    },
    # 40-bit
    crc_40_gsm: %{
      width: 40,
      poly: 0x0000000004820009,
      init: 0x0000000000000000,
      refin: false,
      refout: false,
      xorout: 0x000000FFFFFFFFFF,
      check: 0x000000D4164FC646,
      residue: 0x000000C4FF8071FF,
      sick: false,
      name: "CRC-40/GSM"
    },
    # 64-bit
    crc_64: %{
      width: 64,
      poly: 0x42F0E1EBA9EA3693,
      init: 0x0000000000000000,
      refin: false,
      refout: false,
      xorout: 0x0000000000000000,
      check: 0x6C40DF5F0B497347,
      residue: 0x0000000000000000,
      sick: false,
      name: "CRC-64"
    },
    crc_64_go_iso: %{
      width: 64,
      poly: 0x000000000000001B,
      init: 0xFFFFFFFFFFFFFFFF,
      refin: true,
      refout: true,
      xorout: 0xFFFFFFFFFFFFFFFF,
      check: 0xB90956C775A41001,
      residue: 0x5300000000000000,
      sick: false,
      name: "CRC-64/GO-ISO"
    },
    crc_64_jones: %{
      width: 64,
      poly: 0xAD93D23594C935A9,
      init: 0xFFFFFFFFFFFFFFFF,
      refin: true,
      refout: true,
      xorout: 0x0000000000000000,
      check: 0xCAA717168609F281,
      residue: 0x0000000000000000,
      sick: false,
      name: "CRC-64/JONES"
    },
    crc_64_we: %{
      width: 64,
      poly: 0x42F0E1EBA9EA3693,
      init: 0xFFFFFFFFFFFFFFFF,
      refin: false,
      refout: false,
      xorout: 0xFFFFFFFFFFFFFFFF,
      check: 0x62EC59E3F1A4F00A,
      residue: 0xFCACBEBD5931A992,
      sick: false,
      name: "CRC-64/WE"
    },
    crc_64_xz: %{
      width: 64,
      poly: 0x42F0E1EBA9EA3693,
      init: 0xFFFFFFFFFFFFFFFF,
      refin: true,
      refout: true,
      xorout: 0xFFFFFFFFFFFFFFFF,
      check: 0x995DC9BBDF1939FA,
      residue: 0x49958C9ABD7D353F,
      sick: false,
      name: "CRC-64/XZ"
    }
  }

  # Alias -> root key mapping
  @aliases %{
    arc: :crc_16,
    crc_16_arc: :crc_16,
    crc_16_lha: :crc_16,
    crc_ibm: :crc_16,
    crc_a: :crc_16_a,
    crc_16_spi_fujitsu: :crc_16_aug_ccitt,
    crc_16_umts: :crc_16_buypass,
    crc_16_verifone: :crc_16_buypass,
    crc_16_ccitt_true: :crc_16_ccitt,
    crc_16_kermit: :crc_16_ccitt,
    crc_ccitt: :crc_16_ccitt,
    kermit: :crc_16_ccitt,
    crc_16_darc: :crc_16_genibus,
    crc_16_epc: :crc_16_genibus,
    crc_16_i_code: :crc_16_genibus,
    modbus: :crc_16_modbus,
    crc_16_iec_61158_2: :crc_16_profibus,
    sick: :crc_16_sick,
    crc_16_b: :crc_16_x_25,
    crc_16_ibm_sdlc: :crc_16_x_25,
    crc_16_iso_hdlc: :crc_16_x_25,
    crc_b: :crc_16_x_25,
    x_25: :crc_16_x_25,
    crc_16_acorn: :crc_16_xmodem,
    crc_16_lte: :crc_16_xmodem,
    xmodem: :crc_16_xmodem,
    zmodem: :crc_16_xmodem,
    crc_5: :crc_5_usb,
    dow_crc: :crc_8_maxim,
    x_crc_12: :crc_12_dect,
    crc_12_3gpp: :crc_12_umts,
    crc_24_openpgp: :crc_24,
    crc_32_adccp: :crc_32,
    pkzip: :crc_32,
    b_crc_32: :crc_32_bzip2,
    crc_32_aal5: :crc_32_bzip2,
    crc_32_dect_b: :crc_32_bzip2,
    jamcrc: :crc_32_jamcrc,
    cksum: :crc_32_posix,
    xfer: :crc_32_xfer,
    crc_32_castagnoli: :crc_32c,
    crc_32_interlaken: :crc_32c,
    crc_32_iscsi: :crc_32c,
    crc_64_ecma_182: :crc_64,
    crc_64_go_ecma: :crc_64_xz
  }

  @doc false
  def all, do: @models

  @doc false
  def get(key) when is_atom(key) do
    case Map.get(@models, key) do
      nil ->
        case Map.get(@aliases, key) do
          nil -> nil
          root_key -> Map.get(@models, root_key)
        end

      model ->
        model
    end
  end

  @doc false
  def resolve_key(key) when is_atom(key) do
    if Map.has_key?(@models, key) do
      key
    else
      Map.get(@aliases, key)
    end
  end

  @doc false
  def root_keys, do: Map.keys(@models)

  @doc false
  def init_resource(params) when is_map(params) do
    width = Map.fetch!(params, :width)
    poly = Map.fetch!(params, :poly)
    init = Map.fetch!(params, :init)
    refin = Map.fetch!(params, :refin)
    refout = Map.fetch!(params, :refout)
    xorout = Map.fetch!(params, :xorout)
    check = Map.get(params, :check, 0)
    residue = Map.get(params, :residue, 0)
    sick = Map.get(params, :sick, false)

    msb_mask = 1 <<< (width - 1)
    crc_mask = 1 ||| (msb_mask - 1) <<< 1
    crc_shift = if width < 8, do: 8 - width, else: 0

    %{
      width: width,
      poly: poly,
      init: init,
      refin: refin,
      refout: refout,
      xorout: xorout,
      check: check,
      residue: residue,
      sick: sick,
      msb_mask: msb_mask,
      crc_mask: crc_mask,
      crc_shift: crc_shift,
      value: 0,
      extra: 0
    }
  end
end
