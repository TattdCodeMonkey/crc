#include "erl_nif.h"

static const unsigned short modbus_crc[16] = {
  0x0000, 0xcc01, 0xd801, 0x1400,
	0xf001, 0x3c00, 0x2800, 0xe401,
	0xa001, 0x6c00, 0x7800, 0xb401,
	0x5000, 0x9c01, 0x8801, 0x4400
};

unsigned short calc_16_modbus(unsigned char* data, int size) {
  unsigned short result = 0xffff;
  unsigned char val;

  while (size--) {
    val = *data++;

    result = modbus_crc[(val ^ result) & 15] ^ (result >> 4);
    result = modbus_crc[((val >> 4) ^ result) & 15] ^ (result >> 4);
  }

  return result;
}

ERL_NIF_TERM _calc_16_modbus(ErlNifEnv* env, int arc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary binary;
  int data_size = 0;
  unsigned char* data = NULL;
  unsigned short crc = 0xffff;

  if(arc != 1 || !enif_inspect_binary(env, argv[0], &binary)) {
    return enif_make_badarg(env);
  }

  data_size = binary.size;
  data = binary.data;

  crc = calc_16_modbus(data, data_size);

  return enif_make_int(env, crc);
}
