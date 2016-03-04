#include "erl_nif.h"

#define CRC16 0x8005

unsigned short calc_16(unsigned char* data, int size)
{
  if (data == NULL) {
    return 0;
  }

  unsigned short result = 0;
  int read = 0;
  int flag;
  
  while(size > 0) {
    flag = result >> 15;

    result <<= 1;
    result |= (*data >> (7 - read)) & 1;

    read++;
    if (read > 7) {
      read = 0;
      data++;
      size--;
    }

    if(flag) {
      result ^= CRC16;
    }
  }
  return result;
}

ERL_NIF_TERM _calc_16(ErlNifEnv* env, int arc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary binary;
  int data_size = 0;
  unsigned char* data = NULL;
  unsigned short crc = 0;

  if(arc != 1 || !enif_inspect_binary(env, argv[0], &binary)) {
    return enif_make_badarg(env);
  }

  data_size = binary.size;
  data = binary.data;

  crc = calc_16(data, data_size);

  return enif_make_int(env, crc);
}
