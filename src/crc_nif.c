#include "erl_nif.h"

extern ERL_NIF_TERM _calc_8(ErlNifEnv* env, int arc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM _calc_16(ErlNifEnv* env, int arc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM _calc_16_ccitt(ErlNifEnv* env, int arc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM _calc_16_kermit(ErlNifEnv* env, int arc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM _calc_16_modbus(ErlNifEnv* env, int arc, const ERL_NIF_TERM argv[]);

static ErlNifFunc nif_funcs[] = {
  { "_calc_8", 2, _calc_8 },
  { "_calc_16", 1, _calc_16 },
  { "_calc_16_ccitt", 2, _calc_16_ccitt },
  { "_calc_16_kermit", 2, _calc_16_kermit },
  { "_calc_16_modbus", 1, _calc_16_modbus }
};

static int
load(ErlNifEnv* env, void** priv, ERL_NIF_TERM info) {
  return 0;
}

static int
reload(ErlNifEnv* env, void** priv, ERL_NIF_TERM info) {
  return 0;
}

static int
upgrade(ErlNifEnv* env, void** priv, void** old_priv, ERL_NIF_TERM info) {
  return load(env, priv, info);
}

static void
unload(ErlNifEnv* env, void* priv) {
  enif_free(priv);
}

ERL_NIF_INIT(Elixir.CRC, nif_funcs, &load, &reload, &upgrade, &unload)
