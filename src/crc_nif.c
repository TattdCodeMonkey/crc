#include "erl_nif.h"

extern ERL_NIF_TERM _calc_16_ccitt(ErlNifEnv* env, int arc, const ERL_NIF_TERM argv[]);

static ErlNifFunc nif_funcs[] = {
  { "_calc_16_ccitt", 2, _calc_16_ccitt }
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

ERL_NIF_INIT(Elixir.Crc, nif_funcs, &load, &reload, &upgrade, &unload)
