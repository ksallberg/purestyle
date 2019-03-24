#include <erl_nif.h>

extern int foo(int x);
extern int str(int y);

static ERL_NIF_TERM foo_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
  int x, ret;

  if(!enif_get_int(env, argv[0], &x)) {
    return enif_make_badarg(env);
  }

  ret = foo(x);

  return enif_make_int(env, ret);
}

static ERL_NIF_TERM str_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
  int y, ret;

  if(!enif_get_int(env, argv[0], &y)) {
    return enif_make_badarg(env);
  }

  ret = str(y);

  return enif_make_int(env, ret);
}

static ErlNifFunc nif_funcs[] = {
  {"foo", 1, foo_nif},
  {"str", 1, str_nif}
};

ERL_NIF_INIT(complex6, nif_funcs, NULL, NULL, NULL, NULL)
