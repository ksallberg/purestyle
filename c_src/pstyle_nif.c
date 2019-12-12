#include <erl_nif.h>

extern int pstyle_get_date(char outbuf[]);

static ERL_NIF_TERM get_date_nif(ErlNifEnv *env,
                                 int argc,
                                 const ERL_NIF_TERM argv[])
{
  int mode;
  char ret[10];

  if(!enif_get_int(env, argv[0], &mode)) {
    return enif_make_badarg(env);
  }

  pstyle_get_date(ret);

  if(mode != 0) {
    ret[0] = tolower(ret[0]);
  }

  return enif_make_string(env, ret, ERL_NIF_LATIN1);
}

static ErlNifFunc nif_funcs[] = {
  {"get_date", 1, get_date_nif}
};

ERL_NIF_INIT(pstyle, nif_funcs, NULL, NULL, NULL, NULL)
