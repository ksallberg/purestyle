-module(complex6).
-export([foo/1, str/1]).
-on_load(init/0).

init() ->
    ok = erlang:load_nif("c_src/complex6_nif", 0).

foo(_X) ->
    exit(nif_library_not_loaded).

str(_Y) ->
    exit(nif_library_not_loaded).
