-module(pstyle).
-export([get_date/1]).
-on_load(init/0).

init() ->
    ok = erlang:load_nif("c_src/pstyle_nif", 0).

%% giving 0 means normal
%% giving 1 means lower first char
get_date(_Y) ->
    exit(nif_library_not_loaded).
