-module(homepage).

-export([info/4]).

info(_Data, _Parameters, _Headers, _InstanceName) ->
    <<"Hello there! Welcome to my homepage">>.
