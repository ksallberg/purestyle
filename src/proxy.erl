%% Copyright (c) 2014-2016, Kristian Sällberg
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%%
%% * Redistributions of source code must retain the above copyright notice, this
%%   list of conditions and the following disclaimer.
%%
%% * Redistributions in binary form must reproduce the above copyright notice,
%%   this list of conditions and the following disclaimer in the documentation
%%   and/or other materials provided with the distribution.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
%% LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
%% CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
%% ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
%% OF THE POSSIBILITY OF SUCH DAMAGE.

-module(proxy).

-behaviour(http_handler).

-include_lib("brunhilde/include/brunhilde.hrl").

-export([ start/0
        , init/1
        , routes/0
        ]).

-include("common.hrl").

start() ->
    lager:start(),
    application:start(purestyle).

init(InstanceName) ->
    %% If several instances of purestyle play are running, only start ETS once.
    %% The purpose of having several instances is only for development anyway,
    %% serving HTTP for development and HTTPS for production.
    DB = play:db_name(InstanceName),
    io:format("Starting ets..."),
    ets:new(DB, [public, set, named_table]),
    ets:delete_all_objects(DB),
    ets:insert(DB, {active_users, []}),
    io:format("Starting mnesia..."),
    NodeList = [node()],
    mnesia:create_schema(NodeList),
    mnesia:start(),
    mnesia:create_table(user,
                        [{attributes, record_info(fields, user)},
                         {disc_copies, NodeList}]),
    mnesia:create_table(playlist,
                        [{attributes, record_info(fields, playlist)},
                         {disc_copies, NodeList}]),
    io:format("Starting inets..."),
    ssl:start(),
    inets:start().

routes() ->
    play:routes()
        ++ www:routes()
        ++ demo:routes()
        ++ [{'*', fun handle_wildcard/4}].

handle_wildcard(_Data, _Parameters, _Headers, _InstanceName) ->
    <<"404: Hello there!">>.
