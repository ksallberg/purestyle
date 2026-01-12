%% Copyright (c) 2026, Kristian Sällberg
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

-module(admin).

-include_lib("purestyle/include/common.hrl").

-export([get_users/0,
         get_user/1,
         delete_user/1,
         delete_sql_injection_users/0
        ]).

get_users() ->
    F = fun() ->
            mnesia:select(user, [{#user{username = '$1',
                                        _ = '_'},
                                 [],
                                 ['$1']
                                }])
        end,
    {atomic, Users} = mnesia:transaction(F),
    Users.

get_user(Username) ->
    F = fun() ->
            mnesia:read(user, Username)
        end,
    {atomic, [User]} = mnesia:transaction(F),
    User.

delete_user(Username) ->
    F = fun() ->
            mnesia:delete(user, Username, write)
        end,
    {atomic, Answer} = mnesia:transaction(F),
    Answer.

delete_users(Usernames) ->
    F = fun() ->
                DelFun = fun(Username) ->
                                 mnesia:delete(user, Username, write)
                         end,
                lists:foreach(DelFun, Usernames)
        end,
    {atomic, Answer} = mnesia:transaction(F),
    Answer.

%% delete all users with a '%' in the name
delete_sql_injection_users() ->
    delete_users(
      lists:filter(fun(X) -> lists:member($%, X) end,
                   admin:get_users())).
