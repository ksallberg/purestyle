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

-module(redir).

-behaviour(http_handler).

-include("./_build/default/lib/brunhilde/include/brunhilde.hrl").

-export([ init/1
        , routes/0
        ]).

init(_InstanceName) ->
    ok.

routes() ->
    [ #route{protocol = html,
             verb = get,
             address = "/",
             subdomain = "www",
             callback = fun handle_www/4}

    %% play subdomain
    , #route{protocol = html,
             verb = get,
             address = "/",
             subdomain = "play",
             callback = fun handle_play/4}

    , #route{protocol = html,
             verb = get,
             address = "/",
             subdomain = "demo",
             callback = fun handle_demo/4}
    ].

handle_www(_Data, _Parameters, _Headers, _InstanceName) ->
    #{response      => <<"">>,
      extra_headers => "Location: https://www.purestyle.se\r\n",
      return_code   => "301 Moved Permanently"}.

handle_play(_Data, _Parameters, _Headers, _InstanceName) ->
    #{response      => <<"">>,
      extra_headers => "Location: https://play.purestyle.se\r\n",
      return_code   => "301 Moved Permanently"}.

handle_demo(_Data, _Parameters, _Headers, _InstanceName) ->
    #{response      => <<"">>,
      extra_headers => "Location: https://demo.purestyle.se\r\n",
      return_code   => "301 Moved Permanently"}.
