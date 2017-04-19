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

-module(www).

-include("_build/default/lib/brunhilde/include/brunhilde.hrl").

-export([ routes/0 ]).

routes() ->
    [ #route{protocol = html,
             verb = get,
             address = "/",
             subdomain = "www",
             callback = {homepage, info}}
    , #route{protocol = file,
             verb = get,
             address = "/pstyle.png",
             subdomain = "www",
             callback = fun handle_logo/4}
    , #route{protocol = file,
             verb = get,
             address = "/style.css",
             subdomain = "www",
             callback = fun handle_css/4}
    , #route{protocol = file,
             verb = get,
             address = "/favicon.ico",
             subdomain = "www",
             callback = fun handle_icon/4}
    , #route{protocol = file,
             verb = get,
             address = "/waves.js",
             subdomain = "www",
             callback = fun handle_js/4}
    , #route{protocol = html,
             verb = get,
             address = "/uptime",
             subdomain = "www",
             callback = fun handle_uptime/4}
    ].

%% Experimental:

handle_logo(_, _, _, _InstanceName) ->
    {ok, Binary} = file:read_file("pages/pstyle.png"),
    Binary.

handle_css(_, _, _, _InstanceName) ->
    {ok, Binary} = file:read_file("pages/pstyle.css"),
    Binary.

handle_icon(_, _, _, _InstanceName) ->
    {ok, Binary} = file:read_file("pages/favicon.ico"),
    Binary.

handle_js(_, _, _, _InstanceName) ->
    {ok, Binary} = file:read_file("pages/waves.js"),
    Binary.

handle_uptime(_, _, _, _InstanceName) ->
    Uptime = os:cmd("uptime"),
    FreeM  = os:cmd("free -m"),
    ?l2b(Uptime ++ "\n\n\n" ++ FreeM).
