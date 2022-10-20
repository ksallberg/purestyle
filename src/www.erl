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

-include_lib("brunhilde/include/brunhilde.hrl").

-export([ routes/0 ]).

%% -define(SUBDOMAIN, '*').
-define(SUBDOMAIN, <<"www">>).

routes() ->
    [ #route{protocol = html,
             verb = get,
             address = <<"/">>,
             subdomain = ?SUBDOMAIN,
             callback = fun handle_homepage/4}
    , #route{protocol = file,
             verb = get,
             address = <<"/pstyle.png">>,
             subdomain = ?SUBDOMAIN,
             callback = fun handle_logo/4}
    , #route{protocol = file,
             verb = get,
             address = <<"/style.css">>,
             subdomain = ?SUBDOMAIN,
             callback = fun handle_css/4}
    , #route{protocol = file,
             verb = get,
             address = <<"/favicon.ico">>,
             subdomain = ?SUBDOMAIN,
             callback = fun handle_icon/4}
    , #route{protocol = file,
             verb = get,
             address = <<"/waves.js">>,
             subdomain = ?SUBDOMAIN,
             callback = fun handle_js/4}
    , #route{protocol = file,
             verb = get,
             address = <<"/waves.html">>,
             subdomain = ?SUBDOMAIN,
             callback = fun handle_waves/4}
    , #route{protocol = html,
             verb = get,
             address = <<"/uptime">>,
             subdomain = ?SUBDOMAIN,
             callback = fun handle_uptime/4}

    , #route{protocol = html,
             verb = get,
             address = <<"/unscii-16.ttf">>,
             subdomain = ?SUBDOMAIN,
             callback = fun handle_ttf/4}
    , #route{protocol = html,
             verb = get,
             address = <<"/unscii-16.woff">>,
             subdomain = ?SUBDOMAIN,
             callback = fun handle_woff/4}

    , #route{protocol = html,
             verb = get,
             address = <<"/ramen.html">>,
             subdomain = ?SUBDOMAIN,
             callback = fun handle_ramen/4}

    , #route{protocol = html,
             verb = get,
             address = <<"/rc1.html">>,
             subdomain = ?SUBDOMAIN,
             callback = fun handle_rc1/4}

    , #route{protocol = html,
             verb = get,
             address = <<"/rc2.html">>,
             subdomain = ?SUBDOMAIN,
             callback = fun handle_rc2/4}

    %% debug
    ,  #route{protocol = html,
              verb = get,
              address = <<"/uptime">>,
              callback = fun handle_uptime/4}
    ].

%% Experimental:

handle_logo(_, _, _, _InstanceName) ->
    {ok, Binary} = file:read_file("pages/pstyle.png"),
    Binary.

handle_css(_, _, _, _InstanceName) ->
    {ok, Binary} = file:read_file("pages/style.css"),
    Binary.

handle_icon(_, _, _, _InstanceName) ->
    {ok, Binary} = file:read_file("pages/favicon.ico"),
    Binary.

handle_js(_, _, _, _InstanceName) ->
    {ok, Binary} = file:read_file("pages/waves.js"),
    Binary.

handle_waves(_, _, _, _InstanceName) ->
    {ok, Binary} = file:read_file("pages/waves.html"),
    Binary.

handle_ramen(_, _, _, _InstanceName) ->
    dtl_helper("pages/ramen.dtl").

handle_rc1(_, _, _, _InstanceName) ->
    {ok, Binary} = file:read_file("pages/rc1.html"),
    Binary.

handle_rc2(_, _, _, _InstanceName) ->
    {ok, Binary} = file:read_file("pages/rc2.html"),
    Binary.

%% fonts
handle_ttf(_, _, _, _InstanceName) ->
    {ok, Binary} = file:read_file("pages/unscii-16.ttf"),
    Binary.

handle_woff(_, _, _, _InstanceName) ->
    {ok, Binary} = file:read_file("pages/unscii-16.woff"),
    Binary.

dtl_helper(PageName) ->
    {ok, Module} = erlydtl:compile_file(PageName,
                                        allusers,
                                        [{out_dir, "compiled_templates"}]
                                       ),
    {ok, Binary} = Module:render([]),
    iolist_to_binary(Binary).

handle_uptime(_, _, _, _InstanceName) ->
    lager:log(info, self(), "www: show uptime.", []),
    ErMem    = erlang:memory(),
    {_, Tot} = lists:keyfind(total, 1, ErMem),
    ProxTxt  = integer_to_list(length(erlang:processes())),
    {ok, Module} = erlydtl:compile_file("pages/uptime.dtl",
                                        index,
                                        [{out_dir, "compiled_templates"}]),
    {ok, Binary} =
        Module:render([ {uptime, os:cmd("uptime")}
                      , {freem, os:cmd("free -m")}
                      , {procstext, ProxTxt}
                      , {memory, lists:flatten(io_lib:format("~p", [ErMem]))}
                      , {totmb, integer_to_list(Tot div 1048576)}
                      , {rel, erlang:system_info(otp_release)}
                      , {otpv, erlang:system_info(otp_release)}
                      , {temperature,
                         os:cmd("/opt/vc/bin/vcgencmd measure_temp")}
                      ]),
    iolist_to_binary(Binary).

handle_homepage(_, _, _, _InstanceName) ->
    {ok, Binary} = file:read_file("pages/homepage.html"),
    Binary.
