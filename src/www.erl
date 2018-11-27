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

routes() ->
    [ #route{protocol = html,
             verb = get,
             address = "/",
             subdomain = "www",
             callback = fun handle_homepage/4}
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

    , #route{protocol = html,
             verb = get,
             address = "/unscii-16.ttf",
             subdomain = "www",
             callback = fun handle_ttf/4}
    , #route{protocol = html,
             verb = get,
             address = "/unscii-16.woff",
             subdomain = "www",
             callback = fun handle_woff/4}


    %% bonsai trees
    , #route{protocol = html,
             verb = get,
             address = "/bonsai.html",
             subdomain = "www",
             callback = fun handle_bonsai/4}
    , #route{protocol = html,
             verb = get,
             address = "/bonsai_lonn.html",
             subdomain = "www",
             callback = fun handle_bonsai_lonn/4}
    , #route{protocol = html,
             verb = get,
             address = "/bonsai_stricta.html",
             subdomain = "www",
             callback = fun handle_bonsai_stricta/4}
    , #route{protocol = html,
             verb = get,
             address = "/bonsai_pfitzeriana.html",
             subdomain = "www",
             callback = fun handle_bonsai_pfitzeriana/4}
    , #route{protocol = html,
             verb = get,
             address = "/bonsai_idegran.html",
             subdomain = "www",
             callback = fun handle_bonsai_idegran/4}
    , #route{protocol = html,
             verb = get,
             address = "/bonsai_x.html",
             subdomain = "www",
             callback = fun handle_bonsai_x/4}
    , #route{protocol = html,
             verb = get,
             address = "/bonsai_y.html",
             subdomain = "www",
             callback = fun handle_bonsai_y/4}
    , #route{protocol = html,
             verb = get,
             address = "/bonsai_z.html",
             subdomain = "www",
             callback = fun handle_bonsai_z/4}
    , #route{protocol = html,
             verb = get,
             address = "/bonsai_shinpaku.html",
             subdomain = "www",
             callback = fun handle_bonsai_shinpaku/4}
    , #route{protocol = html,
             verb = get,
             address = "/bonsai_yama.html",
             subdomain = "www",
             callback = fun handle_bonsai_yama/4}
    , #route{protocol = html,
             verb = get,
             address = "/bonsai_hibiscus.html",
             subdomain = "www",
             callback = fun handle_bonsai_hibiscus/4}
    , #route{protocol = html,
             verb = get,
             address = "/bonsai_avenbok.html",
             subdomain = "www",
             callback = fun handle_bonsai_avenbok/4}

    , #route{protocol = html,
             verb = get,
             address = "/bonsai_katt.html",
             subdomain = "www",
             callback = fun handle_bonsai_katt/4}

    %% debug
    ,  #route{protocol = html,
              verb = get,
              address = "/uptime",
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

handle_bonsai(_, _, _, _InstanceName) ->
    {ok, Binary} = file:read_file("pages/bonsai.html"),
    Binary.

handle_bonsai_lonn(_, _, _, _InstanceName) ->
    {ok, Binary} = file:read_file("pages/bonsai_lonn.html"),
    Binary.

handle_bonsai_stricta(_, _, _, _InstanceName) ->
    {ok, Binary} = file:read_file("pages/bonsai_stricta.html"),
    Binary.

handle_bonsai_pfitzeriana(_, _, _, _InstanceName) ->
    {ok, Binary} = file:read_file("pages/bonsai_pfitzeriana.html"),
    Binary.

handle_bonsai_idegran(_, _, _, _InstanceName) ->
    {ok, Binary} = file:read_file("pages/bonsai_idegran.html"),
    Binary.

handle_bonsai_x(_, _, _, _InstanceName) ->
    {ok, Binary} = file:read_file("pages/bonsai_x.html"),
    Binary.

handle_bonsai_y(_, _, _, _InstanceName) ->
    {ok, Binary} = file:read_file("pages/bonsai_y.html"),
    Binary.

handle_bonsai_z(_, _, _, _InstanceName) ->
    {ok, Binary} = file:read_file("pages/bonsai_z.html"),
    Binary.

handle_bonsai_shinpaku(_, _, _, _IN) ->
    {ok, Binary} = file:read_file("pages/bonsai_shinpaku.html"),
    Binary.

handle_bonsai_yama(_, _, _, _IN) ->
    {ok, Binary} = file:read_file("pages/bonsai_yama.html"),
    Binary.

handle_bonsai_avenbok(_, _, _, _IN) ->
    {ok, Binary} = file:read_file("pages/bonsai_avenbok.html"),
    Binary.

handle_bonsai_hibiscus(_, _, _, _IN) ->
    {ok, Binary} = file:read_file("pages/bonsai_hibiscus.html"),
    Binary.

handle_bonsai_katt(_, _, _, _InstanceName) ->
    {ok, Binary} = file:read_file("pages/bonsai_katt.html"),
    Binary.

%% fonts
handle_ttf(_, _, _, _InstanceName) ->
    {ok, Binary} = file:read_file("pages/unscii-16.ttf"),
    Binary.

handle_woff(_, _, _, _InstanceName) ->
    {ok, Binary} = file:read_file("pages/unscii-16.woff"),
    Binary.


handle_uptime(_, _, _, _InstanceName) ->
    lager:log(info, self(), "www: show uptime.", []),
    WrapFun  = fun(Txt) ->
                       "<pre>" ++ Txt ++ "</pre>"
               end,
    Spacing  = "\n\n\n",
    Uptime   = os:cmd("uptime"),
    FreeM    = os:cmd("free -m"),
    Procs    = integer_to_list(length(erlang:processes())),
    ProcsTxt = "Procs: " ++ Procs,
    ErMem    = erlang:memory(),
    Rel      = erlang:system_info(otp_release),
    Memory   = lists:flatten(io_lib:format("~p", [ErMem])),
    {_, Tot} = lists:keyfind(total, 1, ErMem),
    TotMb    = integer_to_list(Tot div 1048576),
    Link     = "<a href='https://play.purestyle.se/'>play</a>",
    FooVal   = integer_to_list(complex6:foo(3)),
    BarVal   = integer_to_list(complex6:bar(5)),
    Nif      = "hello! foo: " ++ FooVal ++ ", bar: " ++ BarVal,
    Cass     = os:cmd("/home/pi/Documents/cassandra/bin/nodetool status"),
    Ls = [ "<html><head></head><body>"
         , WrapFun(Uptime)
         , Spacing
         , WrapFun(FreeM)
         , Spacing
         , WrapFun(ProcsTxt)
         , Spacing
         , WrapFun(Memory)
         , Spacing
         , WrapFun("Total in Mb: " ++ TotMb)
         , Spacing
         , WrapFun("Erlang/OTP relase: " ++ Rel)
         , Spacing
         , Link
         , Spacing
         , WrapFun("NIF result: " ++ Nif)
         , Spacing
         , WrapFun(Cass)
         , Spacing
         , "</body></html>"
         ],
    ?l2b(lists:flatten(Ls)).

handle_homepage(_, _, _, _InstanceName) ->
    {ok, Binary} = file:read_file("pages/homepage.html"),
    Binary.
