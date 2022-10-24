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

-module(demo).

-include_lib("brunhilde/include/brunhilde.hrl").

-export([ routes/0 ]).

routes() ->
    %% File addresses
    [ #route{protocol = file,
             verb = get,
             address = <<"/pstyle.png">>,
             subdomain = <<"demo">>,
             callback = fun handle_logo/4}
    , #route{protocol = file,
             verb = get,
             address = <<"/">>,
             subdomain = <<"demo">>,
             callback = fun handle_page/4}
    ].

handle_page(_, _, _, _InstanceName) ->
    {ok, Module} = erlydtl:compile_file("pages/demo.dtl",
                                        register,
                                        [{out_dir, "compiled_templates"}]
                                       ),
    {ok, Binary} = Module:render([]),
    iolist_to_binary(Binary).

handle_logo(_, _, _, _InstanceName) ->
    {ok, Binary} = file:read_file("pages/pstyle.png"),
    Binary.
