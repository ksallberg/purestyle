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

-module(musiklistan).

-behaviour(http_handler).

-export([reg_user/2,
         playlist_create/2,
         playlists_get/1,
         playlist_get/1,
         add_track/2,
         get_users/0,
         add_playlist_to_user/2,
         leave_list/2
        ]).

-export([ init/0
        , routes/0
        ]).

-include("common.hrl").

-define(KEY, <<"abcdefghabcdefgh">>).
-define(IV, <<"12345678abcdefgh">>).
-define(DB, login_tracker).

init() ->
    io:format("Starting ets..."),

    ets:new(?DB, [public, set, named_table]),
    ets:delete_all_objects(?DB),
    ets:insert(?DB, {active_users, []}),

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
    [ {file, get,  "/favicon.ico",    fun handle_icon/3}
    , {file, get,  "/fluffy_cat.jpg", fun handle_fluffy_cat/3}
    , {file, get,  "/piano_cat.jpg",  fun handle_piano_cat/3}
    , {file, get,  "/hacker.css",     fun handle_css/3}
    , {file, get,  "/playlist.js",    fun handle_js/3}

    , {html, get,  "/allusers",       fun handle_allusers/3}
    , {html, get,  "/",               fun handle_index/3}
    , {html, get,  "/leave",          fun handle_leave/3}
    , {html, get,  "/logout",         fun handle_logout/3}
    , {html, get,  "/pl2",            fun handle_pl2/3}
    , {html, get,  "/playlists",      fun handle_playlists/3}
    , {html, get,  "/register",       fun handle_register/3}
    , {html, get,  "/share_pl",       fun handle_share_pl/3}

    , {html, post, "/login_post",     fun handle_login_post/3}
    , {html, post, "/playlists_post", fun handle_playlists_post/3}
    , {html, post, "/register_post",  fun handle_register_post/3}
    , {html, post, "/share_pl_post",  fun handle_share_pl_post/3}

    , {'*',                           fun handle_wildcard/3}
    ].

%% ---- GET handlers:

handle_icon(_, _, _) ->
    {ok, Binary} = file:read_file("pages/favicon.ico"),
    Binary.

handle_fluffy_cat(_, _, _) ->
    {ok, Binary} = file:read_file("pages/fluffy_cat.jpg"),
    Binary.

handle_piano_cat(_, _, _) ->
    {ok, Binary} = file:read_file("pages/piano_cat.jpg"),
    Binary.

handle_css(_, _, _) ->
    {ok, Binary} = file:read_file("pages/hacker.css"),
    Binary.

handle_js(_, _, _) ->
    {ok, Binary} = file:read_file("pages/playlist.js"),
    Binary.

handle_allusers(_Data, _Parameters, _Headers) ->
    {ok, Binary} = file:read_file("pages/favicon.ico"),
    Binary.

handle_index(_Data, _Parameters, _Headers) ->
    {ok, Module} = erlydtl:compile_file("pages/index.dtl", index),
    {ok, Binary} = Module:render([{header, <<"Login example">>}]),
    Binary.

handle_leave(_Data, _Parameters, _Headers) ->
    {ok, Binary} = file:read_file("pages/favicon.ico"),
    Binary.

handle_logout(_, _, Headers) ->
    case is_logged_in(Headers) of
        false -> ok;
        Username ->  delete_from_active_users(Username)
    end,
    #{response      => <<"">>,
      extra_headers => "Set-Cookie: username=\r\n"
                       "Refresh: 0; url=/\r\n"}.

handle_pl2(_Data, _Parameters, _Headers) ->
    {ok, Binary} = file:read_file("pages/favicon.ico"),
    Binary.

handle_playlists(_Data, _Parameters, Headers) ->
    case is_logged_in(Headers) of
        false ->
            <<"Not logged in.">>;
        Username ->
            Lists    = musiklistan:playlists_get(Username),

            FLists = lists:foldl(fun(List, Acc) ->
                                         Acc ++ add_link(List)
                                 end,
                                 "<table class=\"test\">",
                                 Lists),
            _ = ", you are logged in.<br/>" ++
                   "<h1>Lists:</h1>" ++ FLists ++ "</table>",

            {ok, Module} = erlydtl:compile_file("pages/playlists.dtl",
                                                playlists),
            {ok, Binary} = Module:render([{content, Lists}]),
            Binary
    end.

add_link({Id, Name}) ->
    "<tr>" ++
    "<td><a href=\"pl2.yaws?list="++Id++"\">" ++ Name ++ "</a></td>" ++
    "<td><a href=\"share_pl.yaws?list="++Id++"\">Dela listan</a></td>" ++
    "<td><a href=\"leave.yaws?list="++Id++"\">Lamna listan</a></td>" ++ "</tr>".

handle_register(_Data, _Parameters, _Headers) ->
    {ok, Module} = erlydtl:compile_file("pages/register.dtl", register),
    {ok, Binary} = Module:render([]),
    Binary.

handle_share_pl(_Data, _Parameters, _Headers) ->
    {ok, Binary} = file:read_file("pages/favicon.ico"),
    Binary.

%% ---- POST handlers

handle_login_post(Data, _Parameters, _Headers) ->
    PostParameters = http_parser:parameters(Data),
    {"username", Username} = lists:keyfind("username", 1, PostParameters),
    {"password", Password} = lists:keyfind("password", 1, PostParameters),
    case check_login(Username, Password) of
        login_fail ->
            <<"Login failed...">>;
        {login_ok, Cookie} ->
            #{response      => <<"">>,
              extra_headers => Cookie ++
                  "Refresh: 0; url=playlists\r\n"}
    end.

handle_playlists_post(Data, _Parameters, Headers) ->
    PostParameters = http_parser:parameters(Data),
    Username = is_logged_in(Headers),
    {_, PlaylistName} = lists:keyfind("playlist_name",
                                      1,
                                      PostParameters),
    playlist_create(Username, PlaylistName),
    #{response      => <<"">>,
      extra_headers => "Refresh: 0; url=playlists\r\n"}.

handle_register_post(Data, _Parameters, _Headers) ->
    PostParameters = http_parser:parameters(Data),
    {"username", Username} = lists:keyfind("username", 1, PostParameters),
    {"password", Password} = lists:keyfind("password", 1, PostParameters),
    Result   = reg_user(Username, Password),
    case Result of
        user_registered ->
            case check_login(Username, Password) of
                login_fail ->
                    <<"Registrering gick okej, men login gick inte bra...">>;
                {login_ok, Cookie} ->
                    #{response      => <<"">>,
                      extra_headers => Cookie ++
                                       "Refresh: 0; url=playlists\r\n"}
            end;
        user_already_existing ->
            <<"Anvandaren upptagen">>
    end.

handle_share_pl_post(_Data, _Parameters, _Headers) ->
    {ok, Binary} = file:read_file("pages/favicon.ico"),
    Binary.

handle_wildcard(_Data, _Parameters, _Headers) ->
    <<"404: Hello there!">>.

%% ---- helpers:

get_users() ->
    F = fun() ->
            mnesia:select(user, [{#user{username = '$1',
                                        _ = '_'},
                                 [],
                                 ['$1']
                                }])
        end,
    mnesia:transaction(F).

get_user(Username) ->
    F = fun() ->
            mnesia:read(user, Username)
        end,
    mnesia:transaction(F).

put_obj(Obj) ->
    Fun = fun() ->
              mnesia:write(Obj)
          end,
    mnesia:transaction(Fun).

hash_salt(String) ->
    Appsecret = appsecret:get(),
    crypto:hash(sha512, String++Appsecret).

reg_user(Username, PlainPassword) ->
    Password = hash_salt(PlainPassword),
    User = #user{username = Username,
                 info     = #userinfo{password  = Password,
                                      playlists = []}},
    case get_user(Username) of
        {atomic, []} ->
            put_obj(User),
            user_registered;
        _ ->
            user_already_existing
    end.

check_login(Username, PlainPassword) ->
    Password = hash_salt(PlainPassword),
    case get_user(Username) of
        {atomic, []} -> login_fail;
        {atomic, [#user{info = #userinfo{password = DBPassword}}]} ->
            case Password == DBPassword of
                false -> login_fail;
                true  ->
                    add_to_active_users(Username),
                    HtmlEncode = encrypt(Username),
                    C = "Set-Cookie: username=" ++ HtmlEncode ++ "\r\n",
                    {login_ok, C}
            end
    end.

add_playlist_to_user(PlaylistId, Username) ->
    {atomic, [User]} = get_user(Username),
    UserInfo         = User#user.info,
    Playlists        = UserInfo#userinfo.playlists, %% Old playlists
    ModUserInfo      = UserInfo#userinfo{playlists = Playlists ++ [PlaylistId]},
    put_obj(User#user{info=ModUserInfo}).

playlist_create(Username, PlaylistName) ->
    %% Add the new playlist to an mnesia table
    PlaylistId = uuid:uuid_to_string(uuid:get_v4()),
    NewList = #playlist{id     = PlaylistId,
                        name   = PlaylistName,
                        tracks = []},
    put_obj(NewList),
    %% Add this id to the user
    add_playlist_to_user(PlaylistId, Username).

%% Get name from playlist id
name_from_plid(Plid) ->
    F = fun() ->
            mnesia:read(playlist, Plid)
        end,
    {atomic, [Playlist]} = mnesia:transaction(F),
    Playlist#playlist.name.

playlists_get(Username) ->
    F = fun() ->
            mnesia:select(user, [{#user{username = '$1',
                                        info     = '$2'},
                                 [{'==', '$1', Username}],
                                 ['$2']
                                }])
        end,
    case mnesia:transaction(F) of
        {atomic, []}   -> [];
        {atomic, [UserInfo]} ->
            [{Plid, name_from_plid(Plid)} ||
                Plid <- UserInfo#userinfo.playlists]
    end.

determine_source(Link) ->
    Youtube    = string:str(Link, "youtube.com") > 0,
    SoundCloud = string:str(Link, "soundcloud.com") > 0,
    if
        Youtube    -> youtube;
        SoundCloud -> soundcloud;
        true       -> other
    end.

get_title(Link) ->
    case determine_source(Link) of
        youtube ->
           try
               youtube_title(Link)
           catch _:Error ->
               io:format("Parsing youtube error: ~p", [Error]),
               Link
           end;
       soundcloud ->
           try
               soundcloud_title(Link)
           catch _:Error ->
               io:format("Parsing soundcloud error: ~p", [Error]),
               Link
           end;
       other ->
           Link
    end.

soundcloud_title(Link) ->
    APIKey    = os:getenv("SOUNDCLOUD"),
    QueryLink = "https://api.soundcloud.com/resolve.json?url=" ++ Link ++
                "&client_id=" ++ APIKey,
    {ok, {_HTTPVer, _Headers, Response}}
        = httpc:request(get, {QueryLink, []}, [], []),
    [_, TitleEtc] = re:split(Response, "title\":\""),
    [Title | _]   = re:split(?b2l(TitleEtc), "\","),
    ?b2l(Title).

get_id(Link) ->
    case determine_source(Link) of
        youtube    -> youtube_id(Link);
        soundcloud -> soundcloud_id(Link);
        other      -> "no_id"
    end.

youtube_id(Link) ->
    [_, VIDEtc] = re:split(Link, "v="),
    [VID | _]   = re:split(?b2l(VIDEtc), "&"),
    ?b2l(VID).

soundcloud_id(Link) ->
    APIKey    = os:getenv("SOUNDCLOUD"),
    QueryLink = "https://api.soundcloud.com/resolve.json?url=" ++ Link ++
                "&client_id=" ++ APIKey,
    {ok, {_HTTPVer, _Headers, Response}}
        = httpc:request(get, {QueryLink, []}, [], []),
    [_, IdEtc | _] = re:split(Response, "id\":"),
    [Id | _]       = re:split(?b2l(IdEtc), ","),
    ?b2l(Id).

%% Take a link, extract the title that youtube associates with
%% the title, return both the link and the title
youtube_title(Link) ->
    VID       = youtube_id(Link),
    QueryLink = "http://youtube.com/get_video_info?video_id=" ++ VID,
    {ok, {_HTTPVer, _Headers, Response}}
        = httpc:request(get, {QueryLink, []}, [], []),
    [_, TitleEtc] = re:split(Response, "title="),
    [Title | _]   = re:split(?b2l(TitleEtc), "&"),
    lists:foldl(fun({Old, New}, Acc) ->
                    re:replace(Acc, Old, New, [global,{return,list}])
                end,
                ?b2l(Title),
                [{"\\+", " "}, %% Rules for how to remove html encoded
                 {"%28", "("},
                 {"%29", ")"},
                 {"%C3%A4", "ä"},
                 {"%C3%B6", "ö"},
                 {"%2B", "+"}
                ]).

playlist_get(PlaylistId) ->
    F = fun() ->
            mnesia:read(playlist, PlaylistId)
        end,
    case mnesia:transaction(F) of
        {atomic, []}   -> [];
        {atomic, [Playlist]} -> Playlist
    end.

add_track(PlaylistId, URL) ->
    PlayList   = playlist_get(PlaylistId),
    PrevTracks = PlayList#playlist.tracks,
    NewTrack   = #track{source = determine_source(URL),
                        id     = get_id(URL),
                        url    = URL,
                        title  = get_title(URL)},
    ModList    = PlayList#playlist{tracks = PrevTracks ++ [NewTrack]},
    put_obj(ModList).

leave_list(Username, ListId) ->
    {atomic, [User]} = get_user(Username),
    UserInfo         = User#user.info,
    Playlists        = UserInfo#userinfo.playlists,
    FilteredLists    = [X || X <- Playlists, X /= ListId],
    ModUserInfo      = UserInfo#userinfo{playlists = FilteredLists},
    put_obj(User#user{info=ModUserInfo}).

is_logged_in(Headers) ->
    case [Cookies || {"Cookie", Cookies} <- Headers] of
        [] ->
            false;
        [CookieString] ->
            CookieKVS = http_parser:cookies(CookieString),
            Username = lists:keyfind("username", 1, CookieKVS),
            case Username of
                false ->
                    false;
                {"username", HtmlEncode} ->
                    Decrypted = decrypt(HtmlEncode),
                    case is_active_user(Decrypted) of
                        true ->
                            Decrypted;
                        false ->
                            false
                    end
            end
    end.

is_active_user(Username) ->
    ActiveUsers = ets:lookup(?DB, active_users),
    case ActiveUsers of
        [] ->
            false;
        [{active_users, Ls}] ->
            lists:member(Username, Ls)
    end.

add_to_active_users(Username) ->
    ActiveUsers = ets:lookup(?DB, active_users),
    case ActiveUsers of
        [] ->
            ets:insert(?DB, {active_users, [Username]});
        [{active_users, Ls}] ->
            ets:insert(?DB, {active_users, [Username|Ls]})
    end.

delete_from_active_users(Username) ->
    ActiveUsers = ets:lookup(?DB, active_users),
    case ActiveUsers of
        [] ->
            should_not_happen;
        [{active_users, Ls}] ->
            NewUsers = Ls -- [Username],
            ets:insert(?DB, {active_users, NewUsers})
    end.

encrypt(PlainText) ->
    En = crypto:block_encrypt(aes_cfb128, ?KEY, ?IV, PlainText),
    B64 = base64:encode(En),
    http_uri:encode(binary_to_list(B64)).

decrypt(HtmlEncode) ->
    HtmlUnencode = list_to_binary(http_uri:decode(HtmlEncode)),
    UnB64 = base64:decode(HtmlUnencode),
    Dec = crypto:block_decrypt(aes_cfb128, ?KEY, ?IV, UnB64),
    binary_to_list(Dec).
