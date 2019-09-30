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

-module(play).

-include_lib("brunhilde/include/brunhilde.hrl").

-export([ routes/0
        , db_name/1 ]).

-include("common.hrl").

routes() ->
    %% File addresses
    [ #route{protocol = file,
             verb = get,
             address = "/favicon.ico",
             subdomain = "play",
             callback = fun handle_icon/4}
    , #route{protocol = file,
             verb = get,
             address = "/pstyle.png",
             subdomain = "play",
             callback = fun handle_logo/4}
    , #route{protocol = file,
             verb = get,
             address = "/images/bg.jpg",
             subdomain = "play",
             callback = fun handle_bg/4}
    , #route{protocol = file,
             verb = get,
             address = "/style.css",
             subdomain = "play",
             callback = fun handle_css/4}
    , #route{protocol = file,
             verb = get,
             address = "/playlist.js",
             subdomain = "play",
             callback = fun handle_js/4}
    , #route{protocol = file,
             verb = get,
             address = "/font-awesome.min.css",
             subdomain = "play",
             callback = fun handle_fontawesome/4}

    %% HTML addresses
    , #route{protocol = html,
             verb = get,
             address = "/allusers",
             subdomain = "play",
             callback = fun handle_allusers/4}
    , #route{protocol = html,
             verb = get,
             address = "/",
             subdomain = "play",
             callback = fun handle_index/4}

    , #route{protocol = html,
             verb = get,
             address = "/change_pw",
             subdomain = "play",
             callback = fun handle_changepw/4}

    , #route{protocol = html,
             verb = get,
             address = "/leave",
             subdomain = "play",
             callback = fun handle_leave/4}
    , #route{protocol = html,
             verb = get,
             address = "/delete_song",
             subdomain = "play",
             callback = fun handle_delete_song/4}
    , #route{protocol = html,
             verb = get,
             address = "/logout",
             subdomain = "play",
             callback = fun handle_logout/4}
    , #route{protocol = html,
             verb = get,
             address = "/playlist",
             subdomain = "play",
             callback = fun handle_playlist/4}
    , #route{protocol = html,
             verb = get,
             address = "/playlists",
             subdomain = "play",
             callback = fun handle_playlists/4}
    , #route{protocol = html,
             verb = get,
             address = "/register",
             subdomain = "play",
             callback = fun handle_register/4}
    , #route{protocol = html,
             verb = get,
             address = "/share_playlist",
             subdomain = "play",
             callback = fun handle_share_playlist/4}
    , #route{protocol = html,
             verb = get,
             address = "/share_playlist",
             subdomain = "play",
             callback = fun handle_share_playlist/4}
    , #route{protocol = html,
             verb = post,
             address = "/login_post",
             subdomain = "play",
             callback = fun handle_login_post/4}
    , #route{protocol = html,
             verb = post,
             address = "/change_pw",
             subdomain = "play",
             callback = fun handle_changepw_post/4}
    , #route{protocol = html,
             verb = post,
             address = "/playlists_post",
             subdomain = "play",
             callback = fun handle_playlists_post/4}
    , #route{protocol = html,
             verb = post,
             address = "/register_post",
             subdomain = "play",
             callback = fun handle_register_post/4}
    , #route{protocol = html,
             verb = post,
             address = "/share_playlist_post",
             subdomain = "play",
             callback = fun handle_share_playlist_post/4}
    , #route{protocol = html,
             verb = post,
             address = "/playlist_post",
             subdomain = "play",
             callback = fun handle_playlist_post/4}

    , #route{protocol = json,
             verb = post,
             address = "/change_song",
             subdomain = "play",
             callback = fun handle_change_song/4}

    ].

%% ---- GET handlers:

handle_logo(_, _, _, _InstanceName) ->
    {ok, Binary} = file:read_file("pages/pstyle.png"),
    Binary.

handle_icon(_, _, _, _InstanceName) ->
    {ok, Binary} = file:read_file("pages/favicon.ico"),
    Binary.

handle_css(_, _, _, _InstanceName) ->
    {ok, Binary} = file:read_file("pages/style.css"),
    Binary.

handle_js(_, _, _, _InstanceName) ->
    {ok, Binary} = file:read_file("pages/playlist.js"),
    Binary.

handle_fontawesome(_, _, _, _InstanceName) ->
    {ok, Binary} = file:read_file("pages/font-awesome.min.css"),
    Binary.

handle_bg(_, _, _, _InstanceName) ->
    {ok, Binary} = file:read_file("pages/bg.jpg"),
    Binary.

handle_allusers(_Data, _Parameters, _Headers, _InstanceName) ->
    {atomic, Users} = get_users(),
    {ok, Module} = erlydtl:compile_file("pages/allusers.dtl",
                                        allusers,
                                        [{out_dir, "compiled_templates"}]
                                       ),
    {ok, Binary} = Module:render([{users, Users}]),
    Binary.

handle_index(_Data, _Parameters, Headers, InstanceName) ->
    case is_logged_in(Headers, InstanceName) of
        false ->
            {ok, Module} = erlydtl:compile_file("pages/index.dtl",
                                                index,
                                                [{out_dir,
                                                  "compiled_templates"}]
                                               ),
            {ok, Binary} = Module:render([{header, <<"Login example">>}]),
            Binary;
        _Username ->
            #{response      => <<"">>,
              extra_headers => "Location: /playlists\r\n",
              return_code   => "303 See Other"}
    end.

handle_changepw(_Data, _Parameters, _Headers, _InstanceName) ->
    {ok, Module} = erlydtl:compile_file("pages/changepw.dtl",
                                        index,
                                        [{out_dir,
                                          "compiled_templates"}]
                                       ),
    {ok, Binary} = Module:render([{header, <<"Login example">>}]),
    Binary.

handle_leave(_Data, Parameters, Headers, InstanceName) ->
    case is_logged_in(Headers, InstanceName) of
        false ->
            render_not_logged_in();
        Username ->
            {"list", ListId} = lists:keyfind("list", 1, Parameters),
            leave_list(Username, ListId),
            #{response      => <<"">>,
              extra_headers => "Location: /playlists\r\n",
              return_code   => "303 See Other"}
    end.

handle_delete_song(_Data, Parameters, Headers, InstanceName) ->
    case is_logged_in(Headers, InstanceName) of
        false ->
            render_not_logged_in();
        _Username ->
            {"trackid", TrackId} = lists:keyfind("trackid", 1, Parameters),
            {"list", ListId} = lists:keyfind("list", 1, Parameters),
            delete_song(ListId, TrackId),
            #{response      => <<"">>,
              extra_headers => "Location: /playlist?list="++ListId++"\r\n",
              return_code   => "303 See Other"}
    end.

handle_logout(_, _, Headers, InstanceName) ->
    case is_logged_in(Headers, InstanceName) of
        false -> ok;
        Username ->  delete_from_active_users(Username, InstanceName)
    end,
    #{response      => <<"">>,
      extra_headers => "Set-Cookie: username=\r\n"
                       "Location: /\r\n",
      return_code   => "303 See Other"}.


handle_playlist(_Data, Parameters, Headers, InstanceName) ->
    case is_logged_in(Headers, InstanceName) of
        false ->
            render_not_logged_in();
        _Username ->
            {"list", ListId} = lists:keyfind("list", 1, Parameters),
            Playlist = playlist_get(ListId),
            Tracks   = Playlist#playlist.tracks,
            Tracks2  =
                lists:zip(Playlist#playlist.tracks,
                          lists:seq(0,
                                    length(Playlist#playlist.tracks) - 1
                                   )),
            Tracks3 =
                [{Track#track.title, Track#track.id,
                  Track#track.source, Track#track.url, Id}
                 || {Track, Id} <- Tracks2],
            {ok, Module} = erlydtl:compile_file("pages/playlist.dtl",
                                                playlist,
                                                [{out_dir,
                                                  "compiled_templates"}]
                                               ),
            {ok, Binary} = Module:render([{playlist, Playlist},
                                          {tracks,  Tracks},
                                          {tracks3, Tracks3},
                                          {listid, ListId},
                                          {playlist_name,
                                           Playlist#playlist.name}
                                         ]),
            Binary
    end.

handle_playlists(_Data, _Parameters, Headers, InstanceName) ->
    case is_logged_in(Headers, InstanceName) of
        false ->
            render_not_logged_in();
        Username ->
            Lists = playlists_get(Username),
            {ok, Module} = erlydtl:compile_file("pages/playlists.dtl",
                                                playlists,
                                                [{out_dir,
                                                  "compiled_templates"}]
                                               ),
            {ok, Binary} = Module:render([{content, Lists},
                                          {username, Username}]),
            Binary
    end.

handle_register(_Data, _Parameters, _Headers, _InstanceName) ->
    {ok, Module} = erlydtl:compile_file("pages/register.dtl",
                                        register,
                                        [{out_dir, "compiled_templates"}]
                                       ),
    {ok, Binary} = Module:render([]),
    Binary.

handle_share_playlist(_Data, Parameters, Headers, InstanceName) ->
    {"list", ListId} = lists:keyfind("list", 1, Parameters),
    case is_logged_in(Headers, InstanceName) of
        false ->
            render_not_logged_in();
        _Username ->
            {ok, Module} = erlydtl:compile_file("pages/share_playlist.dtl",
                                                share_playlist,
                                                [{out_dir,
                                                  "compiled_templates"}]
                                               ),
            {ok, Binary} = Module:render([{listid, ListId}]),
            Binary
    end.

%% ---- POST handlers

handle_login_post(Data, _Parameters, _Headers, InstanceName) ->
    PostParameters = http_parser:parameters(Data),
    {"username", Username} = lists:keyfind("username", 1, PostParameters),
    {"password", Password} = lists:keyfind("password", 1, PostParameters),
    case check_login(Username, Password, InstanceName) of
        login_fail ->
            <<"Login failed...">>;
        {login_ok, Cookie} ->
            #{response      => <<"">>,
              extra_headers => Cookie ++
                               "Location: /playlists\r\n",
              return_code   => "303 See Other"}
    end.

handle_changepw_post(Data, _Parameters, _Headers, _InstanceName) ->
    PostParameters = http_parser:parameters(Data),
    {"username", Username}   = lists:keyfind("username", 1, PostParameters),
    {"oldpw", OldPassword}   = lists:keyfind("oldpw", 1, PostParameters),
    {"newpw", NewPassword}   = lists:keyfind("newpw", 1, PostParameters),
    {"newpw2", NewPassword2} = lists:keyfind("newpw2", 1, PostParameters),
    case maybe_change_pw(Username, OldPassword, NewPassword, NewPassword2) of
        change_pw_fail ->
            <<"Password change failed...">>;
        change_pw_ok ->
            <<"Password change successful!">>
    end.

handle_playlists_post(Data, _Parameters, Headers, InstanceName) ->
    PostParameters = http_parser:parameters(Data),
    Username = is_logged_in(Headers, InstanceName),
    {_, PlaylistName} = lists:keyfind("playlist_name",
                                      1,
                                      PostParameters),
    playlist_create(Username, PlaylistName),
    #{response      => <<"">>,
      extra_headers => "Location: /playlists\r\n",
      return_code   => "303 See Other"}.

handle_register_post(Data, _Parameters, _Headers, InstanceName) ->
    PostParameters = http_parser:parameters(Data),
    {"username", Username} = lists:keyfind("username", 1, PostParameters),
    {"password", Password} = lists:keyfind("password", 1, PostParameters),
    Result   = reg_user(Username, Password),
    case Result of
        user_registered ->
            case check_login(Username, Password, InstanceName) of
                login_fail ->
                    <<"Registrering gick okej, men login gick inte bra...">>;
                {login_ok, Cookie} ->
                    #{response      => <<"">>,
                      extra_headers => Cookie ++
                                       "Location: /playlists\r\n",
                      return_code   => "303 See Other"}
            end;
        user_already_existing ->
            <<"Anvandaren upptagen">>
    end.

handle_share_playlist_post(Data, _Parameters, Headers, InstanceName) ->
    PostParameters = http_parser:parameters(Data),
    case is_logged_in(Headers, InstanceName) of
        false ->
            render_not_logged_in();
        _MyUsername ->
            {_, Playlist} = lists:keyfind("playlist",  1, PostParameters),
            {_, Username} = lists:keyfind("user_name", 1, PostParameters),
            add_playlist_to_user(Playlist, Username),
            #{response      => <<"">>,
              extra_headers => "Location: /playlists\r\n",
              return_code   => "303 See Other"}
    end.

handle_playlist_post(Data, _Parameters, Headers, InstanceName) ->
    case is_logged_in(Headers, InstanceName) of
        false ->
            render_not_logged_in();
        _ ->
            PostParameters = http_parser:parameters(Data),
            {_, Playlist} = lists:keyfind("playlist", 1, PostParameters),
            {_, Songname0} = lists:keyfind("track_name", 1, PostParameters),
            Songname = http_uri:decode(Songname0),
            add_track(Playlist, Songname),
            #{response      => <<"">>,
              extra_headers => "Location: /playlist" ++
                               "?list=" ++ Playlist ++ "\r\n",
              return_code   => "303 See Other"}
    end.

handle_change_song(#{<<"id">>     := Id,
                     <<"title">>  := Title,
                     <<"listid">> := ListId}, _, Headers, InstanceName) ->
    case is_logged_in(Headers, InstanceName) of
        false ->
            render_not_logged_in();
        _Username ->
            update_song(?b2l(ListId), ?b2l(Id), ?b2l(Title)),
            #{<<"ok">> => <<"complete">>,
              <<"new_name">> => Title}
    end.

%% ---- helpers:

db_name(InstanceName) ->
    list_to_atom(atom_to_list(?MODULE)      ++
                 atom_to_list(InstanceName) ++
                 "login_tracker").

render_not_logged_in() ->
    {ok, Module} = erlydtl:compile_file("pages/not_logged_in.dtl",
                                        not_logged_in,
                                        [{out_dir, "compiled_templates"}]
                                       ),
    {ok, Binary} = Module:render([{header, <<"Login example">>}]),
    Binary.

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
    Appsecret = get_appsecret(),
    crypto:hash(sha512, String ++ Appsecret).

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

check_login(Username, PlainPassword, InstanceName) ->
    Password = hash_salt(PlainPassword),
    case get_user(Username) of
        {atomic, []} -> login_fail;
        {atomic, [#user{info = #userinfo{password = DBPassword}}]} ->
            case Password == DBPassword of
                false -> login_fail;
                true  ->
                    add_to_active_users(Username, InstanceName),
                    HtmlEncode = encrypt(Username),
                    C = "Set-Cookie: username=" ++ HtmlEncode ++ "\r\n",
                    {login_ok, C}
            end
    end.

maybe_change_pw(Username, OldPw, NewPw, NewPw) ->
    Password = hash_salt(OldPw),
    case get_user(Username) of
        {atomic, []} -> change_pw_fail;
        {atomic, [#user{info = #userinfo{password = DBPassword}=UserInfo}=User
                 ]} ->
            case Password == DBPassword of
                false -> change_pw_fail;
                true  ->
                    %% Actually change pw
                    NewPassword = hash_salt(NewPw),
                    NewUserInfo = UserInfo#userinfo{password = NewPassword},
                    NewUser = User#user{info=NewUserInfo},
                    %% overwrite userinfo
                    put_obj(NewUser),
                    change_pw_ok
            end
    end;
maybe_change_pw(_Username, _OldPw, _NewPw, _NewPw2) ->
    change_pw_fail.

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
    APIKey    = get_soundcloudkey(),
    QueryLink = "https://api.soundcloud.com/resolve.json?url=" ++ Link ++
                "&client_id=" ++ APIKey,
    {ok, {_HTTPVer, _Headers, Response}}
        = httpc:request(get, {QueryLink, []}, [], []),
    [_, TitleEtc] = re:split(Response, "title\":\""),
    [Title | _]   = re:split(?b2l(TitleEtc), "\","),
    ?b2l(Title).

%% other used to be "no_id", but that was not very good since
%% it is used as a key in the db. When changing the "song name"
%% you would change it for some other song in the list if you
%% had multiple "no_id" keys
get_id(Link) ->
    case determine_source(Link) of
        youtube    -> youtube_id(Link);
        soundcloud ->
            try
                soundcloud_id(Link)
            catch
                _:_ ->
                    uuid:uuid_to_string(uuid:get_v4())
            end;
        other      -> uuid:uuid_to_string(uuid:get_v4())
    end.

youtube_id(Link) ->
    [_, VIDEtc] = re:split(Link, "v="),
    [VID | _]   = re:split(?b2l(VIDEtc), "&"),
    ?b2l(VID).

soundcloud_id(Link) ->
    APIKey    = get_soundcloudkey(),
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
                [{"\\+",    " "}, %% Rules for how to remove html encoded
                 {"%28",    "("},
                 {"%29",    ")"},
                 {"%C3%A4", "ä"},
                 {"%C3%B6", "ö"},
                 {"%2B",    "+"}
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

update_song(ListId, TrackId, Title) ->
    PlayList   = playlist_get(ListId),
    PrevTracks = PlayList#playlist.tracks,
    OldTrack = lists:keyfind(TrackId, #track.id, PrevTracks),
    NewTrack = OldTrack#track{title=Title},
    NewTracks = lists:keyreplace(TrackId, #track.id,
                                 PrevTracks, NewTrack),
    NewList = PlayList#playlist{tracks = NewTracks},
    put_obj(NewList).

delete_song(ListId, TrackId) ->
    PlayList   = playlist_get(ListId),
    PrevTracks = PlayList#playlist.tracks,
    NewTracks = lists:keydelete(TrackId, #track.id, PrevTracks),
    NewList = PlayList#playlist{tracks = NewTracks},
    put_obj(NewList).

is_logged_in(Headers, InstanceName) ->
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
                    case is_active_user(Decrypted, InstanceName) of
                        true ->
                            Decrypted;
                        false ->
                            false
                    end
            end
    end.

is_active_user(Username, InstanceName) ->
    DB = db_name(InstanceName),
    ActiveUsers = ets:lookup(DB, active_users),
    case ActiveUsers of
        [] ->
            false;
        [{active_users, Ls}] ->
            lists:member(Username, Ls)
    end.

add_to_active_users(Username, InstanceName) ->
    DB = db_name(InstanceName),
    ActiveUsers = ets:lookup(DB, active_users),
    case ActiveUsers of
        [] ->
            ets:insert(DB, {active_users, [Username]});
        [{active_users, Ls}] ->
            ets:insert(DB, {active_users, [Username|Ls]})
    end.

delete_from_active_users(Username, InstanceName) ->
    DB = db_name(InstanceName),
    ActiveUsers = ets:lookup(DB, active_users),
    case ActiveUsers of
        [] ->
            should_not_happen;
        [{active_users, Ls}] ->
            NewUsers = Ls -- [Username],
            ets:insert(DB, {active_users, NewUsers})
    end.

encrypt(PlainText) ->
    Key = get_cryptkey(),
    Iv = crypto:strong_rand_bytes(16),
    En = crypto:block_encrypt(aes_cfb128, Key, Iv, PlainText),
    B64 = base64:encode(<<Iv:16/binary, En/binary>>),
    http_uri:encode(binary_to_list(B64)).

decrypt(HtmlEncode) ->
    Key = get_cryptkey(),
    HtmlUnencode = list_to_binary(http_uri:decode(HtmlEncode)),
    UnB64 = base64:decode(HtmlUnencode),
    case UnB64 of
        <<Iv:16/binary, CryptoText/binary>> ->
            Dec = crypto:block_decrypt(aes_cfb128, Key, Iv, CryptoText),
            binary_to_list(Dec);
        <<>> ->
            ""
    end.

get_appsecret() ->
    {ok, [#{appsecret := X}]} = file:consult("keys.txt"),
    X.

get_cryptkey() ->
    {ok, [#{cryptkey := X}]} = file:consult("keys.txt"),
    X.

get_soundcloudkey() ->
    {ok, [#{soundcloud := X}]} = file:consult("keys.txt"),
    X.
