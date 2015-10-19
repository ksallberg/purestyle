-module(musiklistan).

-export([start/0,
         reg_user/2,
         check_login/2,
         playlist_create/2,
         playlists_get/1,
         playlist_get/1,
         add_track/2,
         get_users/0,
         add_playlist_to_user/2,
         leave_list/2
        ]).

-include("common.hrl").

start() ->
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
        {atomic, [#user{info = #userinfo{password = DBPassword}}]}  ->
            case Password == DBPassword of
                false -> login_fail;
                true  ->
                    CookieRecord = #usercookie{username = Username},
                    Cookie       = yaws_api:new_cookie_session(CookieRecord),
                    C            = yaws_api:set_cookie("usersession",
                                                       Cookie,
                                                       [{path, "/"}]),
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
