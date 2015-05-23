-module(musiklistan).

-export([start/0,
         reg_user/2,
         check_login/2,
         playlist_create/2,
         playlists_get/1,
         playlist_get/2,
         add_track/3,
         get_users/0
        ]).

%%% 1) All users bucket
%% -----------------------
%% | username | password |
%% -----------------------
%% | someuser | ******   |
%% | user2    | ***      |
%% -----------------------
-record(user, {username,
               password}).

%%% 2) All lists of a user: (example bucket name: user2_lists)
%% ----------------------------
%% | username | listnames     |
%% ----------------------------
%% | user2  | [rock]          |
%% | user2  | [techno, house] |
%% ----------------------------
-record(user_lists, {username,
                     listnames}).

%% 3) Actual list (associated with a user) bucket: user2_list_rock
%% ---------------------------------------------
%% | username_listname | tracks                |
%% ---------------------------------------------
%% | someuser_rock     | [Track, AnotherTrack] |
%% ---------------------------------------------
-record(list, {username_listname,
               tracks}).

-export([get_user/1]).

-include("common.hrl").

start() ->
    io:format("Starting mnesia..."),
    NodeList = [node()],
    mnesia:create_schema(NodeList),
    mnesia:start(),
    mnesia:create_table(user,
                        [{attributes, record_info(fields, user)},
                         {disc_copies, NodeList}]),
    mnesia:create_table(user_lists,
                        [{attributes, record_info(fields, user_lists)},
                         {disc_copies, NodeList}]),
    mnesia:create_table(list,
                        [{attributes, record_info(fields, list)},
                         {disc_copies, NodeList}]),
    io:format("Starting inets..."),
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
            mnesia:select(user, [{#user{username = '$1',
                                        password = '$2'},
                                 [{'==', '$1', Username}],
                                 ['$2']
                                }])
        end,
    mnesia:transaction(F).

put_obj(Obj) ->
    Fun = fun() ->
              mnesia:write(Obj)
          end,
    mnesia:transaction(Fun).

reg_user(Username, Password) ->
    User = #user{username=Username, password=Password},
    case get_user(Username) of
        {atomic, []} ->
            put_obj(User),
            user_registered;
        _ ->
            user_already_existing
    end.

check_login(Username, Password) ->
    case get_user(Username) of
        {atomic, []} -> login_fail;
        {atomic, [DBPassword]}  ->
            case Password == DBPassword of
                false -> login_fail;
                true  ->
                    CookieRecord = #usercookie{username = Username},
                    Cookie       = yaws_api:new_cookie_session(CookieRecord),
                    C = yaws_api:set_cookie("usersession",
                                            Cookie,
                                            [{path, "/"}]),
                    {login_ok, C}
            end
    end.

playlist_create(Username, Playlist) ->
    PrevLists = playlists_get(Username),
    List = #user_lists{username  = Username,
                       listnames = PrevLists ++ [Playlist]},
    put_obj(List).

playlists_get(Username) ->
    F = fun() ->
            mnesia:select(user_lists, [{#user_lists{username  = '$1',
                                                    listnames = '$2'},
                                       [{'==', '$1', Username}],
                                       ['$2']
                                      }])
        end,
    case mnesia:transaction(F) of
        {atomic, []}   -> [];
        {atomic, [Ls]} -> Ls
    end.

get_title(Link) ->
    case string:str(Link, "youtube.com") of
        0 -> Link;
        _ -> try
                youtube_title(Link)
             catch _:Error ->
                io:format("Parsing youtube error: ~p", [Error]),
                Link
             end
    end.

%% Take a link, extract the title that youtube associates with
%% the title, return both the link and the title
youtube_title(Link) ->
    [_, VIDEtc] = re:split(Link, "v="),
    [VID | _]   = re:split(?b2l(VIDEtc), "&"),
    QueryLink   = "http://youtube.com/get_video_info?video_id=" ++ ?b2l(VID),
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

playlist_get(Username, Playlist) ->
    F = fun() ->
            mnesia:select(list, [{#list{username_listname = '$1',
                                        tracks = '$2'},
                                 [{'==', '$1', Username ++ Playlist}],
                                 ['$2']
                                }])
        end,
    case mnesia:transaction(F) of
        {atomic, []}   -> [];
        {atomic, [Ls]} -> Ls
    end.

add_track(Username, Playlist, URL) ->
    PrevTracks = playlist_get(Username, Playlist),
    NewTrack   = #track{url = URL, title = get_title(URL)},
    List = #list{username_listname = Username   ++ Playlist,
                 tracks            = PrevTracks ++ [NewTrack]},
    put_obj(List).
