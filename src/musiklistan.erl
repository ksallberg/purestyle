-module(musiklistan).

-export([start/0,
         reg_user/2,
         check_login/2,
         playlist_create/2,
         playlists_get/1,
         playlist_get/2,
         add_song/3,
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
%% --------------------------------------------------------
%% | username_listname | urls                             |
%% --------------------------------------------------------
%% | someuser_rock     | [http://www.youtube.com/jwdaijd] |
%% --------------------------------------------------------
-record(list, {username_listname,
               urls}).

-export([get_user/1]).

-include("common.hrl").

start() ->
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
    io:format("starting mnesia").

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

playlist_get(Username, Playlist) ->
    F = fun() ->
            mnesia:select(list, [{#list{username_listname = '$1',
                                        urls = '$2'},
                                 [{'==', '$1', Username ++ Playlist}],
                                 ['$2']
                                }])
        end,
    case mnesia:transaction(F) of
        {atomic, []}   -> [];
        {atomic, [Ls]} -> Ls
    end.

add_song(Username, Playlist, Song) ->
    PrevSongs = playlist_get(Username, Playlist),
    List = #list{username_listname=Username ++ Playlist,
                 urls=PrevSongs++[Song]},
    put_obj(List).
