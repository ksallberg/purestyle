-define(l2b, list_to_binary).
-define(b2l, binary_to_list).

-record(usercookie, {username :: string(),
                     times = 0}).

%% 1) All users bucket
%%
%% -------------------
%% | username | info |
%% -------------------
%% | someuser | a    |
%% | user2    | b    |
%% -------------------
-record(user, {username :: string(),
               info     :: userinfo}).

-record(userinfo, {password  :: string(),
                   playlists :: [playlist]}).

%% 2) All playlists: (example bucket name: user2_lists)
%%
%% tracks is a list of strings (id) that corresponds
%% to track.id
%% --------------------------
%% | id     | name | tracks |
%% --------------------------
%% | 34433  | hest | [a, b] |
%% --------------------------
-record(playlist, {id     :: string(),
                   name   :: string(),
                   tracks :: [string()]}).

-record(track, {source = other,
                id     = "no_id",
                url    = "no_url",
                title  = "no_title"}).
