-define(l2b, list_to_binary).
-define(b2l, binary_to_list).

%%% 1) All users bucket
%% -----------------------
%% | username | password |
%% -----------------------
%% | someuser | ******   |
%% | user2    | ***      |
%% -----------------------
-record(user, {username :: string(),
               info     :: userinfo}).

%%% 2) All lists of a user: (example bucket name: user2_lists)
%% ----------------------------
%% | id     | playlists       |
%% ----------------------------
%% | 34433  | [rock, house]   |
%% ----------------------------
-record(playlist, {id     :: string(),
                   name   :: string(),
                   tracks :: [track]}).

-record(usercookie, {username, times = 0}).

-record(userinfo, {password, playlists :: [string()]}).

-record(track, {source=other,
                id="no_id",
                url="no_url",
                title="no_title"}).
