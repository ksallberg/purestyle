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

-record(track, {source = other,
                id     = "no_id",
                url    = "no_url",
                title  = "no_title"}).

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
                   tracks :: [#track{}]}).

-record(public_playlist, {id :: string(), retracted :: boolean()}).

%% Temperature ruuvitag
-record(ruuvidata, {datetime :: {{integer(), integer(), integer()},
                                 {integer(), integer(), integer()}},
                    data_format :: integer(),
                    humidity :: float(),
                    temperature :: float(),
                    pressure :: float(),
                    acceleration :: float(),
                    acceleration_x :: integer(),
                    acceleration_y :: integer(),
                    acceleration_z :: integer(),
                    tx_power :: integer(),
                    battery :: integer(),
                    movement_counter :: integer(),
                    measurement_sequence_number :: integer(),
                    'rssi' :: integer()
                   }).
