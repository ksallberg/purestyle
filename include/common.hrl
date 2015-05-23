-define(l2b, list_to_binary).
-define(b2l, binary_to_list).

-record(usercookie, {username, times = 0}).

-record(track, {source=other, id="no_id", url="no_url", title="no_title"}).
