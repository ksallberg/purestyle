-module(musiklistan).
-export([put_kv/2, get_v/1]).

-define(b2l, binary_to_list).
-define(BUCKET, <<"my bucket">>).
-define(localhost, "127.0.0.1").
-define(RIAK_PORT, 8087).

riak_connect() ->
    {ok, Pid} = riakc_pb_socket:start_link(?localhost, ?RIAK_PORT),
    Pid.

riak_disconnect(Pid) ->
    riakc_pb_socket:stop(Pid).

put_kv(Key, Value) ->
    Pid       = riak_connect(),
    Object    = riakc_obj:new(?BUCKET,
                              Key,
                              Value),
    riakc_pb_socket:put(Pid, Object, [{w, 2}, {dw, 1}, return_body]),
    riak_disconnect(Pid).

get_v(Key) ->
    Pid = riak_connect(),
    {ok, {riakc_obj, _Bucket, _Key, _Bin, [{_Riak, Value}], _, _}}
        = riakc_pb_socket:get(Pid, ?BUCKET, Key),
    riak_disconnect(Pid),
    Value.
