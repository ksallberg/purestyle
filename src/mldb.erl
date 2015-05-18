%% Musiklistan DB proxy

-module(mldb).
-export([put_kv/3, get_v/2]).

-define(b2l, binary_to_list).
-define(localhost, "127.0.0.1").
-define(RIAK_PORT, 8087).

riak_connect() ->
    {ok, Pid} = riakc_pb_socket:start_link(?localhost, ?RIAK_PORT),
    Pid.

riak_disconnect(Pid) ->
    riakc_pb_socket:stop(Pid).

put_kv(Bucket, Key, Value) ->
    Pid       = riak_connect(),
    Object    = riakc_obj:new(Bucket,
                              Key,
                              Value),
    riakc_pb_socket:put(Pid, Object, [{w, 2}, {dw, 1}, return_body]),
    riak_disconnect(Pid).

get_v(Bucket, Key) ->
    Pid = riak_connect(),
    RetVal = case riakc_pb_socket:get(Pid, Bucket, Key) of
        {ok, {riakc_obj, _Bucket, _Key, _Bin, [{_Riak, Value}], _, _}} ->
            Value;
        _ ->
            no_such_key
    end,
    riak_disconnect(Pid),
    RetVal.
