-module(musiklistan).
-export([put_kv/2, get_v/1]).

-define(b2l, binary_to_list).
-define(BUCKET, <<"my bucket">>).
-define(localhost, "127.0.0.1").
-define(RIAK_PORT, 8087).

put_kv(Key, Value) ->
    {ok, Pid} = riakc_pb_socket:start_link(?localhost, ?RIAK_PORT),
    Object    = riakc_obj:new(?BUCKET,
                              Key,
                              Value),
    riakc_pb_socket:put(Pid, Object, [{w, 2}, {dw, 1}, return_body]).

get_v(Key) ->
    {ok, Pid} = riakc_pb_socket:start_link(?localhost, ?RIAK_PORT),
    {ok, {riakc_obj, _Bucket, _Key, _Bin, [{_Riak, Value}], _, _}}
        = riakc_pb_socket:get(Pid, ?BUCKET, Key),
    Value.
