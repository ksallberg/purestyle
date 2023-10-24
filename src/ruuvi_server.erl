-module(ruuvi_server).

-behaviour(gen_server).

-export([code_change/3,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         init/1,
         start_link/0,
         terminate/2]).

-include("common.hrl").

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    Millisec = 1000,
    Timeout = 60 * 5 * Millisec,
    lager:log(info, self(), "Ruuvi monitor server started", []),
    {ok, TRef} = timer:send_interval(Timeout, ask_ruuvi),
    {ok, TRef}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(ask_ruuvi, State) ->
    Res = os:cmd("./get_ruuvi.py"),
    {Date, Time} = erlang:localtime(),
    Parsed = parse_ruuvi(Res, Date, Time),

    Fun = fun() ->
                  mnesia:write(Parsed)
          end,
    {atomic, ok} = mnesia:transaction(Fun),

    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, TRef) ->
    {ok, cancel} = timer:cancel(TRef),
    ok.

parse_ruuvi([${|Rest], Date, Time) ->
    {ToParse, _Discard0} = lists:split(length(Rest)-2, Rest),
    ["'data_format':", DataFormat,
     "'humidity':", Humidity,
     "'temperature':", Temperature,
     "'pressure':", Pressure,
     _, Acc,
     _, AccX,
     _, AccY,
     _, AccZ,
     _, TxPower,
     _, Battery,
     _, MovementCounter,
     _, MeasurementSequenceNumber,
     _, _Mac,
     _, RSSI
    ] = string:lexemes(ToParse, ", "),
    #ruuvidata{datetime = {Date, Time},
               data_format = list_to_integer(DataFormat),
               humidity = list_to_float(Humidity),
               temperature = list_to_float(Temperature),
               pressure = list_to_float(Pressure),
               acceleration = list_to_float(Acc),
               acceleration_x = list_to_integer(AccX),
               acceleration_y = list_to_integer(AccY),
               acceleration_z = list_to_integer(AccZ),
               tx_power = list_to_integer(TxPower),
               battery = list_to_integer(Battery),
               movement_counter = list_to_integer(MovementCounter),
               measurement_sequence_number =
                   list_to_integer(MeasurementSequenceNumber),
               'rssi' = list_to_integer(RSSI)}.
