-module(ruuvi_server).

-behaviour(gen_server).

-export([code_change/3,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         init/1,
         start_link/0,
         terminate/2,
         export_data/0
        ]).

-include("common.hrl").

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    Millisec = 1000,
    Seconds = 60,
    Minutes = 30,
    Timeout = Seconds * Minutes * Millisec,
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
               data_format = maybe_list_to_int(DataFormat),
               humidity = maybe_list_to_float(Humidity),
               temperature = maybe_list_to_float(Temperature),
               pressure = maybe_list_to_float(Pressure),
               acceleration = maybe_list_to_float(Acc),
               acceleration_x = maybe_list_to_int(AccX),
               acceleration_y = maybe_list_to_int(AccY),
               acceleration_z = maybe_list_to_int(AccZ),
               tx_power = maybe_list_to_int(TxPower),
               battery = maybe_list_to_int(Battery),
               movement_counter = maybe_list_to_int(MovementCounter),
               measurement_sequence_number =
                   maybe_list_to_int(MeasurementSequenceNumber),
               'rssi' = maybe_list_to_int(RSSI)}.

maybe_list_to_float("None") ->
    0.0;
maybe_list_to_float(Otherwise) ->
    list_to_float(Otherwise).

maybe_list_to_int("None") ->
    0;
maybe_list_to_int(Otherwise) ->
    list_to_integer(Otherwise).

ruuvi_to_str(#ruuvidata{datetime = {Date, Time},
                        data_format = _DataFormat,
                        humidity = _Humidity,
                        temperature = Temperature,
                        pressure = _Pressure,
                        tx_power = _TxPower,
                        battery = _Battery,
                        measurement_sequence_number =
                            _MeasurementSequenceNumber,
                        'rssi' = _RSSI}) ->
    {Year, Month, Day} = Date,
    {Hour, Min, Sec} = Time,
    DateStr = io_lib:format("~B-~B-~B ~B:~B:~B", [Year, Month, Day,
                                                  Hour, Min, Sec]),
    TempStr = io_lib:format("~.2f", [Temperature]),
    io:lib_format("~s,~s\n", [DateStr, TempStr]).

export_data() ->
    All = fun() ->
              mnesia:foldr(fun(RuuviData, Acc) ->
                                   [RuuviData|Acc]
                           end,
                           [],
                           ruuvidata)
    end,
    {atomic, Data} = mnesia:transaction(All),
    FormattedData = lists:map(fun ruuvi_to_str/1, Data),
    FormattedData.
