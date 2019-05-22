-module(pollution_gen_server).
-author("olliekrk").

-behaviour(gen_server).

%% pollution imports
-import(pollution, [createMonitor/0, addStation/3, addValue/5, getOneValue/4]).

%% API
-export([start/0, stop/0, addStation/2, addValue/4, getOneValue/3, crash/0,
  removeValue/3, getStationMean/2, getDailyMean/2, getMaximumStationGrowthTime/3, getMonitorState/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).

-record(monitor, {locations_map, stations_by_names, data}).

start() ->
  [{lastState, State}] = ets:lookup(backup, lastState),
  gen_server:start_link({local, pollution_server1}, ?MODULE, State, []).

stop() ->
  gen_server:cast(pollution_server1, stop).

crash() ->
  gen_server:cast(pollution_server1, crash).

init(Monitor) ->
  {ok, Monitor}.

process_cast(NewState, _OldState) when is_record(NewState, monitor) ->
  {noreply, NewState};

process_cast(_, OldState) ->
  {noreply, OldState}.

process_call({error, _}, Monitor) ->
  {reply, error, Monitor};

process_call(Result, Monitor) ->
  {reply, Result, Monitor}.

handle_call(Request, _From, State) ->
  %%{reply, Reply, NewLoopData}
  case Request of
    {get_value, Identifier, Datetime, Param} ->
      process_call(pollution:getOneValue(Identifier, Datetime, Param, State), State);

    {get_station_mean, Identifier, Param} ->
      process_call(pollution:getStationMean(Identifier, Param, State), State);

    {get_daily_mean, Date, Param} ->
      process_call(pollution:getDailyMean(Date, Param, State), State);

    {get_max_growth, Identifier, Date, Param} ->
      process_call(pollution:getMaximumStationGrowthTime(Identifier, Date, Param, State), State);

    {get_state} ->
      process_call(State, State);

    _ ->
      {reply, unrecognized_request, State}
  end.

handle_cast(Request, State) ->
  %%{noreply, NewLoopData}
  case Request of
    {add_station, Name, Location} ->
      process_cast(pollution:addStation(Name, Location, State), State);
    {add_value, Identifier, Datetime, Param, Value} ->
      process_cast(pollution:addValue(Identifier, Datetime, Param, Value, State), State);
    {remove_value, Identifier, Datetime, Param} ->
      process_cast(pollution:removeValue(Identifier, Datetime, Param, State), State);
    stop ->
      {stop, normal, State};
    crash ->
      ErrorVar = 1 / 0,
      io:format("Error: ~p~n", [ErrorVar]);
    _ ->
      {noreply, State}
  end.

terminate(Reason, LoopData) ->
  ets:insert(backup, [{lastState, LoopData}]),
  io:format("Generic server terminated, state saved ~n", []),
  Reason.

addStation(Name, Location) ->
  gen_server:cast(pollution_server1, {add_station, Name, Location}).

addValue(Identifier, Datetime, Param, Value) ->
  gen_server:cast(pollution_server1, {add_value, Identifier, Datetime, Param, Value}).

removeValue(Identifier, Datetime, Param) ->
  gen_server:cast(pollution_server1, {remove_value, Identifier, Datetime, Param}).

getOneValue(Identifier, Datetime, Param) ->
  gen_server:call(pollution_server1, {get_value, Identifier, Datetime, Param}).

getStationMean(Identifier, Param) ->
  gen_server:call(pollution_server1, {get_station_mean, Identifier, Param}).

getDailyMean(Date, Param) ->
  gen_server:call(pollution_server1, {get_daily_mean, Date, Param}).

getMaximumStationGrowthTime(Identifier, Date, Param) ->
  gen_server:call(pollution_server1, {get_max_growth, Identifier, Date, Param}).

getMonitorState() ->
  gen_server:call(pollution_server1, {get_state}).