-module(pollution_gen_server).
-author("olliekrk").

-behaviour(gen_server).

%% pollution imports
-import(pollution, [createMonitor/0, addStation/3, addValue/5, getOneValue/4]).

%% API
-export([start/0, stop/0, addStation/2, addValue/4, getOneValue/3, crash/0,
  removeValue/3, getStationMean/2, getDailyMean/2, getMaximumStationGrowthTime/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).

start() ->
  gen_server:start_link({local, pollution_server1}, ?MODULE, [], []).

stop() ->
  gen_server:cast(pollution_server1, stop).

crash() ->
  gen_server:cast(pollution_server1, crash).

init(_Args) ->
  {ok, pollution:createMonitor()}.

handle_call(Request, _From, State) ->
  %%{reply, Reply, NewLoopData}
  case Request of
    {get_value, Identifier, Datetime, Param} ->
      {reply, pollution:getOneValue(Identifier, Datetime, Param, State), State};

    {get_station_mean, Identifier, Datetime} ->
      {reply, pollution:getStationMean(Identifier, Datetime, State), State};

    {get_daily_mean, Date, Param} ->
      {reply, pollution:getDailyMean(Date, Param, State), State};

    {get_max_growth, Identifier, Date, Param} ->
      {reply, pollution:getMaximumStationGrowthTime(Identifier, Date, Param, State), State};

    _ ->
      {reply, unrecognized_request, State}
  end.

handle_cast(Request, State) ->
  %%{noreply, NewLoopData}
  case Request of
    {add_station, Name, Location} ->
      {noreply, pollution:addStation(Name, Location, State)};
    {add_value, Identifier, Datetime, Param, Value} ->
      {noreply, pollution:addValue(Identifier, Datetime, Param, Value, State)};
    {remove_value, Identifier, Datetime, Param} ->
      {noreply, pollution:removeValue(Identifier, Datetime, Param, State)};
    stop ->
      {stop, normal, State};
    crash ->
      ErrorVar = 1 / 0,
      io:format("Error: ~p~n", [ErrorVar]);
    _ ->
      {noreply, State}
  end.

terminate(Reason, LoopData) ->
  io:format("Generic server exited with state ~p~n", [LoopData]),
  Reason.

addStation(Name, Location) ->
  gen_server:cast(pollution_server1, {add_station, Name, Location}).

addValue(Identifier, Datetime, Param, Value) ->
  gen_server:cast(pollution_server1, {add_value, Identifier, Datetime, Param, Value}).

removeValue(Identifier, Datetime, Param) ->
  gen_server:cast(pollution_server1, {remove_value, Identifier, Datetime, Param}).

getOneValue(Identifier, Datetime, Param) ->
  gen_server:call(pollution_server1, {get_value, Identifier, Datetime, Param}).

getStationMean(Identifier, Datetime) ->
  gen_server:call(pollution_server1, {get_station_mean, Identifier, Datetime}).

getDailyMean(Date, Param) ->
  gen_server:call(pollution_server1, {get_daily_mean, Date, Param}).

getMaximumStationGrowthTime(Identifier, Date, Param) ->
  gen_server:call(pollution_server1, {get_max_growth, Identifier, Date, Param}).