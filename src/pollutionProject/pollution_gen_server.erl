-module(pollution_gen_server).
-author("olliekrk").

-behaviour(gen_server).

%% pollution imports
-import(pollution, [createMonitor/0, addStation/3, addValue/5, getOneValue/4]).

%% API
-export([start/0, stop/0, addStation/2, addValue/4, getOneValue/3, crash/0]).

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
    _ ->
      {reply, "Unrecognized request", State}
  end.

handle_cast(Request, State) ->
  %%{noreply, NewLoopData}
  case Request of
    {add_station, Name, Location} ->
      {noreply, pollution:addStation(Name, Location, State)};
    {add_value, Identifier, Datetime, Param, Value} ->
      {noreply, pollution:addValue(Identifier, Datetime, Param, Value, State)};
    stop ->
      terminate(stop_received, State);
    crash ->
      ErrorVar = 1 / 0,
      io:format("Error: ~p~n", [ErrorVar]);
    _ ->
      {reply, "Unrecognized request", State}
  end.

terminate(_Reason, _State) ->
  ok.

addStation(Name, Location) ->
  gen_server:cast(pollution_server1, {add_station, Name, Location}).

addValue(Identifier, Datetime, Param, Value) ->
  gen_server:cast(pollution_server1, {add_value, Identifier, Datetime, Param, Value}).

getOneValue(Identifier, Datetime, Param) ->
  gen_server:call(pollution_server1, {get_value, Identifier, Datetime, Param}).
