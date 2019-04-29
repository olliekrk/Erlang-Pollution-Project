-module(pollution_server).
-author("olliekrk").

%% pollution
-import(pollution,
[createMonitor/0,
addStation/3,
addValue/5,
removeValue/4,
getOneValue/4,
getStationMean/3,
getDailyMean/3,
getMaximumStationGrowthTime/4,
test/0
]).

%% API
-export([start/0, stop/0, crash/0, server_loop/1]).

%% initializes pollution server
init() ->
  server_loop(createMonitor()).

%% creates a server and returns itd PID
start() ->
  Pid = spawn_link(fun init/0),
  ResolveFun = fun(server, _, _) -> Pid end,
  global:register_name(server, Pid, ResolveFun),
  io:format("Starting new server with PID: ~p~n", [Pid]),
  Pid.

%% server shutdown
stop() ->
  global:send(server, stop).

%% server crash
crash() ->
  global:send(server, crash).

%% server loop
server_loop(M) ->
  receive
    {_, add_station, Name, Location} ->
      server_loop(addStation(Name, Location, M));

    {_, add_value, Identifier, Datetime, Param, Value} ->
      server_loop(addValue(Identifier, Datetime, Param, Value, M));

    {_, remove_value, Identifier, Datetime, Param} ->
      server_loop(removeValue(Identifier, Datetime, Param, M));

    {Pid, get_value, Identifier, Datetime, Param} ->
      Pid ! {response, getOneValue(Identifier, Datetime, Param, M)},
      server_loop(M);

    {Pid, get_station_mean, Identifier, Datetime} ->
      Pid ! {response, getStationMean(Identifier, Datetime, M)},
      server_loop(M);

    {Pid, get_daily_mean, Date, Param} ->
      Pid ! {response, getDailyMean(Date, Param, M)},
      server_loop(M);

    {Pid, get_max_growth, Identifier, Date, Param} ->
      Pid ! {response, getMaximumStationGrowthTime(Identifier, Date, Param, M)},
      server_loop(M);

    stop ->
      io:format("Shutting down the server with PID: ~p~n", [self()]), ok;

    crash ->
      ErrorVar = 1 / 0,
      io:format("Error: ~p~n.", [ErrorVar]),
      server_loop(M);

    _ ->
      io:format("Unrecognized request.~n"),
      server_loop(M)

  after
    60000 ->
      io:format("Inactivity for 60 s~n"),
      io:format("Shutting down the server with PID: ~p~n", [self()]), ok

  end.