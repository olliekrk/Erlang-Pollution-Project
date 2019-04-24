-module(pollution_server).
-author("olliekrk").
%% komentarz dla zmiany kodu
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
-export([start/0, stop/0, server_loop/1]).

%% initializes pollution server
init() ->
  server_loop(createMonitor()).

%% creates a server and returns itd PID
start() ->
  Pid = spawn(fun() -> init() end),
  ResolveFun = fun(server, _, _) -> Pid end,
  global:register_name(server, Pid, ResolveFun),
  io:format("Starting the server with PID: ~w.", [Pid]).

%% server shutdown
stop() ->
  global:send(server, stop).

%% server loop
server_loop(M) ->
  receive
    {_, add_station, Name, Location} ->
      server_loop(addStation(Name, Location, M)), ok;

    {_, add_value, Identifier, Datetime, Param, Value} ->
      server_loop(addValue(Identifier, Datetime, Param, Value, M)), ok;

    {_, remove_value, Identifier, Datetime, Param} ->
      server_loop(removeValue(Identifier, Datetime, Param, M)), ok;

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
      io:format("Shutting down the server with PID: ~w.~n", [self()]), ok;

    _ ->
      io:format("Unrecognized request.~n"), server_loop(M)

  after
    20000 ->
      io:format("Inactivity for 20 s.~n"),
      io:format("Shutting down the server with PID: ~w.~n", [self()]), ok

  end.