-module(pollution_server_sup).
-author("olliekrk").

-import(pollution_server, [start/0]).

%% API
-export([start_sup/0]).

start_sup() ->
  process_flag(trap_exit, true),
  start_server().

sup_loop() ->
  process_flag(trap_exit, true),
  receive
    {'EXIT', Pid, Reason} ->
      io:format("Server process with PID: ~p has crashed with reason: ~p~n", [Pid, Reason]),
      io:format("Restarting the server...~n"),
      start_server()
  end,
  sup_loop().

start_server() ->
  Pid = pollution_server:start(),
  io:format("~p: Started supervising PID: ~p~n", [?MODULE, Pid]),
  spawn_link(fun sup_loop/0).