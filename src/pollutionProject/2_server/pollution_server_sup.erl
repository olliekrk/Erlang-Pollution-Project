-module(pollution_server_sup).
-author("olliekrk").

-import(pollution_server, [start/0]).

%% API
-export([start_supervisor/0, supervisor_init/0, supervisor_loop/0, stop_supervisor/0]).

start_supervisor() ->
  Pid = spawn_link(pollution_server_sup, supervisor_init, []),
  register(server_sup, Pid).

supervisor_init() ->
  process_flag(trap_exit, true),
  pollution_server:start(),
  supervisor_loop().

supervisor_loop() ->
  receive
    {'EXIT', Pid, Reason} ->
      io:format("Server process with PID: ~p has crashed with reason: ~p~n", [Pid, Reason]),
      io:format("Restarting the server...~n"),
      supervisor_init();
    stop ->
      io:format("Shutting down the supervisor.~n"),
      exit(self(), shutdown);
    _ ->
      io:format("Unrecognized request for server_sup.~n"),
      supervisor_loop()
  end.

stop_supervisor() ->
  server_sup ! stop.