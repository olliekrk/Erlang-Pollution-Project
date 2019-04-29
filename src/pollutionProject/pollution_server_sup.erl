-module(pollution_server_sup).
-author("olliekrk").

-import(pollution_server, [start/0]).

%% API
-export([start_supervisor/0, supervisor_loop/0]).

start_supervisor() ->
  spawn_link(pollution_server_sup, supervisor_loop, []).

supervisor_loop() ->
  process_flag(trap_exit, true),
  pollution_server:start(),
  receive
    {'EXIT', Pid, Reason} ->
      io:format("Server process with PID: ~p has crashed with reason: ~p~n", [Pid, Reason]),
      io:format("Restarting the server...~n")
  end,
  supervisor_loop().