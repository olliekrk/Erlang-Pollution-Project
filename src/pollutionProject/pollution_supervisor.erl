-module(pollution_supervisor).
-author("olliekrk").

-behaviour(supervisor).

-import(pollution_gen_server, [start/0]).

%% API
-export([start_supervisor/0, init/1, stop/0]).


start_supervisor() ->
  supervisor:start_link({local, pollution_supervisor1}, ?MODULE, []).

init(_Args) ->
  RestartTuple = {one_for_one, 10, 1},
  ChildSpecList = [{server1, {pollution_gen_server, start, []}, permanent, infinity, worker, [pollution_gen_server]}],
  SupervisorSpec = {RestartTuple, ChildSpecList},
  {ok, SupervisorSpec}.

stop() ->
  ok.