-module(pollution_server_test).
-author("olliekrk").

-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

-define(DATE_TIME, {{0, 0, 0}, {0, 0, 0}}).

startServer_test() ->
  pollution_server:start(),
  ?assert(lists:member(server, registered())),
  ?assert(lists:member(server, global:registered_names())).

addStation_test() ->
  ?assertEqual(ok, pollution_server:addStation("Warszawa", {0, 0})),
  ?assertEqual(ok, pollution_server:addStation("Krakow", {0, 1})),
  ?assertEqual(ok, pollution_server:addStation("Gdynia", {1, 0})),
  ?assertEqual(ok, pollution_server:addStation("Zakopane", {9, 9})),
%%  ?assertException(_, _, pollution_server:addStation("Zakopane", {9,8})),
  ?assertEqual(ok, pollution_server:addStation("Wadowice", {1, 1})).

addValue_test() ->
  ?assertEqual(ok, pollution_server:addValue("Warszawa", ?DATE_TIME, pm10, 100.0)),
  ?assertEqual(ok, pollution_server:addValue("Krakow", ?DATE_TIME, pm10, 90.0)),
  ?assertEqual(ok, pollution_server:addValue({1, 0}, ?DATE_TIME, pm10, 80.0)),
  ?assertEqual(ok, pollution_server:addValue({9, 9}, ?DATE_TIME, pm10, 70.0)),
%%  ?assertException(_, _, pollution_server:addValue({9,9}, ?DATE_TIME, pm10, 99.0})),
  ?assertEqual(ok, pollution_server:addValue("Wadowice", ?DATE_TIME, pm10, 0.0)).

-define(MEASUREMENT(Value), {measurement, pm10, Value, ?DATE_TIME}).

getOneValue_test() ->
  ?assertEqual({response, ?MEASUREMENT(100.0)}, pollution_server:getOneValue({0, 0}, ?DATE_TIME, pm10)),
  ?assertEqual({response, ?MEASUREMENT(90.0)}, pollution_server:getOneValue({0, 1}, ?DATE_TIME, pm10)),
  ?assertEqual({response, ?MEASUREMENT(80.0)}, pollution_server:getOneValue("Gdynia", ?DATE_TIME, pm10)),
  ?assertEqual({response, ?MEASUREMENT(70.0)}, pollution_server:getOneValue("Zakopane", ?DATE_TIME, pm10)),
  ?assertEqual({response, ?MEASUREMENT(0.0)}, pollution_server:getOneValue("Wadowice", ?DATE_TIME, pm10)).

removeValue_test() ->
  ?assertEqual(ok, pollution_server:removeValue("Warszawa", ?DATE_TIME, pm10)),
  ?assertEqual(ok, pollution_server:removeValue("Krakow", ?DATE_TIME, pm10)),
  ?assertEqual(ok, pollution_server:removeValue({1, 0}, ?DATE_TIME, pm10)),
  ?assertEqual(ok, pollution_server:removeValue({9, 9}, ?DATE_TIME, pm10)),
  ?assertEqual(ok, pollution_server:removeValue("Wadowice", ?DATE_TIME, pm10)).

stopServer_test() ->
  pollution_server:stop(),
  timer:sleep(10),
  ?assert(not lists:member(server, registered())),
  ?assert(not lists:member(server, global:registered_names())).