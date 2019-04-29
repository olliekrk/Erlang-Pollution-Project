-module(pollutionTests).
-author("olliekrk").

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).
-export([]).

-record(station, {name, location}).
-record(measurement, {param, value, datetime}).
-record(monitor, {locations_map, stations_by_names, data}).

getStationByName_test() ->
  St = #station{name = "Testowa1", location = {1, 1}},
  M = pollution:createMonitor(),
  M1 = pollution:addStation(St#station.name, St#station.location, M),

  ?assertEqual(St, pollution:findStationByName(St#station.name, M1)),
  ?assertException(error, _, pollution:findStationByName("NULL", M1)).

getStationByLocation_test() ->
  St = #station{name = "Testowa1", location = {1, 1}},
  M = pollution:createMonitor(),
  M1 = pollution:addStation(St#station.name, St#station.location, M),

  ?assertEqual(St, pollution:findStationByLocation(St#station.location, M1)),
  ?assertException(error, _, pollution:findStationByLocation({-1, -1}, M1)).
% + exception throwing test


addStation_test() ->
  St1 = #station{name = "Testowa1", location = {1, 1}},
  St2 = #station{name = "Testowa2", location = {2, 2}},
  M = pollution:createMonitor(),
  M1 = pollution:addStation("Testowa1", {1, 1}, M),
  M2 = pollution:addStation("Testowa2", {2, 2}, M1),

  ?assertEqual(St1, pollution:findStationByName("Testowa1", M2)),
  ?assertEqual(St2, pollution:findStationByName("Testowa2", M2))
% + exception throwing test
.
