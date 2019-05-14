-module(pollution_test).
-author("olliekrk").

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

-record(station, {name, location}).
-record(measurement, {param, value, datetime}).
-record(monitor, {locations_map, stations_by_names, data}).
-define(DATE_TIME1, {{0, 0, 0}, {0, 0, 0}}).

findStationByName_test() ->
  St = #station{name = "Testowa1", location = {1, 1}},
  M = pollution:createMonitor(),
  M1 = pollution:addStation(St#station.name, St#station.location, M),

  ?assertEqual(St, pollution:findStationByName(St#station.name, M1)),
  ?assertException(error, _, pollution:findStationByName("NULL", M1)).

findStationByLocation_test() ->
  St = #station{name = "Testowa1", location = {1, 1}},
  M = pollution:createMonitor(),
  M1 = pollution:addStation(St#station.name, St#station.location, M),

  ?assertEqual(St, pollution:findStationByLocation(St#station.location, M1)),
  ?assertException(error, _, pollution:findStationByLocation({-1, -1}, M1)).

addStation_test() ->
  St1 = #station{name = "Testowa1", location = {1, 1}},
  St2 = #station{name = "Testowa2", location = {2, 2}},
  M = pollution:createMonitor(),
  M1 = pollution:addStation("Testowa1", {1, 1}, M),
  M2 = pollution:addStation("Testowa2", {2, 2}, M1),

  ?assert(maps:is_key(St1#station.name, M2#monitor.stations_by_names)),
  ?assert(maps:is_key(St1#station.location, M2#monitor.locations_map)),
  ?assert(maps:is_key(St2#station.name, M2#monitor.stations_by_names)),
  ?assert(maps:is_key(St2#station.location, M2#monitor.locations_map)),
  ?assertEqual(St1, pollution:findStationByName("Testowa1", M2)),
  ?assertEqual(St2, pollution:findStationByName("Testowa2", M2)),
  ?assertException(throw, _, pollution:addStation("Testowa2", {3, 3}, M2)).

addValue_test() ->
  St = #station{name = "Testowa", location = {0, 0}},
  V1 = #measurement{param = pm10, value = 90.0, datetime = ?DATE_TIME1},
  V2 = #measurement{param = pm10, value = 10.0, datetime = ?DATE_TIME1},

  M = pollution:addStation(St#station.name, St#station.location, pollution:createMonitor()),
  M1 = pollution:addValue("Testowa", V1#measurement.datetime, V1#measurement.param, V1#measurement.value, M),

  ?assert(lists:member(V1, maps:get(St, M1#monitor.data))),
  ?assertException(_, _, pollution:addValue("Testowa", V2#measurement.datetime, V2#measurement.param, V2#measurement.value, M1)).

removeValue_test() ->
  St = #station{name = "Testowa", location = {0, 0}},
  V1 = #measurement{param = pm10, value = 90.0, datetime = ?DATE_TIME1},
  M = pollution:addStation(St#station.name, St#station.location, pollution:createMonitor()),
  M1 = pollution:addValue(St#station.name, V1#measurement.datetime, V1#measurement.param, V1#measurement.value, M),
  M2 = pollution:removeValue(St#station.location, V1#measurement.datetime, V1#measurement.param, M1),

  ?assertNot(lists:member(V1, maps:get(St, M2#monitor.data))).

getOneValue_test() ->
  St = #station{name = "Testowa", location = {0, 0}},
  V1 = #measurement{param = pm10, value = 90.0, datetime = ?DATE_TIME1},
  M = pollution:addStation(St#station.name, St#station.location, pollution:createMonitor()),
  M1 = pollution:addValue("Testowa", V1#measurement.datetime, V1#measurement.param, V1#measurement.value, M),

  ?assertEqual(V1, pollution:getOneValue(St#station.location, V1#measurement.datetime, V1#measurement.param, M1)).

getStationMean_test() ->
  St = #station{name = "Testowa", location = {0, 0}},
  V1 = #measurement{param = pm10, value = 75.0, datetime = {{2020, 1, 1}, {6, 0, 0}}},
  V2 = #measurement{param = pm10, value = 25.0, datetime = {{2020, 1, 1}, {12, 0, 0}}},
  V3 = #measurement{param = pm10, value = 50.0, datetime = {{2020, 1, 1}, {18, 0, 0}}},
  M =
    pollution:addValue(St#station.name, V1#measurement.datetime, V1#measurement.param, V1#measurement.value,
      pollution:addValue(St#station.name, V2#measurement.datetime, V2#measurement.param, V2#measurement.value,
        pollution:addValue(St#station.name, V3#measurement.datetime, V3#measurement.param, V3#measurement.value,
          pollution:addStation(St#station.name, St#station.location,
            pollution:createMonitor())))),

  ?assertEqual(50.0, pollution:getStationMean(St#station.location, pm10, M)).

getDailyMean_test() ->
  St1 = #station{name = "Testowa1", location = {0, 0}},
  St2 = #station{name = "Testowa2", location = {0, 1}},
  St3 = #station{name = "Testowa3", location = {1, 0}},
  V1 = #measurement{param = pm10, value = 75.0, datetime = {{2020, 1, 1}, {6, 0, 0}}},
  V2 = #measurement{param = pm10, value = 25.0, datetime = {{2020, 1, 1}, {12, 0, 0}}},
  V3 = #measurement{param = pm10, value = 50.0, datetime = {{2020, 1, 1}, {18, 0, 0}}},

  M =
    pollution:addValue(St1#station.name, V1#measurement.datetime, V1#measurement.param, V1#measurement.value,
      pollution:addValue(St2#station.name, V1#measurement.datetime, V1#measurement.param, V1#measurement.value,
        pollution:addValue(St2#station.name, V2#measurement.datetime, V2#measurement.param, V2#measurement.value,
          pollution:addValue(St3#station.name, V1#measurement.datetime, V1#measurement.param, V1#measurement.value,
            pollution:addValue(St3#station.name, V2#measurement.datetime, V2#measurement.param, V2#measurement.value,
              pollution:addValue(St3#station.name, V3#measurement.datetime, V3#measurement.param, V3#measurement.value,
                pollution:addStation(St1#station.name, St1#station.location,
                  pollution:addStation(St2#station.name, St2#station.location,
                    pollution:addStation(St3#station.name, St3#station.location,
                      pollution:createMonitor()))))))))),

  ?assertEqual((75.0 + 50.0 + 50.0) / 3, pollution:getDailyMean({2020, 1, 1}, pm10, M)).

getMaximumStationGrowthTime_test() ->
  St1 = #station{name = "Testowa1", location = {0, 0}},
  St2 = #station{name = "Testowa2", location = {0, 1}},
  St3 = #station{name = "Testowa3", location = {1, 0}},
  St0 = #station{name = "Testowa0", location = {1, 1}},
  V1 = #measurement{param = pm10, value = 9.0, datetime = {{2020, 1, 1}, {6, 0, 0}}},
  V2 = #measurement{param = pm10, value = 99.0, datetime = {{2020, 1, 1}, {12, 0, 0}}},
  V3 = #measurement{param = pm10, value = 999.0, datetime = {{2020, 1, 1}, {18, 0, 0}}},

  M =
    pollution:addValue(St1#station.name, V1#measurement.datetime, V1#measurement.param, V1#measurement.value,
      pollution:addValue(St2#station.name, V1#measurement.datetime, V1#measurement.param, V1#measurement.value,
        pollution:addValue(St2#station.name, V2#measurement.datetime, V2#measurement.param, V2#measurement.value,
          pollution:addValue(St3#station.name, V1#measurement.datetime, V1#measurement.param, V1#measurement.value,
            pollution:addValue(St3#station.name, V2#measurement.datetime, V2#measurement.param, V2#measurement.value,
              pollution:addValue(St3#station.name, V3#measurement.datetime, V3#measurement.param, V3#measurement.value,
                pollution:addStation(St1#station.name, St1#station.location,
                  pollution:addStation(St2#station.name, St2#station.location,
                    pollution:addStation(St3#station.name, St3#station.location,
                      pollution:addStation(St0#station.name, St0#station.location,
                        pollution:createMonitor())))))))))),

  ?assertMatch({zero_growth, _}, pollution:getMaximumStationGrowthTime(St1#station.name, {2020, 1, 1}, pm10, M)),
  ?assertEqual({90.0, 12}, pollution:getMaximumStationGrowthTime(St2#station.name, {2020, 1, 1}, pm10, M)),
  ?assertEqual({900.0,18}, pollution:getMaximumStationGrowthTime(St3#station.name, {2020, 1, 1}, pm10, M)),
  ?assertMatch({zero_growth,_}, pollution:getMaximumStationGrowthTime(St0#station.name, {2020, 1, 1}, pm10, M)).