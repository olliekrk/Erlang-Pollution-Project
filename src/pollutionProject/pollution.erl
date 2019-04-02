-module(pollution).
-author("olliekrk").

-export([
  createMonitor/0,
  addStation/3,
  addValue/5,
  removeValue/4,
  getOneValue/4,
  getStationMean/3,
  getDailyMean/3,
  getMaximumStationGrowthTime/4,
  test/0
]).

-record(station, {name, location}).
-record(measurement, {param, value, datetime}).
-record(monitor, {locations_map, stations_by_names, data}).

createMonitor() -> #monitor{locations_map = maps:new(), stations_by_names = maps:new(), data = dict:new()}.

getStationByName(Name, M) ->
  try maps:find(Name, M#monitor.stations_by_names) of
    {ok, Station} -> Station
  catch
    error:_ -> {error, Name, "Station with such name does not exist"}
  end.

getStationByLocation({_, _} = Location, M) ->
  try maps:find(Location, M#monitor.locations_map) of
    {ok, Name} -> getStationByName(Name, M)
  catch
    error:_ -> {error, Location, "Station with such location does not exist"}
  end.

findMatchingMeasurements(Station, Param, M) ->
  [Ms || Ms <- dict:fetch(Station, M#monitor.data), Ms#measurement.param == Param].

findMatchingMeasurements(Station, {{_, _, _}, {_, _, _}} = Datetime, Param, M) ->
  [Ms || Ms <- findMatchingMeasurements(Station, Param, M), Ms#measurement.datetime == Datetime];

findMatchingMeasurements(Station, {_, _, _} = Date, Param, M) ->
  GetDate = fun({DT_Date, _}) -> DT_Date end,
  [Ms || Ms <- findMatchingMeasurements(Station, Param, M), GetDate(Ms#measurement.datetime) == Date].

addStation(Name, Location, M) ->
  case (maps:is_key(Location, M#monitor.locations_map) orelse maps:is_key(Name, M#monitor.stations_by_names)) of
    true -> erlang:throw("Station like that is already registered!");
    false -> #monitor{
      locations_map = (M#monitor.locations_map)#{Location => Name},
      stations_by_names = (M#monitor.stations_by_names)#{Name => #station{name = Name, location = Location}},
      data = dict:store(#station{name = Name, location = Location}, [], M#monitor.data)}
  end.

addValue({_, _} = Location, Datetime, Param, Value, M) ->
  addValueUtil(getStationByLocation(Location, M), #measurement{param = Param, value = Value, datetime = Datetime}, M);

addValue(Name, Datetime, Param, Value, M) ->
  addValueUtil(getStationByName(Name, M), #measurement{param = Param, value = Value, datetime = Datetime}, M).

addValueUtil(Station, NewMs, M) ->
  case [Ms || Ms <- dict:fetch(Station, M#monitor.data), Ms#measurement.datetime == NewMs#measurement.datetime, Ms#measurement.param == NewMs#measurement.param] of
    [] -> M#monitor{data = dict:append(Station, NewMs, M#monitor.data)};
    _ -> {throw, "Measurement like that already exists", NewMs}
  end.

removeValue({_, _} = Location, Datetime, Param, M) ->
  removeValueUtil(getStationByLocation(Location, M), Datetime, Param, M);

removeValue(Name, Datetime, Param, M) ->
  removeValueUtil(getStationByName(Name, M), Datetime, Param, M).

removeValueUtil(Station, Datetime, Param, M) ->
  M#monitor{data = dict:filter(fun(K, V) ->
    (K /= Station orelse V#measurement.param /= Param orelse V#measurement.datetime /= Datetime) end, M#monitor.data)}.

getOneValue({_, _} = Location, Datetime, Param, M) ->
  getOneValueUtil(getStationByLocation(Location, M), Datetime, Param, M);

getOneValue(Name, Datetime, Param, M) ->
  getOneValueUtil(getStationByName(Name, M), Datetime, Param, M).

getOneValueUtil(Station, Datetime, Param, M) ->
  case findMatchingMeasurements(Station, Datetime, Param, M) of
    [] -> erlang:throw("No matching measurements");
    [M] -> M
  end.

getStationMean({_, _} = Location, Param, M) ->
  getStationMeanUtil(getStationByLocation(Location, M), Param, M);

getStationMean(Name, Param, M) ->
  getStationMeanUtil(getStationByName(Name, M), Param, M).

getStationMeanUtil(Station, Param, M) ->
  case findMatchingMeasurements(Station, Param, M) of
    [] -> erlang:throw("Unable to calculate stations's mean: No measurements");
    MsList -> lists:foldl(fun(Ms, Acc) -> Ms#measurement.value + Acc end, 0, MsList) / lists:flatlength(MsList)
  end.

getStationDailyMean(Station, M, DateParamFilter) ->
  MsList = lists:filter(DateParamFilter, dict:fetch(Station, M#monitor.data)),
  lists:foldl(fun(Ms, Acc) -> Ms#measurement.value + Acc end, 0, MsList) / lists:flatlength(MsList).

getDailyMean(Param, Date, M) ->
  DateParamFilter =
    fun
      (Ms) ->
        {MsDate, _} = Ms#measurement.datetime,
        Ms#measurement.param == Param andalso MsDate == Date
    end,

  StationDailyMean =
    fun
      (Station) -> getStationDailyMean(Station, M, DateParamFilter)
    end,

  Stations = dict:fetch_keys(M#monitor.data),
  lists:sum(lists:map(StationDailyMean, Stations)) / lists:flatlength(Stations).

calculateGrowth(Ms, NextMs) ->
  Growth = NextMs#measurement.value - Ms#measurement.value,
  {_, {GrowthHour, _, _}} = NextMs#measurement.datetime,
  {Growth, GrowthHour}.

growthsZipFun([], _) -> [];
growthsZipFun(_, []) -> [];
growthsZipFun([Ms | MsTail], [NextMs | NextMsTail]) ->
  [calculateGrowth(Ms, NextMs) | growthsZipFun(MsTail, NextMsTail)].

getMaximumStationGrowthTime({_, _} = Location, Param, Date, M) ->
  getMaximumStationGrowthTimeUtil(getStationByLocation(Location, M), Param, Date, M);

getMaximumStationGrowthTime(Name, Param, Date, M) ->
  getMaximumStationGrowthTimeUtil(getStationByName(Name, M), Param, Date, M).

getMaximumStationGrowthTimeUtil(Station, Param, Date, M) ->
  SortFun =
    fun(Ms1, Ms2) ->
      calendar:datetime_to_gregorian_seconds(Ms1#measurement.datetime) =< calendar:datetime_to_gregorian_seconds(Ms2#measurement.datetime)
    end,

  MaxFun =
    fun({G1, H1}, {G2, H2}) ->
      case G1 > G2 of
        true -> {G1, H1};
        false -> {G2, H2}
      end
    end,

  ParamMs = findMatchingMeasurements(Station, Date, Param, M),
  case ParamMs of
    [] -> {0, no_growth};
    [_ | []] -> {0, no_growth};
    _ ->
      SortedMs = lists:sort(SortFun, ParamMs),
      [_ | SortedMsTail] = SortedMs,
      Growths = growthsZipFun(SortedMs, SortedMsTail),
      lists:foldl(MaxFun, {0, no_growth}, Growths)
  end.

test() ->
  P = pollution:createMonitor(),
  P1 = pollution:addStation("Eighteen", {18, 18}, P),
  P2 = pollution:addStation("Nineteen", {19, 19}, P1),
  P3 = pollution:addStation("Twenty", {20, 20}, P2),
  P4 = pollution:addValue({18, 18}, {{2019, 4, 2}, {10, 0, 0}}, "PM10", 1, P3),
  P5 = pollution:addValue({18, 18}, {{2019, 4, 2}, {18, 0, 0}}, "PM10", 100, P4),
  P6 = pollution:addValue({19, 19}, {{2019, 4, 2}, {20, 0, 0}}, "PM10", 59, P5),
  P7 = pollution:addValue("Eighteen", {{2019, 4, 2}, {11, 0, 0}}, "PM10", 1099, P6),

  pollution:getMaximumStationGrowthTime({18, 18}, "PM10", {2019, 4, 2}, P7),
  pollution:getMaximumStationGrowthTime("Nineteen", "PM10", {2019, 4, 2}, P7),
  pollution:getMaximumStationGrowthTime("Twenty", "PM10", {2019, 4, 2}, P7),

  pollution:getStationMean({18, 18}, "PM10", P7),
  pollution:getStationMean({19, 19}, "PM10", P7),
  pollution:getStationMean({20, 20}, "PM10", P7),

  P7.
