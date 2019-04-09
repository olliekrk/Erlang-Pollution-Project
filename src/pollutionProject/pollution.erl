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

createMonitor() -> #monitor{locations_map = maps:new(), stations_by_names = maps:new(), data = maps:new()}.

getStationByName(Name, M) ->
  try maps:find(Name, M#monitor.stations_by_names) of
    {ok, Station} -> Station
  catch
    _ -> erlang:throw("Station with such name does not exist.~n")
  end.

getStationByLocation({_, _} = Location, M) ->
  try maps:find(Location, M#monitor.locations_map) of
    {ok, Name} -> getStationByName(Name, M)
  catch
    _ -> erlang:throw("Station with such location does not exist.~n")
  end.

findMatchingMeasurements(Station, Param, M) ->
  try maps:find(Station, M#monitor.data) of
    {ok, AllMs} -> [Ms || Ms <- AllMs, Ms#measurement.param == Param]
  catch
    _ -> erlang:throw("No matching measurements.~n")
  end.

findMatchingMeasurements(Station, {{_, _, _}, {_, _, _}} = Datetime, Param, M) ->
  [Ms || Ms <- findMatchingMeasurements(Station, Param, M), Ms#measurement.datetime == Datetime];

findMatchingMeasurements(Station, {_, _, _} = Date, Param, M) ->
  GetDate = fun({DT_Date, _}) -> DT_Date end,
  [Ms || Ms <- findMatchingMeasurements(Station, Param, M), GetDate(Ms#measurement.datetime) == Date].

addStation(Name, Location, M) ->
  case (maps:is_key(Location, M#monitor.locations_map) orelse maps:is_key(Name, M#monitor.stations_by_names)) of
    true -> io:format("Station is already registered.~n"), M;
    false ->
      Station = #station{name = Name, location = Location},
      #monitor{
        locations_map = (M#monitor.locations_map)#{Location => Name},
        stations_by_names = (M#monitor.stations_by_names)#{Name => Station},
        data = (M#monitor.data)#{Station=> []}}
  end.

addValue({_, _} = Location, Datetime, Param, Value, M) ->
  try getStationByLocation(Location, M) of
    Station -> addValueUtil(Station, #measurement{param = Param, value = Value, datetime = Datetime}, M)
  catch
    _:Reason -> io:format(Reason), M
  end;

addValue(Name, Datetime, Param, Value, M) ->
  try getStationByName(Name, M) of
    Station -> addValueUtil(Station, #measurement{param = Param, value = Value, datetime = Datetime}, M)
  catch
    _:Reason -> io:format(Reason), M
  end.

addValueUtil(Station, NewMs, M) ->
  AllMs = maps:get(Station, M#monitor.data),
  case [Ms || Ms <- AllMs, Ms#measurement.datetime == NewMs#measurement.datetime, Ms#measurement.param == NewMs#measurement.param] of
    [] -> M#monitor{data = (M#monitor.data)#{Station => [NewMs | AllMs]}};
    _ -> io:format("Measurement is already in the database.~n"), M
  end.

removeValue({_, _} = Location, Datetime, Param, M) ->
  try getStationByLocation(Location, M) of
    Station -> removeValueUtil(Station, Datetime, Param, M)
  catch
    _:Reason -> io:format(Reason), M
  end;

removeValue(Name, Datetime, Param, M) ->
  try getStationByName(Name, M) of
    Station -> removeValueUtil(Station, Datetime, Param, M)
  catch
    _:Reason -> io:format(Reason), M
  end.

removeValueUtil(Station, Datetime, Param, M) ->
  Filter = fun(K, V) -> K /= Station orelse V#measurement.param /= Param orelse V#measurement.datetime /= Datetime end,
  M#monitor{data = maps:filter(Filter, M#monitor.data)}.

getOneValue({_, _} = Location, Datetime, Param, M) ->
  try getStationByLocation(Location, M) of
    Station -> getOneValueUtil(Station, Datetime, Param, M)
  catch
    _:Reason -> {unknown_value, Reason}
  end;

getOneValue(Name, Datetime, Param, M) ->
  try getStationByName(Name, M) of
    Station -> getOneValueUtil(Station, Datetime, Param, M)
  catch
    _:Reason -> {unknown_value, Reason}
  end.

getOneValueUtil(Station, Datetime, Param, M) ->
  try findMatchingMeasurements(Station, Datetime, Param, M) of
    [SingleMs] -> SingleMs;
    _ -> {unknown_value, "More than one measurement was found."}
  catch
    _:Reason -> {unknown_value, Reason}
  end.

getStationMean({_, _} = Location, Param, M) ->
  try getStationByLocation(Location, M) of
    Station -> getStationMeanUtil(Station, Param, M)
  catch
    _:Reason -> {unknown_value, Reason}
  end;

getStationMean(Name, Param, M) ->
  try getStationByName(Name, M) of
    Station -> getStationMeanUtil(Station, Param, M)
  catch
    _:Reason -> {unknown_value, Reason}
  end.

getStationMeanUtil(Station, Param, M) ->
  try findMatchingMeasurements(Station, Param, M) of
    [] -> 0;
    MsList ->
      lists:foldl(fun(Ms, Acc) -> Ms#measurement.value + Acc end, 0, MsList) / length(MsList)
  catch
    _:Reason -> {unknown_value, Reason}
  end.

getStationDailyMean(Station, M, DateParamFilter) ->
  try maps:find(Station, M#monitor.data) of
    AllMs ->
      MsList = lists:filter(DateParamFilter, AllMs),
      lists:foldl(fun(Ms, Acc) -> Ms#measurement.value + Acc end, 0, MsList) / lists:flatlength(MsList)
  catch
    _ -> erlang:throw("Station does not exist")
  end.

getDailyMean(Date, Param, M) ->
  FilterFun = fun(Ms) ->
    {MsDate, _} = Ms#measurement.datetime,
    MsDate == Date andalso Ms#measurement.param == Param end,

  MeanFun = fun(Station) -> getStationDailyMean(Station, M, FilterFun) end,

  Stations = maps:keys(M#monitor.data),
  try
    lists:sum(lists:map(MeanFun, Stations)) / lists:flatlength(Stations)
  catch
    _:Reason -> {unknown_value, Reason}
  end.

getGrowth(Ms, NextMs) ->
  Growth = NextMs#measurement.value - Ms#measurement.value,
  {_, {GrowthHour, _, _}} = NextMs#measurement.datetime,
  {Growth, GrowthHour}.

zipToGrowths([], _) -> [];
zipToGrowths(_, []) -> [];
zipToGrowths([Ms | MsTail], [NextMs | NextMsTail]) ->
  [getGrowth(Ms, NextMs) | zipToGrowths(MsTail, NextMsTail)].

getMaximumStationGrowthTime({_, _} = Location, Date, Param, M) ->
  try getStationByLocation(Location, M) of
    Station -> getMaximumStationGrowthTimeUtil(Station, Date, Param, M)
  catch
    _:Reason -> {unknown_value, Reason}
  end;

getMaximumStationGrowthTime(Name, Date, Param, M) ->
  try getStationByName(Name, M) of
    Station -> getMaximumStationGrowthTimeUtil(Station, Date, Param, M)
  catch
    _:Reason -> {unknown_value, Reason}
  end.

getMaximumStationGrowthTimeUtil(Station, Date, Param, M) ->
  ConvertDateTime = fun calendar:datetime_to_gregorian_seconds/1,
  SortFun = fun(Ms1, Ms2) ->
    ConvertDateTime(Ms1#measurement.datetime) =< ConvertDateTime(Ms2#measurement.datetime)
            end,
  MaxFun = fun
             ({G1, H1}, {G2, _}) when G1 > G2 -> {G1, H1};
             ({_, _}, {G2, H2}) -> {G2, H2}
           end,

  try findMatchingMeasurements(Station, Date, Param, M) of
    [] -> {zero_growth, no_measurement};
    [_ | []] -> {zero_growth, one_measurement};
    MsList ->
      SortedMs = lists:sort(SortFun, MsList),
      [_ | SortedMsTail] = SortedMs,
      Growths = zipToGrowths(SortedMs, SortedMsTail),
      lists:foldl(MaxFun, {0, 0}, Growths)
  catch
    _:Reason -> {unknown_value, Reason}
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

  pollution:getMaximumStationGrowthTime({18, 18}, {2019, 4, 2}, "PM10", P7),
  pollution:getMaximumStationGrowthTime("Nineteen", {2019, 4, 2}, "PM10", P7),
  pollution:getMaximumStationGrowthTime("Twenty", {2019, 4, 2}, "PM10", P7),

  pollution:getStationMean({18, 18}, "PM10", P7),
  pollution:getStationMean({19, 19}, "PM10", P7),
  pollution:getStationMean({20, 20}, "PM10", P7),

  P7.
