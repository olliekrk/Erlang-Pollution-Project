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
  getStationByName/2,
  getStationByLocation/2,
  test/0,
  getMaximumStationGrowthTime/4]).

-record(station, {name, location}).
-record(measurement, {param, value, datetime}).
-record(monitor, {locations_map, stations_by_names, data}).

createMonitor() -> #monitor{locations_map = maps:new(), stations_by_names = maps:new(), data = dict:new()}.

getStationByName(Name, M) ->
  try maps:find(Name, M#monitor.stations_by_names) of
    {ok, Station} -> Station
  catch
    error:Reason -> {error, caught, Reason}
  end.

getStationByLocation(Location, M) ->
  try maps:find(Location, M#monitor.locations_map) of
    {ok, Name} -> getStationByName(Name, M)
  catch
    error:Reason -> {error, caught, Reason}
  end.

findMatchingMeasurements(Station, Param, M) ->
  [Ms || Ms <- dict:fetch(Station, M#monitor.data), Ms#measurement.param == Param].

findMatchingMeasurements(Station, {{_, _, _}, {_, _, _}} = Datetime, Param, M) ->
  [Ms || Ms <- findMatchingMeasurements(Station, Param, M), Ms#measurement.datetime == Datetime];

findMatchingMeasurements(Station, {_, _, _} = Date, Param, M) ->
  GetDate = fun({DT_Date, _}) -> DT_Date end,
  [Ms || Ms <- findMatchingMeasurements(Station, Param, M), GetDate(Ms#measurement.datetime) == Date].

addStation(Name, Location, M) ->
  case maps:is_key(Name, M#monitor.locations_map) or maps:is_key(Location, M#monitor.stations_by_names) of
    true -> erlang:throw("Station like that is already registered!");
    _ -> #monitor{
      locations_map = (M#monitor.locations_map)#{Location => Name},
      stations_by_names = (M#monitor.stations_by_names)#{Name => #station{name = Name, location = Location}},
      data = dict:store(#station{name = Name, location = Location}, [], M#monitor.data)}
  end.

addValue({X, Y}, Datetime, Param, Value, M) ->
  addValueUtil(getStationByLocation({X, Y}, M), #measurement{param = Param, value = Value, datetime = Datetime}, M);

addValue(Name, Datetime, Param, Value, M) ->
  addValueUtil(getStationByName(Name, M), #measurement{param = Param, value = Value, datetime = Datetime}, M).

addValueUtil(Station, NewMs, M) ->
  case [Ms || Ms <- dict:fetch(Station, M#monitor.data), Ms#measurement.datetime == NewMs#measurement.datetime, Ms#measurement.param == NewMs#measurement.param] of
    [] -> M#monitor{data = dict:append(Station, NewMs, M#monitor.data)};
    _ -> erlang:throw("Measurement like that already exists")
  end.

removeValue(Location, Datetime, Param, M) when is_tuple(Location) ->
  removeValueUtil(getStationByLocation(Location, M), Datetime, Param, M);

removeValue(Name, Datetime, Param, M) ->
  removeValueUtil(getStationByName(Name, M), Datetime, Param, M).

removeValueUtil(Station, Datetime, Param, M) ->
  M#monitor{data = dict:filter(fun(K, V) ->
    (K /= Station orelse V#measurement.param /= Param orelse V#measurement.datetime /= Datetime) end, M#monitor.data)}.

getOneValue(Location, Datetime, Param, M) when is_tuple(Location) ->
  getOneValueUtil(getStationByLocation(Location, M), Datetime, Param, M);

getOneValue(Name, Datetime, Param, M) ->
  getOneValueUtil(getStationByName(Name, M), Datetime, Param, M).

getOneValueUtil(Station, Datetime, Param, M) ->
  case findMatchingMeasurements(Station, Datetime, Param, M) of
    [] -> erlang:throw("No matching measurements");
    [M] -> M
  end.

getStationMean(Location, Param, M) when is_tuple(Location) ->
  getStationMeanUtil(getStationByLocation(Location, M), Param, M);

getStationMean(Name, Param, M) ->
  getStationMeanUtil(getStationByName(Name, M), Param, M).

getStationMeanUtil(Station, Param, M) ->
  MsList = findMatchingMeasurements(Station, Param, M),
  lists:foldl(fun(Ms, Acc) -> Ms#measurement.value + Acc end, 0, MsList) / lists:flatlength(MsList).

filterByParamAndDate(Date, Param) ->
  fun(Ms) ->
    {MsDate, _} = Ms#measurement.datetime,
    Ms#measurement.param == Param andalso MsDate == Date
  end.

getStationDailyMean(Station, M, DateParamFilter) ->
  MsList = lists:filter(DateParamFilter, dict:fetch(Station, M#monitor.data)),
  lists:foldl(fun(Ms, Acc) -> Ms#measurement.value + Acc end, 0, MsList) / lists:flatlength(MsList).

getDailyMean(Param, Date, M) ->
  Stations = dict:fetch_keys(M#monitor.data),
  DateFilter = filterByParamAndDate(Param, Date),
  StationDailyMean = fun(Station) -> getStationDailyMean(Station, M, DateFilter) end,
  lists:sum(lists:map(StationDailyMean, Stations)) / lists:flatlength(Stations).

getMaximumStationGrowthTime(Station, Param, Date, M) ->
  SortFun =
    fun(Ms1, Ms2) ->
      calendar:datetime_to_gregorian_seconds(Ms1#measurement.datetime) =< calendar:datetime_to_gregorian_seconds(Ms2#measurement.datetime)
    end,

  GrowthFun =
    fun(Ms, NextMs) ->
      GrowthVal = NextMs#measurement.value - Ms#measurement.value,
      {_, {GrowthHour, _, _}} = NextMs#measurement.datetime,
      {GrowthVal, GrowthHour}
    end,

  MaxFun =
    fun({G1, H1}, {G2, H2}) ->
      case G1 > G2 of
        true -> {G1, H1};
        false -> {G2, H2}
      end
    end,

  ParamMs = findMatchingMeasurements(Station, Date, Param, M),
  SortedMs = lists:sort(SortFun, ParamMs),
  [_ | SortedMsTail] = SortedMs,
  Growths = lists:zipwith(GrowthFun, SortedMs, SortedMsTail),
  lists:foldl(MaxFun, {0, no_growth}, Growths).

test() ->
  P = pollution:createMonitor(),
  P1 = pollution:addStation("Aleja Slowackiego", {50.2345, 18.3445}, P),
  St = pollution:getStationByName("Aleja Slowackiego", P1),
  io:write(St),
  P2 = pollution:addValue({50.2345, 18.3445}, calendar:local_time(), "PM10", 59, P1),
  P3 = pollution:addValue("Aleja Slowackiego", calendar:local_time(), "PM2,5", 113, P2),
  io:write(P3),
  P3.
