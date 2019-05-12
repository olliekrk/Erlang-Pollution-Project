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
  findStationByName/2, findStationByLocation/2]).

-record(station, {name, location}).
-record(measurement, {param, value, datetime}).
-record(monitor, {locations_map, stations_by_names, data}).

createMonitor() -> #monitor{locations_map = maps:new(), stations_by_names = maps:new(), data = maps:new()}.

findStationByName(Name, M) ->
  try maps:find(Name, M#monitor.stations_by_names) of
    {ok, Station} -> Station
  catch
    _:_ -> erlang:throw("Station with such name does not exist.")
  end.

findStationByLocation({_, _} = Location, M) ->
  try maps:find(Location, M#monitor.locations_map) of
    {ok, Name} -> findStationByName(Name, M)
  catch
    _:_ -> erlang:throw("Station with such location does not exist.")
  end.

findMatchingMeasurements(Station, Param, M) ->
  try maps:find(Station, M#monitor.data) of
    {ok, AllMs} -> [Ms || Ms <- AllMs, Ms#measurement.param == Param]
  catch
    _:_ -> erlang:throw("No matching measurements.")
  end.

findMatchingMeasurements(Station, {{_, _, _}, {_, _, _}} = Datetime, Param, M) ->
  List = findMatchingMeasurements(Station, Param, M),
  [Ms || Ms <- List, Ms#measurement.datetime == Datetime];

findMatchingMeasurements(Station, {_, _, _} = Date, Param, M) ->
  List = findMatchingMeasurements(Station, Param, M),
  GetDate = fun({DT_Date, _}) -> DT_Date end,
  [Ms || Ms <- List, GetDate(Ms#measurement.datetime) == Date].

addStation(Name, Location, M) ->
  case (maps:is_key(Location, M#monitor.locations_map) orelse maps:is_key(Name, M#monitor.stations_by_names)) of
    true -> erlang:throw("Station is already registered.");
    false ->
      Station = #station{name = Name, location = Location},
      #monitor{
        locations_map = (M#monitor.locations_map)#{Location => Name},
        stations_by_names = (M#monitor.stations_by_names)#{Name => Station},
        data = (M#monitor.data)#{Station=> []}}
  end.

addValue({_, _} = Location, Datetime, Param, Value, M) ->
  Station = findStationByLocation(Location, M),
  addValueUtil(Station, #measurement{param = Param, value = Value, datetime = Datetime}, M);

addValue(Name, Datetime, Param, Value, M) ->
  Station = findStationByName(Name, M),
  addValueUtil(Station, #measurement{param = Param, value = Value, datetime = Datetime}, M).

addValueUtil(Station, NewMs, M) ->
  AllMs = maps:get(Station, M#monitor.data),
  case [Ms || Ms <- AllMs, Ms#measurement.datetime == NewMs#measurement.datetime, Ms#measurement.param == NewMs#measurement.param] of
    [] -> M#monitor{data = (M#monitor.data)#{Station => [NewMs | AllMs]}};
    _ -> erlang:throw("Measurement is already in the database.")
  end.

removeValue({_, _} = Location, Datetime, Param, M) ->
  Station = findStationByLocation(Location, M),
  removeValueUtil(Station, Datetime, Param, M);

removeValue(Name, Datetime, Param, M) ->
  Station = findStationByName(Name, M),
  removeValueUtil(Station, Datetime, Param, M).

removeValueUtil(Station, Datetime, Param, M) ->
  Filter = fun(K, V) -> K /= Station orelse V#measurement.param /= Param orelse V#measurement.datetime /= Datetime end,
  M#monitor{data = maps:filter(Filter, M#monitor.data)}.

getOneValue({_, _} = Location, Datetime, Param, M) ->
  Station = findStationByLocation(Location, M),
  getOneValueUtil(Station, Datetime, Param, M);

getOneValue(Name, Datetime, Param, M) ->
  Station = findStationByName(Name, M),
  getOneValueUtil(Station, Datetime, Param, M).

getOneValueUtil(Station, Datetime, Param, M) ->
  case findMatchingMeasurements(Station, Datetime, Param, M) of
    [SingleMs] -> SingleMs;
    _ -> erlang:throw("More than one measurement was found.")
  end.

getStationMean({_, _} = Location, Param, M) ->
  Station = findStationByLocation(Location, M),
  getStationMeanUtil(Station, Param, M);

getStationMean(Name, Param, M) ->
  Station = findStationByName(Name, M),
  getStationMeanUtil(Station, Param, M).

getStationMeanUtil(Station, Param, M) ->
  case findMatchingMeasurements(Station, Param, M) of
    [] -> 0;
    MsList ->
      lists:foldl(fun(Ms, Acc) -> Ms#measurement.value + Acc end, 0, MsList) / length(MsList)
  end.

getStationDailyMean(Station, M, DateParamFilter) ->
  case maps:find(Station, M#monitor.data) of
    {ok, AllMs} ->
      MsList = lists:filter(DateParamFilter, AllMs),
      lists:foldl(fun(Ms, Acc) -> Ms#measurement.value + Acc end, 0, MsList) / lists:flatlength(MsList);
    _ -> erlang:throw("Station data does not exist")
  end.

getDailyMean(Date, Param, M) ->
  FilterFun = fun(Ms) ->
    {MsDate, _} = Ms#measurement.datetime,
    MsDate == Date andalso Ms#measurement.param == Param end,

  MeanFun = fun(Station) ->
    getStationDailyMean(Station, M, FilterFun) end,

  Stations = maps:keys(M#monitor.data),
  try
    lists:sum(lists:map(MeanFun, Stations)) / lists:flatlength(Stations)
  catch
    _:Reason -> erlang:throw(Reason)
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
  Station = findStationByLocation(Location, M),
  getMaximumStationGrowthTimeUtil(Station, Date, Param, M);

getMaximumStationGrowthTime(Name, Date, Param, M) ->
  Station = findStationByName(Name, M),
  getMaximumStationGrowthTimeUtil(Station, Date, Param, M).

getMaximumStationGrowthTimeUtil(Station, Date, Param, M) ->
  ConvertDateTime = fun calendar:datetime_to_gregorian_seconds/1,
  SortFun = fun(Ms1, Ms2) ->
    ConvertDateTime(Ms1#measurement.datetime) =< ConvertDateTime(Ms2#measurement.datetime)
            end,
  MaxFun = fun
             ({G1, H1}, {G2, _}) when G1 > G2 -> {G1, H1};
             ({_, _}, {G2, H2}) -> {G2, H2}
           end,

  case findMatchingMeasurements(Station, Date, Param, M) of
    [] -> {zero_growth, "Zero measurements available"};
    [_ | []] -> {zero_growth, "Only one measurement available"};
    MsList when is_list(MsList) ->
      SortedMs = lists:sort(SortFun, MsList),
      [_ | SortedMsTail] = SortedMs,
      Growths = zipToGrowths(SortedMs, SortedMsTail),
      lists:foldl(MaxFun, {0, 0}, Growths)
  end.