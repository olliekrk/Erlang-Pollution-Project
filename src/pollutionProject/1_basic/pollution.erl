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
  findStationByName/2, findStationByLocation/2, findMatchingMeasurements/3]).

-record(station, {name, location}).
-record(measurement, {param, value, datetime}).
-record(monitor, {locations_map, stations_by_names, data}).

createMonitor() -> #monitor{locations_map = maps:new(), stations_by_names = maps:new(), data = maps:new()}.

findStationByName(Name, M) ->
  try maps:find(Name, M#monitor.stations_by_names) of
    {ok, Station} -> Station
  catch
    _ -> {error, "Station with such name does not exist."}
  end.

findStationByLocation({_, _} = Location, M) ->
  try maps:find(Location, M#monitor.locations_map) of
    {ok, Name} -> findStationByName(Name, M)
  catch
    _ -> {error, "Station with such location does not exist."}
  end.

findMatchingMeasurements(Station, Param, M) ->
  try maps:find(Station, M#monitor.data) of
    {ok, AllMs} -> [Ms || Ms <- AllMs, Ms#measurement.param == Param]
  catch
    _ -> {error, "No matching measurements."}
  end.

findMatchingMeasurements(Station, {{_, _, _}, {_, _, _}} = Datetime, Param, M) ->
  case findMatchingMeasurements(Station, Param, M) of
    {error, Reason} -> {error, Reason};
    List -> [Ms || Ms <- List, Ms#measurement.datetime == Datetime]
  end;

findMatchingMeasurements(Station, {_, _, _} = Date, Param, M) ->
  case findMatchingMeasurements(Station, Param, M) of
    {error, Reason} -> {error, Reason};
    List ->
      GetDate = fun({DT_Date, _}) -> DT_Date end,
      [Ms || Ms <- List, GetDate(Ms#measurement.datetime) == Date]
  end.

addStation(Name, Location, M) ->
  case (maps:is_key(Location, M#monitor.locations_map) orelse maps:is_key(Name, M#monitor.stations_by_names)) of
    true -> {error, "Station is already registered."};
    false ->
      Station = #station{name = Name, location = Location},
      #monitor{
        locations_map = (M#monitor.locations_map)#{Location => Name},
        stations_by_names = (M#monitor.stations_by_names)#{Name => Station},
        data = (M#monitor.data)#{Station=> []}}
  end.

addValue({_, _} = Location, Datetime, Param, Value, M) ->
  case findStationByLocation(Location, M) of
    {error, R} -> {error, R};
    Station -> addValueUtil(Station, #measurement{param = Param, value = Value, datetime = Datetime}, M)
  end;

addValue(Name, Datetime, Param, Value, M) ->
  case findStationByName(Name, M) of
    {error, R} -> {error, R};
    Station -> addValueUtil(Station, #measurement{param = Param, value = Value, datetime = Datetime}, M)
  end.

addValueUtil(Station, NewMs, M) ->
  AllMs = maps:get(Station, M#monitor.data),
  case [Ms || Ms <- AllMs, Ms#measurement.datetime == NewMs#measurement.datetime, Ms#measurement.param == NewMs#measurement.param] of
    [] -> M#monitor{data = (M#monitor.data)#{Station => [NewMs | AllMs]}};
    _ -> {error, "Measurement is already in the database."}
  end.

removeValue({_, _} = Location, Datetime, Param, M) ->
  case findStationByLocation(Location, M) of
    {error, R} -> {error, R};
    Station -> removeValueUtil(Station, Datetime, Param, M)
  end;

removeValue(Name, Datetime, Param, M) ->
  case findStationByName(Name, M) of
    {error, R} -> {error, R};
    Station -> removeValueUtil(Station, Datetime, Param, M)
  end.

removeValueUtil(Station, Datetime, Param, M) ->
  FilterFun = fun(Ms) -> Ms#measurement.param /= Param orelse Ms#measurement.datetime /= Datetime end,
  FilteredList = lists:filter(FilterFun, maps:get(Station, M#monitor.data)),
  M#monitor{data = (M#monitor.data)#{Station => FilteredList}}.

getOneValue({_, _} = Location, Datetime, Param, M) ->
  case findStationByLocation(Location, M) of
    {error, R} -> {error, R};
    Station -> getOneValueUtil(Station, Datetime, Param, M)
  end;

getOneValue(Name, Datetime, Param, M) ->
  case findStationByName(Name, M) of
    {error, R} -> {error, R};
    Station -> getOneValueUtil(Station, Datetime, Param, M)
  end.

getOneValueUtil(Station, Datetime, Param, M) ->
  case findMatchingMeasurements(Station, Datetime, Param, M) of
    {error, R} -> {error, R};
    [SingleMs] -> SingleMs;
    _ -> {error, "More than one measurement was found."}
  end.

getStationMean({_, _} = Location, Param, M) ->
  case findStationByLocation(Location, M) of
    {error, R} -> {error, R};
    Station -> getStationMeanUtil(Station, Param, M)
  end;

getStationMean(Name, Param, M) ->
  case findStationByName(Name, M) of
    {error, R} -> {error, R};
    Station -> getStationMeanUtil(Station, Param, M) end.

getStationMeanUtil(Station, Param, M) ->
  case findMatchingMeasurements(Station, Param, M) of
    {error, R} -> {error, R};
    [] -> 0;
    MsList ->
      lists:foldl(fun(Ms, Acc) -> Ms#measurement.value + Acc end, 0, MsList) / length(MsList)
  end.

getStationDailyMean(Station, M, DateParamFilter) ->
  case maps:find(Station, M#monitor.data) of
    {ok, AllMs} ->
      MsList = lists:filter(DateParamFilter, AllMs),
      lists:foldl(fun(Ms, Acc) -> Ms#measurement.value + Acc end, 0, MsList) / lists:flatlength(MsList);
    _ -> {error, "Station data does not exist"}
  end.

getDailyMean(Date, Param, M) ->
  FilterFun = fun(Ms) ->
    {MsDate, _} = Ms#measurement.datetime,
    MsDate == Date andalso Ms#measurement.param == Param end,

  MeanFun = fun(Station) -> getStationDailyMean(Station, M, FilterFun) end,

  Stations = maps:keys(M#monitor.data),
  try
    lists:sum(lists:map(MeanFun, Stations)) / length((Stations))
  catch
    _ -> no_data
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
  case findStationByLocation(Location, M) of
    {error, R} -> {error, R};
    Station -> getMaximumStationGrowthTimeUtil(Station, Date, Param, M)
  end;

getMaximumStationGrowthTime(Name, Date, Param, M) ->
  case findStationByName(Name, M) of
    {error, R} -> {error, R};
    Station -> getMaximumStationGrowthTimeUtil(Station, Date, Param, M)
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

  case findMatchingMeasurements(Station, Date, Param, M) of
    {error, R} -> {error, R};
    [] -> {zero_growth, "Zero measurements available"};
    [_ | []] -> {zero_growth, "Only one measurement available"};
    MsList when is_list(MsList) ->
      SortedMs = lists:sort(SortFun, MsList),
      [_ | SortedMsTail] = SortedMs,
      Growths = zipToGrowths(SortedMs, SortedMsTail),
      lists:foldl(MaxFun, {0, 0}, Growths)
  end.