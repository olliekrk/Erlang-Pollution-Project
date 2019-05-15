defmodule PollutionData do
  @moduledoc false

  def import_lines_from_CSV(filename \\ "pollution.csv") do
    File.read!(filename)
    |> String.split("\r\n")
  end

  def parse_line(line) do
    [date, time, x_location, y_location, value] = String.split(line, ",")

    map_to_number = fn t -> elem(Float.parse(t), 0)end

    date = String.split(date, "-")
           |> Enum.reverse
           |> Enum.map(map_to_number)
           |> :erlang.list_to_tuple

    time = String.split(time, ":")
           |> Enum.map(map_to_number)

    time = Enum.concat(time, [0])
           |> :erlang.list_to_tuple

    datetime = :erlang.list_to_tuple([date, time])

    location = [x_location, y_location]
               |> Enum.map(map_to_number)
               |> :erlang.list_to_tuple

    value = map_to_number.(value)

    %{:datetime => datetime, :location => location, :pollutionLevel => value}
  end

  def identify_stations(structure_list) do

    extract_location = fn
      structure -> structure.location
    end

    reduce_fun = fn
      location, map -> Map.put(map, extract_location.(location), "unnamed")
    end

    structure_list
    |> Enum.reduce(%{}, reduce_fun)
  end

  def generate_station(locations_map) do
    generate_entry = fn location ->
      x = elem(location, 0)
      y = elem(location, 1)
      name = "station_#{x}_#{y}"
      {location, name}
    end

    for {k, _} <- locations_map, into: %{}, do: generate_entry.(k)

  end

  # combines all of the above functions
  def prepare_data(filename \\ "pollution.csv") do
    list = for line <- import_lines_from_CSV(filename) do
      parse_line(line) end
    list
    |> identify_stations
    |> generate_station
  end

  def start_erlang_server do
    :pollution_server_sup.start_supervisor()
    # todo finish this
  end

end
