defmodule PollutionData do
  @moduledoc false

  def import_lines_from_CSV(filename \\ "pollution.csv") do
    File.read!(filename)
    |> String.split("\r\n")
  end

  def parse_line(line) do
    [date, time, x_location, y_location, value] = String.split(line, ",")

    map_to_float = fn t -> elem(Float.parse(t), 0)end
    map_to_integer = fn t -> elem(Integer.parse(t), 0)end

    date = String.split(date, "-")
           |> Enum.reverse
           |> Enum.map(map_to_integer)
           |> :erlang.list_to_tuple

    time = String.split(time, ":")
           |> Enum.map(map_to_integer)

    time = Enum.concat(time, [0])
           |> :erlang.list_to_tuple

    datetime = :erlang.list_to_tuple([date, time])

    location = [x_location, y_location]
               |> Enum.map(map_to_float)
               |> :erlang.list_to_tuple

    value = map_to_float.(value)

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
      name = 'station_#{x}_#{y}'
      {location, name}
    end

    for {k, _} <- locations_map, into: %{}, do: generate_entry.(k)

  end

  def prepare_data(filename \\ "pollution.csv") do
    list = for line <- import_lines_from_CSV(filename) do
      parse_line(line) end
    list
    |> Enum.each(
         fn %{:datetime => datetime, :location => location, :pollutionLevel => value} ->
           :pollution_gen_server.addValue(location, datetime, 'PM10', value)
         end
       )
    #    IO.puts "Measurements data loaded"
  end

  # combines all of the above functions
  def prepare_stations(filename \\ "pollution.csv") do
    list = for line <- import_lines_from_CSV(filename) do
      parse_line(line) end
    list
    |> identify_stations
    |> generate_station
    |> Enum.each(fn {location, name} -> :pollution_gen_server.addStation(name, location) end)
    #    IO.puts "Stations data loaded"
  end

  def prepare_monitor(filename \\ "pollution.csv") do
    prepare_stations(filename)
    prepare_data(filename)
  end

  def start_erlang_server do
    :pollution_supervisor.start_supervisor()
    IO.puts "Supervisor has started"

    stationsLoadingTime =
      fn -> prepare_stations end
      |> :timer.tc
      |> elem(0)
      |> fn t -> t / 1_000_000.0 end.()

    dataLoadingTime =
      fn -> prepare_data end
      |> :timer.tc
      |> elem(0)
      |> fn t -> t / 1_000_000.0 end.()

    IO.puts "Measurements prepared in #{dataLoadingTime} s"
    IO.puts "Stations prepared in #{stationsLoadingTime} s"

  end

  def measure(function) do
    {time, value} =
      function
      |> :timer.tc

    time = time / 1_000_000.0

    IO.puts "Time: #{time} s"
    value
  end

  def run_measurements() do
    r = measure(fn -> :pollution_gen_server.getStationMean({20.060, 49.986}, 'PM10') end)
    IO.puts "Result: #{r}"

    r = measure(fn -> :pollution_gen_server.getDailyMean({2017, 5, 3}, 'PM10') end)
    IO.puts "Result: #{r}"

    r = measure(
      fn -> :pollution_gen_server.getMaximumStationGrowthTime({19.827, 50.042}, {2017, 5, 3}, 'PM10')
      end
    )
    {growth, hour} = r
    IO.puts "Growth: #{growth}, hour: #{hour}"

    r = measure(
      fn -> :pollution_gen_server.getMaximumStationGrowthTime({19.895, 50.055}, {2017, 5, 3}, 'PM10')
      end
    )
    {growth, hour} = r
    IO.puts "Growth: #{growth}, hour: #{hour}"

  end

  def stop_all() do
    :pollution_supervisor.stop()
    IO.puts "Supervisor stopped"

    :pollution_gen_server.stop()
    IO.puts "Server stopped"

  end

end
