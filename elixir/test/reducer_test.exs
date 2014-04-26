defmodule ReducerTest do
  use ExUnit.Case
  import MixHelpers

  test "reducer correctly writes counts" do
    destination = "/tmp/etl_elixir_test/final/final"
    File.rm(destination)

    Reducer.reduce("../fixtures", destination)

    assert_file(destination)
    stream = Stream.take File.stream!(destination), 1
    [line | _] = Enum.to_list(stream)
    [_ | count] = String.split(line, "\t")
    assert String.strip(List.last(count)) == "2"
  end
end
