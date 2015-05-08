defmodule Mix.Tasks.Mapreduce do
  use Mix.Task

  @moduledoc """
  Run a map reduce against all tweets to check for the frequency of a term based on neighborhood
  """
  def run(_) do
    mappings = Mapper.map("../tmp/tweets/")
    Reducer.reduce(mappings, "../tmp/elixir_output")
  end
end
