defmodule Mix.Tasks.Mapreduce do
  use Mix.Task

  @moduledoc """
  Run a map reduce against all tweets to check for the frequency of a term based on neighborhood
  """
  def run([arg]) do
    actor = arg_to_actor(arg)

    generate_input_files("../tmp/tweets/")
    |> map_to_tasks(actor)
    |> reduce_tasks()
    |> sort_by_frequency_and_hood()
    |> write_to_disk("../tmp/elixir_#{arg}_output")
  end

  defp arg_to_actor("binary"), do: BinaryMapActor
  defp arg_to_actor("regex"), do: RegexMapActor

  defp generate_input_files(input_dir) do
    File.ls!(input_dir)
    |> Enum.map(fn p -> Path.join(input_dir, p) end)
    |> Enum.filter(fn p -> !File.dir?(p) end)
  end

  defp map_to_tasks(files, actor) do
    Enum.map(files, &Task.async(actor, :map, [&1]))
  end

  defp reduce_tasks(tasks) do
    Enum.reduce(tasks, HashDict.new, &merge/2)
  end

  defp merge(task, acc) do
    HashDict.merge(Task.await(task, :infinity), acc,
                   fn _k, v1, v2 -> v1 + v2 end)
  end

  def sort_by_frequency_and_hood(dict) do
    Enum.sort dict, fn {v1k, v1v}, {v2k, v2v} ->
      v1v > v2v || (v1v == v2v && v1k < v2k)
    end
  end

  defp write_to_disk(list, destination) do
    File.mkdir_p!(Path.dirname(destination))
    stream = File.stream!(destination, [:delayed_write])
    Enum.into(list, stream, fn {hood, count} ->
      "#{hood}\t#{count}\n"
    end)
  end
end
