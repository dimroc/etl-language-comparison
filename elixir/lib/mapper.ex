defmodule Mapper do
  def map(input_dir) do
    generate_input_files(input_dir)
    |> Enum.map(&Task.async(MapActor, :map, [&1]))
    |> Enum.map(&Task.await(&1, :infinity))
  end

  defp generate_input_files(input_dir) do
    File.ls!(input_dir)
    |> Enum.map(fn p -> Path.join(input_dir, p) end)
    |> Enum.filter(fn p -> !File.dir?(p) end)
  end
end
