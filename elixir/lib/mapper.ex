defmodule Mapper do
  def map(input_dir) do
    input_files = generate_input_files(input_dir)

    input_files
    |> Enum.each(fn t -> Task.start_link(MapActor, :map, [self(), t]) end)

    listen_for_children(length(input_files), [])
  end

  defp listen_for_children(0, acc) do
    acc
  end

  defp listen_for_children(count, acc) do
    receive do
      {:ok, mapping} -> listen_for_children(count - 1, [mapping|acc])
    end
  end

  defp generate_input_files(input_dir) do
    File.ls!(input_dir)
    |> Enum.map(fn p -> Path.join(input_dir, p) end)
    |> Enum.filter(fn p -> !File.dir?(p) end)
  end
end
