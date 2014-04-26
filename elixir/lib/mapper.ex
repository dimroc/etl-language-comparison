defmodule Mapper do
  def map(input_dir, output_dir) do
    input_files = generate_input_files(input_dir)
    output_files = generate_output_files(input_files, output_dir)

    input_files
    |> Enum.zip(output_files)
    |> Enum.traverse(fn t -> spawn(MapActor, :map, [self()] ++ tuple_to_list(t)) end)

    listen_for_children(length(input_files))
  end

  defp listen_for_children(count) when count <= 0 do
  end

  defp listen_for_children(count) do
    receive do
      :ok -> listen_for_children(count - 1)
    end
  end

  defp generate_output_files(input_files, output_dir) do
    File.mkdir_p! output_dir
    Enum.map(input_files, fn f -> Path.join(output_dir, Path.basename f) end)
  end

  defp generate_input_files(input_dir) do
    File.ls!(input_dir)
    |> Enum.map(fn p -> Path.join(input_dir, p) end)
    |> Enum.filter(fn p -> !File.dir?(p) end)
  end
end
