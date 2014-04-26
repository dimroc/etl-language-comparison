defmodule Reducer do
  def reduce(input_dir, destination) do
    files = generate_input_files(input_dir)
    current = self()
    Enum.traverse(files, fn f -> spawn_link(Reducer, :read_file, [current, f]) end)

    final = Map.to_list(receive_reduction(length(files), %{}))
    |> Enum.sort fn a, b -> 
      { _, counta } = a
      { _, countb } = b
      counta > countb
    end

    write_to_destination(destination, final)
  end

  def read_file(parent, file) do
    IO.puts "reducing file #{file}"
    stream = File.stream!(file)
    |> Stream.map fn line -> String.split(line, "\t") end

    # Attempted to 'fold' the stream into a map, but couldn't find the appropriate method.
    # Settled for Enum.to_list.
    map = List.foldl(Enum.to_list(stream), %{}, &Reducer.reduce_stream/2)

    send parent, { :ok, map }
  end

  def reduce_stream(pair, acc) do
    new_entry = case pair do
      [hood, count] -> Map.new [{binary_to_atom(hood), binary_to_integer String.strip(count)}]
    end

    Map.merge(acc, new_entry, fn _, v1, v2 -> v1 + v2 end)
  end

  defp receive_reduction(count, acc) when count <= 0 do
    acc
  end

  defp receive_reduction(count, acc) do
    receive do
      { :ok, reduction } ->
        Map.merge(acc, receive_reduction(count - 1, reduction), fn k, v1, v2 -> v1 + v2 end)
    end
  end

  defp write_to_destination(destination, list) do
    File.mkdir_p!(Path.dirname(destination))
    stream = File.stream!(destination, [:delayed_write])
    Enum.into(list, stream, fn pair ->
      case pair do
        {hood, count} -> "#{hood}\t#{count}\n"
      end
    end)
  end

  defp generate_input_files(input_dir) do
    File.ls!(input_dir)
    |> Enum.map(fn p -> Path.join(input_dir, p) end)
    |> Enum.filter(fn p -> !File.dir?(p) end)
  end
end
