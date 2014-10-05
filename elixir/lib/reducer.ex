defmodule Reducer do
  @dict HashDict.new

  def reduce(input_dir, destination) do
    files = generate_input_files(input_dir)
    current = self()
    Enum.each(files, fn f -> spawn_link(Reducer, :read_file, [current, f]) end)

    final =
      receive_reduction(length(files), @dict)
      |> Enum.sort fn a, b ->
        {_, counta} = a
        {_, countb} = b
        counta > countb
      end

    write_to_destination(destination, final)
  end

  def read_file(parent, file) do
    IO.puts "reducing file #{file}"

    map =
      File.stream!(file)
      |> Stream.map(fn line -> String.split(line, "\t") end)
      |> Enum.reduce(@dict, &reduce_stream/2)

    send parent, { :ok, map }
  end

  def reduce_stream([hood, count], acc) do
    hook     = String.to_atom(hood)
    {int, _} = Integer.parse(count)
    HashDict.update(acc, hook, int, &(&1 + int))
  end

  defp receive_reduction(0, acc) do
    acc
  end

  defp receive_reduction(count, acc) do
    receive do
      { :ok, reduction } ->
        HashDict.merge(acc, receive_reduction(count - 1, reduction), fn _k, v1, v2 -> v1 + v2 end)
    end
  end

  defp write_to_destination(destination, list) do
    File.mkdir_p!(Path.dirname(destination))
    stream = File.stream!(destination, [:delayed_write])
    Enum.into(list, stream, fn {hood, count} ->
      "#{hood}\t#{count}\n"
    end)
  end

  defp generate_input_files(input_dir) do
    File.ls!(input_dir)
    |> Enum.map(fn p -> Path.join(input_dir, p) end)
    |> Enum.filter(fn p -> !File.dir?(p) end)
  end
end
