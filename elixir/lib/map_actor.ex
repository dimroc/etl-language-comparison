defmodule MapActor do
  @pattern ~r/knicks/i

  def map(parent, file) do
    #IO.puts "mapping file #{file}"

    mapping =
      File.stream!(file)
      |> Stream.map(fn line -> String.split(line, "\t") end)
      |> Enum.reduce(HashDict.new, &reduce_stream/2)

    send parent, { :ok, mapping }
  end

  def reduce_stream([_, hood, _, message], acc) do
    hook = String.to_atom(hood)
    if message =~ @pattern do
      HashDict.update(acc, hook, 1, &(&1 + 1))
    else
      acc
    end
  end
end
