defmodule MapActor do
  @pattern ~r/knicks/i

  def map(file) do
    bin_pattern = :binary.compile_pattern("\t")

    File.stream!(file)
    |> Stream.map(fn line -> String.split(line, bin_pattern) end)
    |> Enum.reduce(HashDict.new, &reduce_stream/2)
  end

  def reduce_stream([_, hood, _, message], acc) do
    if message =~ @pattern do
      hood = String.to_atom(hood)
      HashDict.update(acc, hood, 1, &(&1 + 1))
    else
      acc
    end
  end
end
