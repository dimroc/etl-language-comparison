defmodule MapActor do
  @pattern ~r/knicks/i
  
  def map(file) do
    match = @pattern
    #IO.puts "mapping file #{file}"
    File.stream!(file)
    |> Stream.filter( fn(line) -> Regex.match?(match,line) end)
    |> Enum.reduce(HashDict.new, &reduce_stream/2)
  end

  def reduce_stream(message, acc) do
    hood = String.split(message,"\t") |> Enum.at(1)
    hook = String.to_atom(hood)
    HashDict.update(acc, hook, 1, &(&1 + 1))
  end

end
