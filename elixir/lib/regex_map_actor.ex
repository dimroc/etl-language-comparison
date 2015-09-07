defmodule RegexMapActor do
  @pattern ~r/knicks/i

  def map(file) do
    tab_pattern = :binary.compile_pattern("\t")
    Enum.reduce(File.stream!(file), HashDict.new,
                &reduce_stream(&1, &2, tab_pattern))
  end

  def reduce_stream(line, acc, tab) do
    if line =~ @pattern do
      hood = Utils.parse_hood(line, tab, byte_size(line))
      HashDict.update(acc, hood, 1, &(&1 + 1))
    else
      acc
    end
  end
end
