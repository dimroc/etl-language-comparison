defmodule BinaryMapActor do
  def map(file) do
    newline_pattern = :binary.compile_pattern("\n")
    knicks_pattern = :binary.compile_pattern(Utils.permute_case("knicks"))
    tab_pattern = :binary.compile_pattern("\t")

    # Load the whole file in memory and work with offsets
    reduce_file(File.read!(file), HashDict.new,
                newline_pattern, tab_pattern, knicks_pattern)
  end

  def reduce_file(binary, dict, newline, tab, pattern) do
    case :binary.match(binary, newline) do
      {pos, _} ->
        dict =
          case :binary.match(binary, pattern, scope: {0, pos}) do
            {_, _} ->
              hood = Utils.parse_hood(binary, tab, pos)
              HashDict.update(dict, hood, 1, &(&1 + 1))
            :nomatch ->
              dict
          end

        rest = :binary.part(binary, pos+1, byte_size(binary)-pos-1)
        reduce_file(rest, dict, newline, tab, pattern)
      :nomatch ->
        dict
    end
  end
end
