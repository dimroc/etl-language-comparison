defmodule Reducer do
  @dict HashDict.new

  def reduce(mappings, destination) do
    reduction = Enum.reduce(mappings, HashDict.new, &reduce_mappings/2)
    final = HashDict.to_list(reduction)
            # Sort by frequency count first, neighborhood second.
            |> Enum.sort fn { v1k, v1v }, { v2k, v2v } -> v1v > v2v || (v1v == v2v && v1k < v2k) end
    write_to_destination(destination, final)
  end

  def reduce_mappings(new_hash, acc) do
    HashDict.merge(new_hash, acc, fn _k, v1, v2 -> v1 + v2 end)
  end

  defp write_to_destination(destination, list) do
    File.mkdir_p!(Path.dirname(destination))
    stream = File.stream!(destination, [:delayed_write])
    Enum.into(list, stream, fn {hood, count} ->
      "#{hood}\t#{count}\n"
    end)
  end
end
