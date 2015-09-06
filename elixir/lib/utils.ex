defmodule Utils do
  def parse_hood(binary, tab, pos) do
    {a, _} = :binary.match(binary, tab)
    {b, _} = :binary.match(binary, tab, scope: {a+1, pos-a-1})
    String.to_atom :binary.part(binary, a+1, b-a-1)
  end

  import Bitwise

  def permute_case(text) do
    list = String.graphemes(text)
    permute_case(list, 0, 0, bsl(1, length(list)), [], [])
  end

  defp permute_case(_, l, _, l, _, acc),
    do: Enum.reverse(acc)

  defp permute_case([h | t], i, j, l, word, acc) when band(j, 1) == 0,
    do: permute_case(t, i, bsr(j, 1), l, [String.upcase(h) | word], acc)

  defp permute_case([h | t], i, j, l, word, acc),
    do: permute_case(t, i, bsr(j, 1), l, [String.downcase(h) | word], acc)

  defp permute_case([], i, _, l, word, acc) do
    list = Enum.reverse(word)
    j = i+1
    permute_case(list, j, j, l, [], [IO.iodata_to_binary(list) | acc])
  end
end
