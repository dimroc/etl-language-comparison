defmodule MapActor do
  @pattern ~r/knicks/i

  def map(parent, {input_file, output_file}) do
    IO.puts "mapping file #{input_file}"
    output = File.stream!(output_file, [:delayed_write])
    File.stream!(input_file)
    |> Enum.into(output, fn line -> map_line(line) end)

    send parent, :ok
  end

  def map_line(line) do
    [_, hood, _, message] = String.split(line, "\t")

    if message =~ @pattern do
      "#{hood}\t1\n"
    else
      "#{hood}\t0\n"
    end
  end
end
