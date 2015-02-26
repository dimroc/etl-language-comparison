require 'fileutils'

# Code shared between ruby/ and ruby_parallel/
class Mapper
  REGEX = Regexp.compile /knicks/i

  attr_accessor :input_file, :output_file
  def initialize(input_file, output_file)
    @input_file = input_file
    @output_file = output_file
  end

  def map
    puts "mapping #{input_file} to #{output_file}"

    FileUtils.mkdir_p File.dirname(output_file)
    input = File.open(input_file, "r")
    out = File.open(output_file, "w")

    input.each_line do |line|
      tokens = line.split "\t"
      hood = tokens[1]
      message = tokens[3]

      if message =~ REGEX
        out.write "#{hood}\t1\n"
      else
        out.write "#{hood}\t0\n"
      end
    end

    input.close
    out.close
  end
end
