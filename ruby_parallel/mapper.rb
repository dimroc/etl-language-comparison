require 'fileutils'

class Mapper
  attr_accessor :input_file, :output_file
  def initialize(input_file, output_file)
    @input_file = input_file
    @output_file = output_file
  end

  def map
    f = File.open(input_file, "r")
    FileUtils.mkdir_p File.dirname(output_file)
    out = File.open(output_file, "w")

    f.each_line do |line|
      tokens = line.split "\t"
      hood = tokens[1]
      message = tokens[3]

      if message =~ /knicks/i
        out.write "#{hood}\t1\n"
      else
        out.write "#{hood}\t0\n"
      end
    end
    f.close
  end
end

input_file = ARGV[0]
output_file = ARGV[1]
Mapper.new(input_file, output_file).map
