require_relative "mapper"
input_file = ARGV[0]
output_file = ARGV[1]
Mapper.new(input_file, output_file).map
