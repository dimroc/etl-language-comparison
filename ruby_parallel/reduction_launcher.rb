require_relative "reducer"
input_dir = "../tmp/ruby_parallel_output"
destination = "../tmp/ruby_parallel_output/final/final"
Reducer.new(input_dir, destination).reduce
