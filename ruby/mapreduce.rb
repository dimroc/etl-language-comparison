require 'celluloid/autostart'
require 'fileutils'
require_relative "concurrent_mapper"
require_relative "../ruby_parallel/reducer"

input_dir = "../tmp/tweets"
output_dir = "../tmp/ruby_output"
destination = "../tmp/ruby_output/final/final"

FileUtils.mkdir_p output_dir

input_files = Dir.glob(input_dir + "/tweets_*")
output_files = input_files.map.with_index { |_, i| File.join(output_dir, "output_#{i}") }

def run_mapping_concurrently(input_files, output_files)
  pool = ConcurrentMapper.pool

  futures = input_files.zip(output_files).map do |input, output|
    pool.future.map(input, output)
  end

  futures.map(&:value)
end

run_mapping_concurrently(input_files, output_files)
Reducer.new(output_dir, destination).reduce
