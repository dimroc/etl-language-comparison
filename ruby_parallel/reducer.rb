require 'fileutils'

class Reducer
  attr_accessor :input_dir, :destination
  def initialize(input_dir, destination)
    @input_dir = input_dir
    @destination = destination
  end

  def reduce
    hash = {}
    input_files.each do |filepath|
      single_hash = reduce_from_file(filepath)
      hash.merge!(single_hash) { |_, v1, v2| v1 + v2 }
    end

    write_output hash
  end

  def input_files
    @input_files ||= begin
                       Dir.glob(input_dir + "/output_*")
                     end
  end

  def reduce_from_file(filepath)
    hash = {}
    File.open(filepath, "r") do |f|
      f.each_line do |line|
        hood, count = line.split "\t"
        hood = hood.to_sym
        hash[hood] = count.to_i + hash.fetch(hood, 0)
      end
    end
    hash
  end

  def write_output(hash)
    sorted = hash.to_a.sort_by { |p| -p[1] }
    FileUtils.mkdir_p(File.dirname(destination))
    File.open(destination, "w") do |f|
      sorted.each do |pair|
        f.write "#{pair[0]}\t#{pair[1]}\n"
      end
    end
  end
end

input_dir = "../tmp/ruby_parallel_output"
destination = "../tmp/ruby_parallel_output/final/final"
Reducer.new(input_dir, destination).reduce
