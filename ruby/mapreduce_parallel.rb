require 'parallel'
require 'fileutils'

input_dir = "../tmp/tweets"
input_files = Dir.glob(input_dir + "/tweets_*")

class Mapper
  REGEX = Regexp.compile /knicks/i

  def read(input_file)
    hash = {}
    hash.default = 0

    File.open(input_file, "r") do |input|
      input.each_line do |line|
        tokens = line.split "\t"
        hood = tokens[1]
        message = tokens[3]

        hash[hood] += 1 if message =~ REGEX
      end
    end
    hash
  end
end

def run_mapping_concurrently(input_files)
  Parallel.map(input_files) do |input|
    Mapper.new.read(input)
  end
end

def write_output(hash)
  destination = "../tmp/ruby_parallel_output/final/final"
  sorted = hash.to_a.sort_by { |p| [-p[1], p[0]] }
  FileUtils.mkdir_p(File.dirname(destination))
  File.open(destination, "w") do |f|
    sorted.each do |pair|
      f.write "#{pair[0]}\t#{pair[1]}\n"
    end
  end
end

def reduce(mappings)
  mappings.inject({}) do |memo, reduction|
    memo.merge!(reduction) { |_, v1, v2| v1 + v2 }
  end
end

mappings = run_mapping_concurrently(input_files)
write_output(reduce(mappings))
