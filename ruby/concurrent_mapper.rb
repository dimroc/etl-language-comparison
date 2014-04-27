require_relative "../ruby_parallel/mapper"

class ConcurrentMapper
  include Celluloid

  def map(input_file, output_file)
    Mapper.new(input_file, output_file).map
  end
end
