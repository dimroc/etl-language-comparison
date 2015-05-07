import mapreduce._

object Main extends App {
  val mappings = Mapper.map("../tmp/tweets/")
  Reducer.reduce(mappings, "../tmp/scala_output")
}
