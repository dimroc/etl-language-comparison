import mapreduce._

object Main extends App {
  val mappings = Mapper.mapWithAkka("../tmp/tweets/")
//  val mappings = Mapper.mapWithPar("../tmp/tweets/")
//  val mappings = Mapper.mapWithFuture("../tmp/tweets/")
  Reducer.reduce(mappings, "../tmp/scala_output")
}
