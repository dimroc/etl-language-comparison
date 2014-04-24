import mapreduce._

object Main extends App {
  Mapper.map("../tmp/tweets/", "../tmp/scala_output/")
  Reducer.reduce("../tmp/scala_output/", "../tmp/scala_output/final/final")
}
