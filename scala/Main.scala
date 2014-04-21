import mapreduce._

object Main extends App {
  Mapper.map("../tmp/tweets/", "../tmp/scala_output/")
}
