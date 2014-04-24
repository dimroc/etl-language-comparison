package mapreduce

import org.scalatest._
import java.io._
import scala.io._

class ReducerSpec extends FlatSpec with Matchers {
  "A Reducer" should "reduce counts from fixures" in {
    println(s"Working out of ${new File(".").getAbsolutePath()}")
    Reducer.reduce("../fixtures", "/tmp/etl_scala_test/final")

    val file = new File("/tmp/etl_scala_test/final")
    val it = Source.fromFile(file.getAbsolutePath()).getLines()
    val first = it.next()
    val tokens = first.split('\t') // hood\tcount
    println(s"Retrieved row: ${tokens mkString("\t")}")
    tokens(1) should be ("2")
  }
}

