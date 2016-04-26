package backend

import scala.io.Source
import org.json4s._
import org.json4s.native.JsonMethods._

object Program {
  def main(args: Array[String]): Unit = {
    val input = Source.stdin.getLines
    if (input.isEmpty) {
      println("No input")
    } else {
      val json = parse(input.mkString("\n"))
      println(json)
    }

    println("Hello from Scala!")
  }
}
