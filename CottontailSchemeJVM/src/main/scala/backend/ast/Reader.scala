package backend.ast

import org.json4s.native.JsonMethods._

import org.json4s._

abstract class ReadResult
case class ReadSuccess(program : ProgramSyntaxTree) extends ReadResult
case class ReadFailure(message : String) extends ReadResult

object Reader {

  def readAST(input : Iterator[String]) : ReadResult = {
    if (input.isEmpty) {
      ReadFailure("No input")
    } else {
      val rawJson = input.mkString("\n")
      val json = parse(rawJson)
      ReadFailure("Parsing not implemented yet, got json: " + json.toString())
    }
  }

}
