package backend

import backend.ast.{ReadFailure, ReadSuccess, Reader}

import scala.io.Source

object Program {
  def main(args: Array[String]): Unit = {
    val input = Source.stdin.getLines

    Reader.readAST(input) match {
      case ReadSuccess(program) =>
        println("AST parsing succeeded: " + program.toString)
        CodeGenerator.generateCodeFor(program)
      case ReadFailure(message) => println("AST parsing failed: " + message)
    }
  }
}
