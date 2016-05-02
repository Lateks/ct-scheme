package backend

import backend.ast.{ReadError, ReadFailure, ReadSuccess, Reader}

import scala.io.Source

object Program {
  def main(args: Array[String]): Unit = {
    val input = Source.stdin.getLines

    Reader.readAST(input) match {
      case ReadSuccess(program) =>
        CodeGenerator.generateCodeFor(program)
      case ReadFailure(message) => println("AST parsing failed: " + message)
      case ReadError(errMsg) => println(errMsg)
    }
  }
}
