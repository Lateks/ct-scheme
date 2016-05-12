package backend

import backend.ast.{ReadError, ReadFailure, ReadSuccess, Reader}

import scala.io.Source

object Program {
  def main(args: Array[String]): Unit = {
    var debug = false
    var optimizeTailCalls = true
    for (arg <- args) {
      arg match {
        case "--notc" | "-t" => optimizeTailCalls = false
        case "--debug" | "-d" => debug = true
      }
    }

    val input = Source.stdin.getLines

    Reader.readAST(input) match {
      case ReadSuccess(program) =>
        CodeGenerator.generateCodeFor(program, debug, optimizeTailCalls)
      case ReadFailure(message) => println("AST parsing failed: " + message)
      case ReadError(errMsg) => println(errMsg)
    }
  }
}
