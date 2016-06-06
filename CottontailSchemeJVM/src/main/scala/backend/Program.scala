package backend

import backend.ast._

import scala.io.Source
import scala.language.postfixOps
import scala.sys.process._

object Program {
  def main(args: Array[String]): Unit = {
    var debug = false
    var optimizeTailCalls = true
    var fileName: String = null
    var inDevelopmentEnvironment = false
    var useMono = false
    for (arg <- args) {
      arg match {
        case "--notc" | "-t" =>
          optimizeTailCalls = false
        case "--debug" | "-d" =>
          debug = true
        case "--dev" =>
          inDevelopmentEnvironment = true
        case "--mono" =>
          useMono = true
        case s =>
          if (fileName == null) {
            fileName = s
          } else {
            println("Unrecognized flag " + s)
          }
      }
    }

    val input = if (fileName == null) {
      Source.stdin.getLines.mkString("\n")
    } else {
      val dotnetPath =
        if (useMono) {
            "mono CottontailScheme.exe"
        } else if (inDevelopmentEnvironment) {
            ".\\CottontailScheme\\bin\\Debug\\CottontailScheme.exe"
        } else {
          "CottontailScheme.exe"
        }
      val command = dotnetPath + " -json " + fileName
      command !!
    }

    Reader.readAST(input) match {
      case ReadSuccess(program) =>
        CodeGenerator.generateCodeFor(program, debug, optimizeTailCalls)
      case ReadFailure(message) => println("AST parsing failed: " + message)
      case ReadError(errMsg) => println(errMsg)
    }
  }
}
