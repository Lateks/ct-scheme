package backend

import backend.ast._

import java.nio.file.Paths
import scala.io.Source
import scala.language.postfixOps
import scala.sys.process._
import scala.sys

object Program {
  def main(args: Array[String]): Unit = {
    var debug = false
    var optimizeTailCalls = true
    var optimizeTailRecursion = true
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
        case "--notailrec" =>
          optimizeTailRecursion = false
        case s =>
          if (fileName == null) {
            fileName = s
          } else {
            println("Unrecognized flag " + s)
          }
      }
    }

    val fileSeparator = sys.props("file.separator")
    val jarPath = Paths.get(Program.getClass.getProtectionDomain.getCodeSource.getLocation.getPath.replaceFirst("^/([A-Z]:)", "$1").replace("/", fileSeparator)).getParent.toString
    val input = if (fileName == null) {
      Source.stdin.getLines.mkString("\n")
    } else {
      val dotnetPath =
        if (useMono) {
            "mono " + jarPath + fileSeparator + "CottontailScheme.exe"
        } else if (inDevelopmentEnvironment) {
            ".\\CottontailScheme\\bin\\Debug\\CottontailScheme.exe"
        } else {
          jarPath + fileSeparator + "CottontailScheme.exe"
        }
      val command = dotnetPath + " -json " + fileName
      command !!
    }

    Reader.readAST(input) match {
      case ReadSuccess(program) =>
        CodeGenerator.generateCodeFor(program, debug, optimizeTailCalls, optimizeTailRecursion)
      case ReadFailure(message) => println("AST parsing failed: " + message)
      case ReadError(errMsg) => println(errMsg)
    }
  }
}
