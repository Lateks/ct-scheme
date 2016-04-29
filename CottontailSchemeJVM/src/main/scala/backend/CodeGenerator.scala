package backend

import java.io.{PrintWriter}

import backend.ast._
import backend.codegen.{CodeGenException, DebugClassWriter, SimpleMethodVisitor}
import lib._
import org.objectweb.asm.Type
import org.objectweb.asm.Opcodes._

object CodeGenerator {

  val debug = true
  val stdout = new PrintWriter(System.out)

  def getInternalName(c : Class [_]): String = {
    Type.getType(c).getInternalName
  }

  def getDescriptor(c : Class [_]): String = {
    Type.getType(c).getDescriptor
  }

  def declareClass(className : String, parentClassType : String): DebugClassWriter = {
    val c = new DebugClassWriter(debug, stdout)
    c.declareClass(className, parentClassType)
    c
  }

  val builtInProcedures = List("display", "newline")

  def loadLiteral(method : SimpleMethodVisitor, literalValue: LiteralValue): Unit = {
    def initializeObjectWithString(classTypeName : String, param : String): Unit = {
      method.createAndDupObject(classTypeName)
      method.loadConstant(param)
      method.invokeConstructor(classTypeName, getDescriptor(classOf[String]))
    }

    literalValue match {
      case LiteralString(s) =>
        val classTypeName = getInternalName(classOf[CTString])
        initializeObjectWithString(classTypeName, s)
      case LiteralSymbol(s) =>
        val classTypeName = getInternalName(classOf[CTSymbol])
        initializeObjectWithString(classTypeName, s)
      case LiteralBoolean(b) =>
        val boolClassTypeName = getInternalName(classOf[CTBool])
        val ctObjectDescriptor = getDescriptor(classOf[CTObject])
        if (b) {
          method.visitFieldInsn(GETSTATIC, boolClassTypeName, "trueInstance", ctObjectDescriptor)
        } else {
          method.visitFieldInsn(GETSTATIC, boolClassTypeName, "falseInstance", ctObjectDescriptor)
        }
      case LiteralNumber(n) =>
        val classTypeName = getInternalName(classOf[CTNumber])
        method.createAndDupObject(classTypeName)
        method.loadConstant(n.asInstanceOf[java.lang.Double])
        method.invokeConstructor(classTypeName, getDescriptor(classOf[Double]))
      case LiteralList(l) => throw new CodeGenException("Not implemented yet: lists")
    }
  }

  def emitBuiltInProcedureCall(method : SimpleMethodVisitor, procedureName : String): Unit = {
    procedureName match {
      case "display" =>
        val objectDescriptor = getDescriptor(classOf[Object])
        method.visitMethodInsn(INVOKESTATIC, getInternalName(classOf[BuiltIns]), "display", "(" + objectDescriptor + ")" + objectDescriptor, false)
      case "newline" =>
        val objectDescriptor = getDescriptor(classOf[Object])
        method.visitMethodInsn(INVOKESTATIC, getInternalName(classOf[BuiltIns]), "newline", "()" + objectDescriptor, false)
      case s =>
        throw new CodeGenException("Unknown built-in procedure '" + procedureName + "'")
    }
  }

  def emitProcedureCall(method : SimpleMethodVisitor, procedure : Expression, args : List[Expression]): Unit = {
    procedure match {
      case VariableReference(id) =>
        if (builtInProcedures.contains(id.uniqueName)) {
          for (arg <- args) {
            pushExpressionResultToStack(method, arg)
          }
          emitBuiltInProcedureCall(method, id.uniqueName)
        } else {
          throw new CodeGenException("Not implemented yet: procedure call to " + id.uniqueName)
        }
      case e =>
        throw new CodeGenException("Not implemented yet: first class procedure call")
    }
  }

  def generateExpression(method : SimpleMethodVisitor, expression : Expression, keepResultOnStack : Boolean): Unit = {
    expression match {
      case ProcedureCall(proc, args, tc) => // TODO: Not handling tail calls yet
        emitProcedureCall(method, proc, args)
      case ValueExpression(lit) =>
        loadLiteral(method, lit)
      case _ =>
        throw new CodeGenException("Expression type not implemented yet: " + expression)
    }

    if (!keepResultOnStack) {
      method.visitInsn(POP)
    }
  }

  def pushExpressionResultToStack(method : SimpleMethodVisitor, expression : Expression): Unit = {
    generateExpression(method, expression, true)
  }

  def generateTopLevelExpression(method : SimpleMethodVisitor, expression: Expression): Unit = {
    generateExpression(method, expression, false)
  }

  def generateTopLevelModule(program: ProgramSyntaxTree, mainMethod : SimpleMethodVisitor): Unit = {
    // TODO: variable declarations
    // TODO: procedure definitions

    mainMethod.visitCode()

    for (expr <- program.expressions) {
      generateTopLevelExpression(mainMethod, expr)
    }

    mainMethod.emitReturn()

    mainMethod.setMaxs()
    mainMethod.visitEnd()
  }

  def generateCodeFor(program : ProgramSyntaxTree) : Unit = {
    val mainClass = declareClass(program.programName, getInternalName(classOf[Object]))

    val mainMethodDescriptor = "([" + getDescriptor(classOf[String]) + ")" + Type.VOID_TYPE.getDescriptor
    val mainMethod = new SimpleMethodVisitor(mainClass, ACC_PUBLIC + ACC_STATIC, "main", mainMethodDescriptor)

    generateTopLevelModule(program, mainMethod)

    mainClass.visitEnd()
    mainClass.writeToDisk()
  }

}
