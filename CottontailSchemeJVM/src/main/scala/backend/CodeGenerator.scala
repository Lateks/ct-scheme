package backend

import java.io.PrintWriter

import backend.ast._
import backend.codegen.{CodeGenException, DebugClassWriter, SimpleMethodVisitor}
import lib._
import org.objectweb.asm.{Label, Type}
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

  def loadBoolean(method : SimpleMethodVisitor, b : Boolean): Unit = {
    val boolClassTypeName = getInternalName(classOf[CTBool])
    val ctObjectDescriptor = getDescriptor(classOf[CTObject])
    if (b) {
      method.emitGetStatic(boolClassTypeName, "trueInstance", ctObjectDescriptor)
    } else {
      method.emitGetStatic(boolClassTypeName, "falseInstance", ctObjectDescriptor)
    }
  }

  def loadUndefined(method : SimpleMethodVisitor): Unit = {
    method.emitGetStatic(getInternalName(classOf[CTUndefined]), "instance", getDescriptor(classOf[CTObject]))
  }

  def createArray[T](method : SimpleMethodVisitor, elems : List[T], loader : T => Unit): Unit = {
    method.loadConstant(elems.length.asInstanceOf[java.lang.Integer])
    method.visitTypeInsn(ANEWARRAY, getInternalName(classOf[Object]))
    for ((elem, i) <- elems.zipWithIndex) {
      method.dup()
      method.loadConstant(i.asInstanceOf[java.lang.Integer])
      loader(elem)
      method.storeInArray()
    }
  }

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
        loadBoolean(method, b)
      case LiteralNumber(n) =>
        val classTypeName = getInternalName(classOf[CTNumber])
        method.createAndDupObject(classTypeName)
        method.loadConstant(n.asInstanceOf[java.lang.Double])
        method.invokeConstructor(classTypeName, getDescriptor(classOf[Double]))
      case LiteralList(l) =>
        createArray(method, l, (elem : LiteralValue) => loadLiteral(method, elem))
        method.emitInvokeStatic(getInternalName(classOf[BuiltIns]), "toList", "([" + getDescriptor(classOf[Object]) + ")" + getDescriptor(classOf[Object]))
    }
  }

  val builtInProcedures = List("car", "cdr", "cons", "display", "eq?", "list", "newline", "not", "null?", "zero?", "+", "-", "*", "/")
  val builtInProceduresTakingArrayParam = List("list", "+", "-", "*", "/")

  def emitBuiltInProcedureCall(method : SimpleMethodVisitor, procedureName : String): Unit = {
    val builtInsClass = getInternalName(classOf[BuiltIns])
    val objectDescriptor = getDescriptor(classOf[Object])

    def makeBuiltInMethodDescriptor(numParams : Int, isArray : Boolean): String = {
      val paramTypes = List.range(0, numParams).map((_) => objectDescriptor).mkString("")
      val paramDesc = if (isArray) {
        "[" + paramTypes
      } else {
        paramTypes
      }

      "(" + paramDesc + ")" + objectDescriptor
    }

    def callMethod(name : String, numParams : Int, isArray : Boolean): Unit = {
      method.emitInvokeStatic(builtInsClass, name, makeBuiltInMethodDescriptor(numParams, isArray))
    }

    procedureName match {
      case "car" =>
        callMethod("car", 1, isArray = false)
      case "cdr" =>
        callMethod("cdr", 1, isArray = false)
      case "cons" =>
        callMethod("cons", 2, isArray = false)
      case "display" =>
        callMethod("display", 1, isArray = false)
      case "eq?" =>
        callMethod("areEq", 2, isArray = false)
      case "list" =>
        callMethod("toList", 1, isArray = true)
      case "newline" =>
        callMethod("newline", 0, isArray = false)
      case "not" =>
        callMethod("not", 1, isArray = false)
      case "null?" =>
        callMethod("isNull", 1, isArray = false)
      case "zero?" =>
        callMethod("isZero", 1, isArray = false)
      case "+" =>
        callMethod("plus", 1, isArray = true)
      case "-" =>
        callMethod("minus", 1, isArray = true)
      case "*" =>
        callMethod("mult", 1, isArray = true)
      case "/" =>
        callMethod("div", 1, isArray = true)
      case _ =>
        throw new CodeGenException("Unknown built-in procedure '" + procedureName + "'")
    }
  }

  def pushBuiltInProcedureArgs(method : SimpleMethodVisitor, procedureName : String, args : List[Expression]): Unit = {
    if (builtInProceduresTakingArrayParam.contains(procedureName)) {
      createArray(method, args, (elem : Expression) => pushExpressionResultToStack(method, elem))
    } else {
      for (arg <- args) {
        pushExpressionResultToStack(method, arg)
      }
    }
  }

  def emitProcedureCall(method : SimpleMethodVisitor, procedure : Expression, args : List[Expression]): Unit = {
    procedure match {
      case VariableReference(id) =>
        if (builtInProcedures.contains(id.uniqueName)) {
          pushBuiltInProcedureArgs(method, id.uniqueName, args)
          emitBuiltInProcedureCall(method, id.uniqueName)
        } else {
          throw new CodeGenException("Not implemented yet: procedure call to " + id.uniqueName)
        }
      case e =>
        throw new CodeGenException("Not implemented yet: first class procedure call")
    }
  }

  def emitBooleanConversion(method : SimpleMethodVisitor): Unit = {
    val booleanConverterDescriptor = "(" + getDescriptor(classOf[Object]) + ")" + getDescriptor(classOf[Boolean])
    method.emitInvokeStatic(getInternalName(classOf[BuiltIns]), "toBoolean", booleanConverterDescriptor)
  }

  def emitConditional(method : SimpleMethodVisitor, cond : Expression, thenBranch : Expression, elseBranch : Expression): Unit = {
    val thenLabel = new Label()
    val exitLabel = new Label()

    pushExpressionResultToStack(method, cond)
    emitBooleanConversion(method)
    method.visitJumpInsn(IFNE, thenLabel)

    pushExpressionResultToStack(method, elseBranch)
    method.visitJumpInsn(GOTO, exitLabel)

    method.visitLabel(thenLabel)

    pushExpressionResultToStack(method, thenBranch)

    method.visitLabel(exitLabel)
  }

  def generateExpression(method : SimpleMethodVisitor, expression : Expression, keepResultOnStack : Boolean): Unit = {
    expression match {
      case ProcedureCall(proc, args, tc) => // TODO: Not handling tail calls yet
        emitProcedureCall(method, proc, args)
      case ValueExpression(lit) =>
        loadLiteral(method, lit)
      case Conditional(cond, thenBranch, elseBranch) =>
        emitConditional(method, cond, thenBranch, elseBranch)
      case SequenceExpression(sequenceType, exprs) =>
        throw new CodeGenException("Expression type not implemented yet: sequence expressions")
      case Assignment(id, expr) =>
        throw new CodeGenException("Expression type not implemented yet: assignment expression")
      case VariableReference(id) =>
        throw new CodeGenException("Expression type not implemented yet: variable reference expression")
      case Closure(id) =>
        throw new CodeGenException("Expression type not implemented yet: closure")
      case UndefinedValue() => ()
    }

    expression match {
      case UndefinedValue() =>
        if (keepResultOnStack)
          loadUndefined(method)
      case _ =>
        if (!keepResultOnStack)
          method.popStack()
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
