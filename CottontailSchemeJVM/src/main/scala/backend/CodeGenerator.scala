package backend

import java.io.PrintWriter
import java.lang.invoke.MethodHandle

import backend.ast.SequenceType.SequenceType
import backend.ast._
import backend.codegen.{CodeGenException, DebugClassWriter, SimpleMethodVisitor}
import lib._
import org.objectweb.asm.{Label, Type}
import org.objectweb.asm.Opcodes._

object CodeGenerator {

  abstract class Variable
  case class StaticField(name : String) extends Variable
  case class LocalVariable(name : String, index : Int) extends Variable

  case class ProgramState(mainClass : DebugClassWriter, scope : Map[String, Variable], methods : Map[String, SimpleMethodVisitor])

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

  val builtInProcedures = List("car", "cdr", "cons", "display", "eq?", "list", "newline", "not", "null?", "zero?", "+", "-", "*", "/", "<", ">")
  val builtInProceduresTakingArrayParam = List("list", "+", "-", "*", "/", "<", ">")

  def makeObjectMethodDescriptor(numParams : Int, isArray : Boolean): String = {
    val objectDescriptor = getDescriptor(classOf[Object])
    val paramTypes = List.range(0, numParams).map((_) => objectDescriptor).mkString("")
    val paramDesc = if (isArray) {
      "[" + paramTypes
    } else {
      paramTypes
    }

    "(" + paramDesc + ")" + objectDescriptor
  }

  def convertBuiltInProcedureName(procedureName : String): String = {
    procedureName match {
      case "eq?" => "areEq"
      case "list" => "toList"
      case "null?" => "isNull"
      case "zero?" => "isZero"
      case "+" => "plus"
      case "-" => "minus"
      case "*" => "mult"
      case "/" => "div"
      case "<" => "lessThan"
      case ">" => "greaterThan"
      case "car" | "cdr" | "cons" | "display" | "newline" | "not" => procedureName
      case _ => throw new CodeGenException("Unknown built-in procedure '" + procedureName + "'")
    }
  }

  def emitBuiltInProcedureCall(method : SimpleMethodVisitor, procedureName : String): Unit = {

    val convertedProcedureName = convertBuiltInProcedureName(procedureName)

    val paramCount =
      procedureName match {
        case "newline" => 0
        case "cons" | "eq?" => 2
        case _ => 1
      }

    val builtInsClass = getInternalName(classOf[BuiltIns])
    val isArray = builtInProceduresTakingArrayParam.contains(procedureName)
    val methodDescriptor = makeObjectMethodDescriptor(paramCount, isArray)
    method.emitInvokeStatic(builtInsClass, convertedProcedureName, methodDescriptor)
  }

  def pushArgs(method : SimpleMethodVisitor, state : ProgramState, args : List[Expression]): Unit = {
    for (arg <- args) {
      pushExpressionResultToStack(method, state, arg)
    }
  }

  def pushBuiltInProcedureArgs(method : SimpleMethodVisitor, state : ProgramState, procedureName : String, args : List[Expression]): Unit = {
    if (builtInProceduresTakingArrayParam.contains(procedureName)) {
      createArray(method, args, (elem : Expression) => pushExpressionResultToStack(method, state, elem))
    } else {
      pushArgs(method, state, args)
    }
  }

  def emitFirstClassProcedureCallToObjectOnStack(method : SimpleMethodVisitor, state : ProgramState, args : List[Expression]): Unit = {
    method.visitTypeInsn(CHECKCAST, getInternalName(classOf[MethodHandle]))
    pushArgs(method, state, args)
    method.visitMethodInsn(INVOKEVIRTUAL, getInternalName(classOf[MethodHandle]), "invoke", makeObjectMethodDescriptor(args.length, false), false)
  }

  def emitProcedureCall(method : SimpleMethodVisitor, state : ProgramState, procedure : Expression, args : List[Expression]): Unit = {
    procedure match {
      case VariableReference(id) =>
        if (builtInProcedures.contains(id.uniqueName)) {
          pushBuiltInProcedureArgs(method, state, id.uniqueName, args)
          emitBuiltInProcedureCall(method, id.uniqueName)
        } else if (state.scope.get(id.uniqueName).isDefined) {
          emitVariableReference(method, state, id)
          emitFirstClassProcedureCallToObjectOnStack(method, state, args)
        } else if (state.methods.get(id.uniqueName).isDefined) {
          val m = state.methods.get(id.uniqueName).get
          pushArgs(method, state, args)
          method.emitInvokeStatic(state.mainClass.getName, id.uniqueName, m.descriptor)
        } else {
          throw new CodeGenException("Unknown procedure " + id.name)
        }
      case e =>
        throw new CodeGenException("Not implemented yet: first class procedure call")
    }
  }

  def emitBooleanConversion(method : SimpleMethodVisitor): Unit = {
    val booleanConverterDescriptor = "(" + getDescriptor(classOf[Object]) + ")" + getDescriptor(classOf[Boolean])
    method.emitInvokeStatic(getInternalName(classOf[BuiltIns]), "toBoolean", booleanConverterDescriptor)
  }

  def emitConditional(method : SimpleMethodVisitor, state : ProgramState, cond : Expression, thenBranch : Expression, elseBranch : Expression): Unit = {
    val thenLabel = new Label()
    val exitLabel = new Label()

    pushExpressionResultToStack(method, state, cond)
    emitBooleanConversion(method)
    method.visitJumpInsn(IFNE, thenLabel)

    pushExpressionResultToStack(method, state, elseBranch)
    method.visitJumpInsn(GOTO, exitLabel)

    method.visitLabel(thenLabel)

    pushExpressionResultToStack(method, state, thenBranch)

    method.visitLabel(exitLabel)
  }

  def emitBeginSequence(method : SimpleMethodVisitor, state : ProgramState, exprs : List[Expression]): Unit = {
    val nonTailExprs = exprs.init
    val tailExpr = exprs.last
    for (expr <- nonTailExprs) {
      generateExpression(method, state, expr, keepResultOnStack = false)
    }
    generateExpression(method, state, tailExpr, keepResultOnStack = true)
  }

  def emitBooleanSequence(method : SimpleMethodVisitor, state : ProgramState, exprs : List[Expression], breakValue : Boolean): Unit = {
    if (exprs.isEmpty) {
      loadBoolean(method, !breakValue)
    } else {
      val nonTailExprs = exprs.init
      val tailExpr = exprs.last
      val exitLabel = new Label()
      val comparison = if (breakValue) IFNE else IFEQ

      for (expr <- nonTailExprs) {
        generateExpression(method, state, expr, keepResultOnStack = true)

        method.dup()

        emitBooleanConversion(method)
        method.visitJumpInsn(comparison, exitLabel)
        method.popStack()
      }

      generateExpression(method, state, tailExpr, keepResultOnStack = true)

      method.visitLabel(exitLabel)
    }
  }

  def emitSequenceExpression(method : SimpleMethodVisitor, state : ProgramState, seqType : SequenceType, exprs : List[Expression]): Unit = {
    seqType match {
      case SequenceType.BeginSequence => emitBeginSequence(method, state, exprs)
      case SequenceType.AndSequence => emitBooleanSequence(method, state, exprs, breakValue = false)
      case SequenceType.OrSequence => emitBooleanSequence(method, state, exprs, breakValue = true)
    }
  }

  def emitVariableReference(method : SimpleMethodVisitor, state : ProgramState, id : Identifier): Unit = {
    state.scope.get(id.uniqueName) match {
      case None =>
        if (builtInProcedures.contains(id.uniqueName)) {
          val builtInName = convertBuiltInProcedureName(id.uniqueName)
          method.emitGetStatic(getInternalName(classOf[BuiltInProcObjects]), builtInName, getDescriptor(classOf[MethodHandle]))
        } else {
          throw new CodeGenException("Unknown variable " + id.name)
        }
      case Some(v) =>
        v match {
          case StaticField(n) =>
            method.emitGetStatic(state.mainClass.getName, n, getDescriptor(classOf[Object]))
          case LocalVariable(n, i) =>
            method.visitVarInsn(ALOAD, i)
          case _ =>
            throw new CodeGenException("Unknown field type " + v)
        }
    }
  }

  def emitVariableAssignment(method : SimpleMethodVisitor, state : ProgramState, id : Identifier, expression: Expression): Unit = {
    state.scope.get(id.uniqueName) match {
      case None =>
        throw new CodeGenException("Unknown variable " + id.name)
      case Some(v) =>
        pushExpressionResultToStack(method, state, expression)

        v match {
          case StaticField(n) =>
            method.emitPutStatic(state.mainClass.getName, n, getDescriptor(classOf[Object]))
          case LocalVariable(n, i) =>
            method.visitVarInsn(ASTORE, i)
          case _ =>
            throw new CodeGenException("Unknown field type " + v)
        }

        loadUndefined(method)
    }
  }

  def generateExpression(method : SimpleMethodVisitor, state : ProgramState, expression : Expression, keepResultOnStack : Boolean): Unit = {
    expression match {
      case ProcedureCall(proc, args, tc) => // TODO: Not handling tail calls yet
        emitProcedureCall(method, state, proc, args)
      case ValueExpression(lit) =>
        loadLiteral(method, lit)
      case Conditional(cond, thenBranch, elseBranch) =>
        emitConditional(method, state, cond, thenBranch, elseBranch)
      case SequenceExpression(sequenceType, exprs) =>
        emitSequenceExpression(method, state, sequenceType, exprs)
      case Assignment(id, expr) =>
        emitVariableAssignment(method, state, id, expr)
      case VariableReference(id) =>
        emitVariableReference(method, state, id)
      case Closure(id) =>
        throw new CodeGenException("Expression type not implemented yet: closure")
      case UndefinedValue() =>
        loadUndefined(method)
    }

    if (!keepResultOnStack)
      method.popStack()
  }

  def pushExpressionResultToStack(method : SimpleMethodVisitor, state : ProgramState, expression : Expression): Unit = {
    generateExpression(method, state, expression, true)
  }

  def generateTopLevelExpression(method : SimpleMethodVisitor, state : ProgramState, expression: Expression): Unit = {
    generateExpression(method, state, expression, false)
  }

  def generateProcedureBody(method : SimpleMethodVisitor, state : ProgramState, expressions : List[Expression], isMainMethod : Boolean): Unit = {
    method.visitCode()

    if (isMainMethod) {
      for (expr <- expressions) {
        generateTopLevelExpression(method, state, expr)
      }

      method.emitReturn()
    } else {
      for (expr <- expressions.init) {
        generateTopLevelExpression(method, state, expr)
      }

      pushExpressionResultToStack(method, state, expressions.last)
      method.emitObjectReturn()
    }

    method.setMaxs()
    method.visitEnd()
  }

  def introduceVariables(program : ProgramSyntaxTree, mainClass : DebugClassWriter): Map[String, Variable] = {
    def introduceVariable(v : VariableDeclaration) = {
      mainClass.visitField(ACC_PRIVATE | ACC_STATIC, v.id.uniqueName, getDescriptor(classOf[Object]), null, null).visitEnd()
      (v.id.uniqueName, StaticField(v.id.uniqueName))
    }

    // TODO: variables for procedures that are set!
    program.variableDeclarations.map(introduceVariable).toMap
  }

  def generateTopLevelProcedures(program : ProgramSyntaxTree, state : ProgramState, mainClass : DebugClassWriter) = {
    def generateProcedure(p : ProcedureDefinition) = {
      val descriptor = p.closure.formals match {
        case SingleArgFormals(id) => makeObjectMethodDescriptor(1, isArray = false)
        case MultiArgFormals(ids) => makeObjectMethodDescriptor(ids.length, isArray = false)
      }
      val m = new SimpleMethodVisitor(mainClass, ACC_PRIVATE + ACC_STATIC, p.id.uniqueName, descriptor)
      (p.id.uniqueName, m)
    }

    def procedureBody(p : ProcedureDefinition, state : ProgramState) = {
      state.methods.get(p.id.uniqueName) match {
        case None =>
          throw new CodeGenException("Internal error: could not find procedure " + p.id.uniqueName)
        case Some(mv) =>
          // TODO: local procedures?
          val argIds = p.closure.formals match {
            case SingleArgFormals(id) => List(id.uniqueName)
            case MultiArgFormals(ids) => ids.map((id) => id.uniqueName)
          }
          val localIds = p.closure.variableDeclarations.map((v) => v.id.uniqueName)
          val locals = (argIds ::: localIds).zipWithIndex.map((i) => (i._1, LocalVariable(i._1, i._2)))

          val newScope = locals.foldLeft(state.scope)((scope : Map[String, Variable], v) => scope.updated(v._1, v._2))
          val newState = state.copy(scope = newScope)

          generateProcedureBody(mv, newState, p.closure.body, isMainMethod = false)
      }
    }

    val methods = program.procedureDefinitions.map(generateProcedure).toMap
    val newState = state.copy(methods = methods)

    for (pd <- program.procedureDefinitions) {
      procedureBody(pd, newState)
    }

    newState
  }

  def generateCodeFor(program : ProgramSyntaxTree) : Unit = {
    val mainClass = declareClass(program.programName, getInternalName(classOf[Object]))
    val scope = introduceVariables(program, mainClass)
    val state = ProgramState(mainClass, scope, Map.empty)

    val newState = generateTopLevelProcedures(program, state, mainClass)

    val mainMethodDescriptor = "([" + getDescriptor(classOf[String]) + ")" + Type.VOID_TYPE.getDescriptor
    val mainMethod = new SimpleMethodVisitor(mainClass, ACC_PUBLIC + ACC_STATIC, "main", mainMethodDescriptor)

    generateProcedureBody(mainMethod, newState, program.expressions, isMainMethod = true)

    mainClass.visitEnd()
    mainClass.writeToDisk()
  }

}
