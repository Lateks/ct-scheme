package backend

import java.io.PrintWriter

import backend.ast.SequenceType.SequenceType
import backend.ast._
import backend.codegen.{CodeGenException, DebugClassWriter, SimpleMethodVisitor}
import lib._
import org.objectweb.asm.{Label, Type, Handle}
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

  def pushArrayArgs(method : SimpleMethodVisitor, state : ProgramState, args : List[Expression]): Unit = {
    createArray(method, args, (elem : Expression) => pushExpressionResultToStack(method, state, elem))
  }

  def pushBuiltInProcedureArgs(method : SimpleMethodVisitor, state : ProgramState, procedureName : String, args : List[Expression]): Unit = {
    if (builtInProceduresTakingArrayParam.contains(procedureName)) {
      pushArrayArgs(method, state, args)
    } else {
      pushArgs(method, state, args)
    }
  }

  def emitFirstClassProcedureCallToObjectOnStack(method : SimpleMethodVisitor, state : ProgramState, args : List[Expression]): Unit = {
    method.visitTypeInsn(CHECKCAST, getInternalName(classOf[CTProcedure]))
    pushArrayArgs(method, state, args)
    method.visitMethodInsn(INVOKEINTERFACE, getInternalName(classOf[CTProcedure]), "apply", makeObjectMethodDescriptor(1, isArray = true), onInterface = true)
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
        pushExpressionResultToStack(method, state, e)
        emitFirstClassProcedureCallToObjectOnStack(method, state, args)
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
          method.emitGetStatic(getInternalName(classOf[BuiltInProcObjects]), builtInName, getDescriptor(classOf[CTProcedure]))
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
    generateExpression(method, state, expression, keepResultOnStack = true)
  }

  def generateTopLevelExpression(method : SimpleMethodVisitor, state : ProgramState, expression: Expression): Unit = {
    generateExpression(method, state, expression, keepResultOnStack = false)
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
    def introduceVariable(name : String) = {
      mainClass.visitField(ACC_PRIVATE | ACC_STATIC, name, getDescriptor(classOf[Object]), null, null).visitEnd()
      (name, StaticField(name))
    }

    val topLevelVariables = program.variableDeclarations.map((v) => introduceVariable(v.id.uniqueName))
    val procedureObjectVariables = program.procedureDefinitions
      .filter((p) => p.closure.isUsedAsFirstClassValue || p.closure.isReassigned)
      .map((p) => introduceVariable(p.id.uniqueName))

    (topLevelVariables ::: procedureObjectVariables).toMap
  }

  def generateProcedures(procedureDefinitions : List[ProcedureDefinition], state : ProgramState): ProgramState = {
    def generateProcedure(p : ProcedureDefinition) = {
      val descriptor = buildMethodDescriptor(p.closure)
      val m = new SimpleMethodVisitor(state.mainClass, ACC_PRIVATE + ACC_STATIC, p.id.uniqueName, descriptor)
      (p.id.uniqueName, m)
    }

    def procedureBody(p : ProcedureDefinition, state : ProgramState) = {
      state.methods.get(p.id.uniqueName) match {
        case None =>
          throw new CodeGenException("Internal error: could not find procedure " + p.id.uniqueName)
        case Some(mv) =>
          println("Generating code for method " + p.id.uniqueName + ", scope has the following methods: " + state.methods)
          val stateWithNestedMethods = generateProcedures(p.closure.procedureDefinitions, state)

          val argIds = p.closure.formals match {
            case SingleArgFormals(id) => List(id.uniqueName)
            case MultiArgFormals(ids) => ids.map((id) => id.uniqueName)
          }
          val localIds = p.closure.variableDeclarations.map((v) => v.id.uniqueName)
          val localsAndArgs = (argIds ::: localIds).zipWithIndex.map((i) => (i._1, LocalVariable(i._1, i._2)))

          val newScope = localsAndArgs.foldLeft(stateWithNestedMethods.scope)((scope : Map[String, Variable], v) => scope.updated(v._1, v._2))
          val procedureBodyState = stateWithNestedMethods.copy(scope = newScope)

          generateProcedureBody(mv, procedureBodyState, p.closure.body, isMainMethod = false)
      }
    }

    // TODO: add arguments for captured variables
    val methods = procedureDefinitions.map(generateProcedure).toMap
    val updatedMethods = state.methods ++ methods
    val newState = state.copy(methods = updatedMethods)

    for (pd <- procedureDefinitions) {
      procedureBody(pd, newState)
    }

    newState
  }

  def generateTopLevelProcedures(program : ProgramSyntaxTree, state : ProgramState) = {
    generateProcedures(program.procedureDefinitions, state)
  }

  def buildMethodDescriptor(closure : ClosureDefinition): String = {
    val sb = new StringBuilder("(")
    val objectDescriptor = getDescriptor(classOf[Object])
    closure.formals match {
      case SingleArgFormals(id) =>
        sb.append(objectDescriptor)
      case MultiArgFormals(ids) =>
        for (id <- ids) {
          sb.append(objectDescriptor)
        }
    }

    sb.append(")").append(objectDescriptor).toString
  }

  def buildProcedureObjectDescriptor(closure : ClosureDefinition): String = {
    closure.formals match {
      case SingleArgFormals(id) =>
        getDescriptor(classOf[CTProcedure1])
      case MultiArgFormals(ids) =>
        ids.length match {
          case 0 =>
            getDescriptor(classOf[CTProcedure0])
          case 1 =>
            getDescriptor(classOf[CTProcedure1])
          case 2 =>
            getDescriptor(classOf[CTProcedure2])
          case 3 =>
            getDescriptor(classOf[CTProcedure3])
          case 4 =>
            getDescriptor(classOf[CTProcedure4])
          case 5 =>
            getDescriptor(classOf[CTProcedure5])
          case _ =>
            throw new CodeGenException("Internal error: procedures with more than 5 parameters are not supported")
        }
    }
  }

  def lambdaMetafactoryHandle() = {
    new Handle(H_INVOKESTATIC,
      "java/lang/invoke/LambdaMetafactory",
      "metafactory",
      "(Ljava/lang/invoke/MethodHandles$Lookup;Ljava/lang/String;Ljava/lang/invoke/MethodType;Ljava/lang/invoke/MethodType;Ljava/lang/invoke/MethodHandle;Ljava/lang/invoke/MethodType;)Ljava/lang/invoke/CallSite;",
      false)
  }

  def loadProcedureObject(method : SimpleMethodVisitor, state : ProgramState, methodName : String, closure : ClosureDefinition): Unit = {
    val lambdaMetaFactoryHandle = lambdaMetafactoryHandle()

    val methodOnInterface = false
    val methodDescriptor = buildMethodDescriptor(closure)
    val methodHandle = new Handle(H_INVOKESTATIC, state.mainClass.getName, methodName, methodDescriptor, methodOnInterface)
    val procedureObjectDescriptor = buildProcedureObjectDescriptor(closure)
    val applyMethodDescriptor = "()" + procedureObjectDescriptor // No captures, so parentheses are empty

    println("Loading procedure object for method " + methodName)
    println("Method descriptor is: " + methodDescriptor)
    println("Procedure object descriptor is: " + procedureObjectDescriptor)
    println("Method type: " + Type.getType(methodDescriptor))
    println("Apply method descriptor: " + applyMethodDescriptor)
    println("Method handle: " + methodHandle)

    method.visitInvokeDynamicInsn("apply",
      applyMethodDescriptor,
      lambdaMetaFactoryHandle,
      Type.getType(methodDescriptor),
      methodHandle,
      Type.getType(methodDescriptor))
  }

  def attachArgumentHandler(method : SimpleMethodVisitor, methodName : String, closure : ClosureDefinition): Unit = {
    val lambdaMetaFactoryHandle = lambdaMetafactoryHandle()
    val procedureObjectDescriptor = getDescriptor(classOf[CTProcedure])
    val arrayMethodDescriptor = "(" + getDescriptor(classOf[Array[Object]]) + ")" + getDescriptor(classOf[Object])

    closure.formals match {
      case SingleArgFormals(id) =>
        val applyMethodTypeDescriptor = "(" + getDescriptor(classOf[CTProcedure1]) + ")" + procedureObjectDescriptor
        val matcherDescriptor = "(" + getDescriptor(classOf[CTProcedure1]) + getDescriptor(classOf[Array[Object]]) + ")" + getDescriptor(classOf[Object])
        val matcherHandle = new Handle(H_INVOKESTATIC, getInternalName(classOf[ProcedureHelpers]), "matchVarargs", matcherDescriptor, false)
        method.visitInvokeDynamicInsn("apply",
          applyMethodTypeDescriptor,
          lambdaMetaFactoryHandle,
          Type.getType(arrayMethodDescriptor),
          matcherHandle,
          Type.getType(arrayMethodDescriptor)
        )
      case MultiArgFormals(ids) =>
        method.visitLdcInsn(methodName)
        val wrappedObjectDescriptor = buildProcedureObjectDescriptor(closure)
        val applyMethodTypeDescriptor = "(" + wrappedObjectDescriptor + getDescriptor(classOf[String]) + ")" + procedureObjectDescriptor
        val matcherDescriptor = "(" + wrappedObjectDescriptor + getDescriptor(classOf[String]) + getDescriptor(classOf[Array[Object]]) + ")" + getDescriptor(classOf[Object])
        val matchMethodName = "match" + (closure.formals match {
          case SingleArgFormals(id) => 1
          case MultiArgFormals(idList) => idList.length
        })
        val matcherHandle = new Handle(H_INVOKESTATIC, getInternalName(classOf[ProcedureHelpers]), matchMethodName, matcherDescriptor, false)
        method.visitInvokeDynamicInsn("apply",
          applyMethodTypeDescriptor,
          lambdaMetaFactoryHandle,
          Type.getType(arrayMethodDescriptor),
          matcherHandle,
          Type.getType(arrayMethodDescriptor)
        )
    }
  }

  def makeInitializer(program : ProgramSyntaxTree, state : ProgramState) = {
    println("Generating initializer")

    val initializer = new SimpleMethodVisitor(state.mainClass, ACC_STATIC, "<clinit>", "()V")
    initializer.visitCode()

    val firstClassProcs = program.procedureDefinitions
      .filter((p) => p.closure.isUsedAsFirstClassValue || p.closure.isReassigned)

    for (p <- firstClassProcs) {
      println("Found first class procedure " + p + ", storing it in a static field")
      state.scope.get(p.id.uniqueName) match {
        case None => throw new CodeGenException("Internal error: field " + p.id.uniqueName + " has not been declared.")
        case Some(v) =>
          v match {
            case StaticField(n) =>
              loadProcedureObject(initializer, state, p.id.uniqueName, p.closure)
              attachArgumentHandler(initializer, p.id.name, p.closure)
              initializer.emitPutStatic(state.mainClass.getName, n, getDescriptor(classOf[Object]))
            case e =>
              throw new CodeGenException("Internal error: Unexpected field type for " + p.id.uniqueName + ": " + e)
          }
      }
    }

    initializer.emitReturn()
    initializer.visitMaxs(0, 0)
    initializer.visitEnd()
    println("Finished generating initializer")
  }

  def addInnerClassReferences(mainClass : DebugClassWriter): Unit = {
    mainClass.visitInnerClass("java/lang/invoke/MethodHandles$Lookup", "java/lang/invoke/MethodHandles", "Lookup", ACC_PUBLIC + ACC_FINAL + ACC_STATIC);
  }

  def generateCodeFor(program : ProgramSyntaxTree) : Unit = {
    println("Generating code")

    val mainClass = declareClass(program.programName, getInternalName(classOf[Object]))
    val scope = introduceVariables(program, mainClass)
    val state = ProgramState(mainClass, scope, Map.empty)

    val newState = generateTopLevelProcedures(program, state)

    val mainMethodDescriptor = "([" + getDescriptor(classOf[String]) + ")" + Type.VOID_TYPE.getDescriptor
    val mainMethod = new SimpleMethodVisitor(mainClass, ACC_PUBLIC + ACC_STATIC, "main", mainMethodDescriptor)

    println("Adding class references")
    addInnerClassReferences(mainClass)

    generateProcedureBody(mainMethod, newState, program.expressions, isMainMethod = true)

    println("Calling initializer generator")
    makeInitializer(program, newState)

    mainClass.visitEnd()
    mainClass.writeToDisk()
  }

}
