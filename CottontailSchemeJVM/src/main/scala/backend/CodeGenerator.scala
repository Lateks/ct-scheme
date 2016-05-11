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
  case class LocalReferenceVariable(name : String, index : Int) extends Variable

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

  def emitListConversion(method : SimpleMethodVisitor): Unit = {
    method.emitInvokeStatic(getInternalName(classOf[BuiltIns]), "toList", "([" + getDescriptor(classOf[Object]) + ")" + getDescriptor(classOf[Object]))
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
        emitListConversion(method)
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
    pushArrayArgs(method, state, args)
    method.visitMethodInsn(INVOKESTATIC,
      getInternalName(classOf[ProcedureHelpers]),
      "callProcedure",
      "(" + getDescriptor(classOf[Object]) + getDescriptor(classOf[Array[Object]]) + ")" + getDescriptor(classOf[Object]),
      onInterface = false)
  }

  def emitProcedureCall(method : SimpleMethodVisitor, state : ProgramState, procedure : Expression, args : List[Expression], tailCall : Boolean): Unit = {
    procedure match {
      case VariableReference(id) =>
        if (builtInProcedures.contains(id.uniqueName)) {
          // No tail call handling for built-in procedures
          // because they never result in further tail calls.
          pushBuiltInProcedureArgs(method, state, id.uniqueName, args)
          emitBuiltInProcedureCall(method, id.uniqueName)
        } else if (state.scope.get(id.uniqueName).isDefined) {
          // TODO: if in tail position, build CTTailContinuation value:
          // - create CTTailContinuation object
          // - create CTProcedure0 object, binding args
          // - initialize CTTailContinuation object
          // TODO: if not in tail position, add call to ProcedureHelpers.trampoline
          emitVariableReference(method, state, id)
          emitFirstClassProcedureCallToObjectOnStack(method, state, args)
        } else if (state.methods.get(id.uniqueName).isDefined) {
          // TODO: the same tail call handling as above
          // Note: this call should have had its arguments checked
          // during semantic analysis, so no need to attach a matcher
          // to the procedure object.
          // However, if the called procedure is variable arity,
          // the argument list should be built here while binding
          // args to make the CTProcedure0 object.
          val m = state.methods.get(id.uniqueName).get
          if (m.isVarargs) {
            pushArrayArgs(method, state, args)
            emitListConversion(method)
          } else {
            pushArgs(method, state, args)
          }
          method.emitInvokeStatic(state.mainClass.getName, id.uniqueName, m.descriptor)
        } else {
          throw new CodeGenException("Unknown procedure " + id.name)
        }
      case e =>
        // TODO: the same tail call handling as above
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

  def emitVariableReference(method : SimpleMethodVisitor, state : ProgramState, id : Identifier, rawReferences : Boolean): Unit = {
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
          case LocalReferenceVariable(n, i) =>
            method.visitVarInsn(ALOAD, i)
            if (!rawReferences) {
              method.visitTypeInsn(CHECKCAST, getInternalName(classOf[CTReferenceCell]))
              method.visitMethodInsn(INVOKEVIRTUAL, getInternalName(classOf[CTReferenceCell]), "get", makeObjectMethodDescriptor(0, isArray = false), onInterface = false)
            }
          case _ =>
            throw new CodeGenException("Unknown field type " + v)
        }
    }
  }

  def emitVariableReference(method : SimpleMethodVisitor, state : ProgramState, id : Identifier): Unit = {
    emitVariableReference(method, state, id, rawReferences = false)
  }

  def emitVariableAssignment(method : SimpleMethodVisitor, state : ProgramState, id : Identifier, loadValue : (SimpleMethodVisitor, ProgramState) => Unit): Unit = {
    state.scope.get(id.uniqueName) match {
      case None =>
        throw new CodeGenException("Trying to set unknown variable " + id.name)
      case Some(v) =>
        v match {
          case StaticField(n) =>
            loadValue(method, state)
            method.emitPutStatic(state.mainClass.getName, n, getDescriptor(classOf[Object]))
          case LocalVariable(n, i) =>
            loadValue(method, state)
            method.visitVarInsn(ASTORE, i)
          case LocalReferenceVariable(n, i) =>
            method.visitVarInsn(ALOAD, i)
            method.visitTypeInsn(CHECKCAST, getInternalName(classOf[CTReferenceCell]))
            loadValue(method, state)
            method.visitMethodInsn(INVOKEVIRTUAL, getInternalName(classOf[CTReferenceCell]), "set", "(" + getDescriptor(classOf[Object]) + ")V", onInterface = false)
          case _ =>
            throw new CodeGenException("Unknown field type " + v)
        }
    }
  }

  def emitVariableAssignment(method : SimpleMethodVisitor, state : ProgramState, id : Identifier, expression: Expression): Unit = {
    val loader = (m : SimpleMethodVisitor, s : ProgramState) => pushExpressionResultToStack(m, s, expression)
    emitVariableAssignment(method, state, id, loader)
    loadUndefined(method)
  }

  def generateExpression(method : SimpleMethodVisitor, state : ProgramState, expression : Expression, keepResultOnStack : Boolean): Unit = {
    expression match {
      case ProcedureCall(proc, args, isTailCall) =>
        emitProcedureCall(method, state, proc, args, isTailCall)
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
      case Closure(c) =>
        loadFirstClassProcedure(method, state, c)
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

  def generateProcedureBody(method : SimpleMethodVisitor, state : ProgramState, expressions : List[Expression], isMainMethod : Boolean, emitPreamble : Option[SimpleMethodVisitor => Unit]): Unit = {
    method.visitCode()

    emitPreamble match {
      case None => ()
      case Some(f) => f(method)
    }

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

  def isUsedAsFirstClassProcedure(procedureDefinition : ProcedureDefinition): Boolean = {
    procedureDefinition.closure.isReassigned || procedureDefinition.closure.isUsedAsFirstClassValue
  }

  def capturesVariables(procedureDefinition: ProcedureDefinition): Boolean = {
    procedureDefinition.closure.environment.nonEmpty
  }

  def loadEmptyReferenceCell(method: SimpleMethodVisitor): Unit = {
    val classTypeName = getInternalName(classOf[CTReferenceCell])
    method.createAndDupObject(classTypeName)
    method.invokeConstructor(classTypeName)
  }

  def loadReferenceCellWithLocalVariableValue(method : SimpleMethodVisitor, localVarIndex : Int): Unit = {
    val classTypeName = getInternalName(classOf[CTReferenceCell])
    method.createAndDupObject(classTypeName)
    method.visitVarInsn(ALOAD, localVarIndex)
    method.invokeConstructor(classTypeName, getDescriptor(classOf[Object]))
  }

  def findClosuresInScope(expressions : List[Expression]): List[ClosureDefinition] = {
    def findClosuresInExpression(expr : Expression): List[ClosureDefinition] = {
      expr match {
        case ProcedureCall(proc, args, _) =>
          findClosuresInExpression(proc) ::: findClosuresInScope(args)
        case Conditional(cond, thenBranch, elseBranch) =>
          findClosuresInExpression(cond) ::: findClosuresInExpression(thenBranch) ::: findClosuresInExpression(elseBranch)
        case SequenceExpression(_, exprs) =>
          findClosuresInScope(exprs)
        case Assignment(_, e) =>
          findClosuresInExpression(e)
        case Closure(c) =>
          List(c)
        case UndefinedValue() | ValueExpression(_) | VariableReference(_) =>
          List()
      }
    }

    expressions.flatMap(findClosuresInExpression)
  }

  def generateProcedures(procedures : List[ClosureDefinition], state : ProgramState): ProgramState = {
    def generateProcedure(c : ClosureDefinition) = {
      val descriptor = buildMethodDescriptor(c)
      val isVarargs = c.formals match {
        case SingleArgFormals(_) => true
        case MultiArgFormals(_) => false
      }
      val m = new SimpleMethodVisitor(state.mainClass, ACC_PRIVATE + ACC_STATIC, c.functionName.uniqueName, descriptor, isVarargs)
      (c.functionName.uniqueName, m)
    }

    def makeLocalVariable(name : String, index : Int, storedAsReference : List[String]): Variable = {
      if (storedAsReference.contains(name)) {
        LocalReferenceVariable(name, index)
      } else {
        LocalVariable(name, index)
      }
    }

    def procedureBody(c : ClosureDefinition, state : ProgramState) = {
      val procName = c.functionName.uniqueName
      state.methods.get(procName) match {
        case None =>
          throw new CodeGenException("Internal error: could not find procedure " + procName)
        case Some(mv) =>
          println("Generating code for method " + procName + ", scope has the following methods: " + state.methods)
          val anonymousProcedures = findClosuresInScope(c.body)
          val nestedProcedures = anonymousProcedures ::: c.procedureDefinitions.map((p) => p.closure)
          val stateWithNestedMethods = generateProcedures(nestedProcedures, state)
          val firstClassAndCapturingProcedures = c.procedureDefinitions
            .filter((p) => isUsedAsFirstClassProcedure(p) || capturesVariables(p))

          // Find escaping variables (captured by any first class procedure in scope)
          val escapingVariables = (anonymousProcedures ::: firstClassAndCapturingProcedures.map((p) => p.closure))
            .flatMap((c) => c.environment)
            .distinct
          val escapingVariableNames = escapingVariables.map((id) => id.uniqueName)
          val capturedProcedures = c.procedureDefinitions.filter((p) => escapingVariableNames.contains(p.id.uniqueName))

          val localProcedureObjects = (firstClassAndCapturingProcedures ::: capturedProcedures).distinct

          // Find all ids we need to introduce local variables for
          val captureIds = c.environment.map((capture) => capture.uniqueName)
          val argIds = c.formals match {
            case SingleArgFormals(id) => List(id.uniqueName)
            case MultiArgFormals(ids) => ids.map((id) => id.uniqueName)
          }
          val localIds = c.variableDeclarations.map((v) => v.id.uniqueName)
          val localProcIds = localProcedureObjects.map((p) => p.id.uniqueName)
          val localVariableIds = captureIds ::: argIds ::: localIds ::: localProcIds

          // Variables captured from surrounding scopes as well
          // as variables escaping from this scope are represented
          // as reference cells.
          val localReferenceVariables = (escapingVariableNames ::: captureIds).distinct
          val localsAndArgs = localVariableIds
            .zipWithIndex
            .map((i) => (i._1, makeLocalVariable(i._1, i._2, localReferenceVariables)))

          val newScope = localsAndArgs.foldLeft(stateWithNestedMethods.scope)((scope : Map[String, Variable], v) => scope.updated(v._1, v._2))
          val procedureBodyState = stateWithNestedMethods.copy(scope = newScope)

          def emitPreamble (methodVisitor : SimpleMethodVisitor) = {
            val newCaptures = escapingVariables
              .filter((v) => !captureIds.contains(v.uniqueName))

            println("Setting up escaping variables for method " + c.functionName.uniqueName + ", scope is " + procedureBodyState.scope)

            for (capture <- newCaptures) {
              println("Setting up escaping variable " + capture)

              val localVarIndex = procedureBodyState.scope.get(capture.uniqueName) match {
                case Some(LocalReferenceVariable(_, i)) => i
                case None =>
                  throw new CodeGenException("Internal error: could not find local variable " + capture.uniqueName)
                case Some(v) =>
                  throw new CodeGenException("Internal error: unexpected variable type for variable " + capture.uniqueName + ": " + v)
              }

              if (argIds.contains(capture.uniqueName)) {
                loadReferenceCellWithLocalVariableValue(methodVisitor, localVarIndex)
              } else {
                loadEmptyReferenceCell(methodVisitor)
              }

              methodVisitor.visitVarInsn(ASTORE, localVarIndex)
            }

            for (p <- localProcedureObjects) {
              val loader = (m : SimpleMethodVisitor, s : ProgramState) => loadFirstClassProcedure(m, s, p.closure)
              emitVariableAssignment(methodVisitor, procedureBodyState, p.id, loader)
            }
          }

          generateProcedureBody(mv, procedureBodyState, c.body, isMainMethod = false, Some(emitPreamble))
      }
    }

    val methods = procedures.map(generateProcedure).toMap
    val updatedMethods = state.methods ++ methods
    val newState = state.copy(methods = updatedMethods)

    for (c <- procedures) {
      procedureBody(c, newState)
    }

    newState
  }

  def generateTopLevelProcedures(program : ProgramSyntaxTree, state : ProgramState) = {
    val closuresInTopLevelScope = findClosuresInScope(program.expressions)
    val namedProcedures = program.procedureDefinitions.map((p) => p.closure)
    generateProcedures(namedProcedures ::: closuresInTopLevelScope, state)
  }

  def buildMethodDescriptor(closure : ClosureDefinition, withCaptures : Boolean): String = {
    val sb = new StringBuilder("(")
    val objectDescriptor = getDescriptor(classOf[Object])

    if (withCaptures) {
      for (capture <- closure.environment) {
        println("Building method descriptor for closure " + closure.functionName.uniqueName + ", adding parameter for capture " + capture)
        sb.append(objectDescriptor)
      }
    }

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

  def buildMethodDescriptor(closure : ClosureDefinition): String = {
    buildMethodDescriptor(closure, withCaptures = true)
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
    val implementedMethodDescriptor = buildMethodDescriptor(closure, withCaptures = false)

    val objectDescriptor = getDescriptor(classOf[Object])
    val captureDescriptors = closure.environment.map((_) => objectDescriptor).mkString("")
    val applyMethodDescriptor = "(" + captureDescriptors + ")" + procedureObjectDescriptor

    println("\n*****")
    println("Loading procedure object for method " + methodName)
    println("Method descriptor is: " + methodDescriptor)
    println("Procedure object descriptor is: " + procedureObjectDescriptor)
    println("Implemented method descriptor: " + implementedMethodDescriptor)
    println("Implemented method type: " + Type.getType(implementedMethodDescriptor))
    println("Apply method descriptor: " + applyMethodDescriptor)
    println("Method handle: " + methodHandle)
    println("*****\n")

    for (capture <- closure.environment) {
      emitVariableReference(method, state, capture, rawReferences = true)
    }

    method.visitInvokeDynamicInsn("apply",
      applyMethodDescriptor,
      lambdaMetaFactoryHandle,
      Type.getType(implementedMethodDescriptor),
      methodHandle,
      Type.getType(implementedMethodDescriptor))
  }

  def attachArgumentHandler(method : SimpleMethodVisitor, methodName : String, closure : ClosureDefinition): Unit = {
    val ctProcDescriptor = getDescriptor(classOf[CTProcedure])
    val (matchMethodName, matchMethodType) = closure.formals match {
      case SingleArgFormals(_) =>
        ("matchVarargs", "(" + getDescriptor(classOf[CTProcedure1]) + ")" + ctProcDescriptor)
      case MultiArgFormals(ids) =>
        val procedureObjectDescriptor = buildProcedureObjectDescriptor(closure)
        ("match" + ids.length, "(" +procedureObjectDescriptor + getDescriptor(classOf[String]) + ")" + ctProcDescriptor)
    }
    closure.formals match {
      case MultiArgFormals(_) =>
        method.visitLdcInsn(methodName)
      case _ => ()
    }
    method.emitInvokeStatic(getInternalName(classOf[ProcedureHelpers]), matchMethodName, matchMethodType)
  }

  def loadFirstClassProcedure(method : SimpleMethodVisitor, state : ProgramState, c : ClosureDefinition): Unit = {
    loadProcedureObject(method, state, c.functionName.uniqueName, c)
    attachArgumentHandler(method, c.functionName.name, c)
  }

  def makeInitializer(program : ProgramSyntaxTree, state : ProgramState) = {
    println("Generating initializer")

    val initializer = new SimpleMethodVisitor(state.mainClass, ACC_STATIC, "<clinit>", "()V", false)
    initializer.visitCode()

    val firstClassProcs = program.procedureDefinitions.filter(isUsedAsFirstClassProcedure)

    for (p <- firstClassProcs) {
      println("Found first class procedure " + p + ", storing it in a static field")
      state.scope.get(p.id.uniqueName) match {
        case None => throw new CodeGenException("Internal error: field " + p.id.uniqueName + " has not been declared.")
        case Some(v) =>
          v match {
            case StaticField(n) =>
              loadFirstClassProcedure(initializer, state, p.closure)
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
    val mainMethod = new SimpleMethodVisitor(mainClass, ACC_PUBLIC + ACC_STATIC, "main", mainMethodDescriptor, false)

    println("Adding class references")
    addInnerClassReferences(mainClass)

    generateProcedureBody(mainMethod, newState, program.expressions, isMainMethod = true, None)

    println("Calling initializer generator")
    makeInitializer(program, newState)

    mainClass.visitEnd()
    mainClass.writeToDisk()
  }

}
