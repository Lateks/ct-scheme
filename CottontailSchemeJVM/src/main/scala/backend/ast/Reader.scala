package backend.ast

import org.json4s.JsonDSL._
import org.json4s.native.Serialization.write
import org.json4s.native.JsonMethods._
import org.json4s._
import org.json4s.native.Serialization

abstract class ReadResult
case class ReadSuccess(program : ProgramSyntaxTree) extends ReadResult
case class ReadError(message : String) extends ReadResult
case class ReadFailure(message : String) extends ReadResult

object Reader {
  implicit val formats =
    Serialization.formats(NoTypeHints) +
    ProcedureDefinitionSerializer +
    VariableDeclarationSerializer +
    ClosureFormalsSerializer +
    IdentifierSerializer +
    ExpressionSerializer +
    LiteralValueSerializer

  object IdentifierSerializer extends CustomSerializer[Identifier](format => (
    {
      case x: JObject =>
        val name = (x \ "name").extract[String]
        val uniqueName = (x \ "uniqueName").extract[String]
        val argIndex : Option[Int] = x \ "argIndex" match {
          case JNull => None
          case JNothing => None
          case JObject(JField("Case", JString("Some")) :: JField("Fields", JArray(JInt(i) :: Nil)) :: Nil) =>
            Some(i.intValue())
          case a => throw new MappingException("Expected argIndex to be null or an integer value but was " + pretty(render(a)))
        }
        Identifier(name, uniqueName, argIndex)
    },
    {
      case x: Identifier => write(x)
    }
    ))

  object ClosureFormalsSerializer extends CustomSerializer[ClosureFormals](format => (
    {
      case JObject(JField("Case", JString(caseName)) :: JField("Fields", JArray(fst :: Nil)) :: Nil) =>
        caseName match {
          case "SingleArgFormals" =>
            val id = fst.extract[Identifier]
            SingleArgFormals(id)
          case "MultiArgFormals" =>
            val ids = fst.extract[List[Identifier]]
            MultiArgFormals(ids)
          case s =>
            throw new MappingException("Invalid closure formals type: " + s)
        }
      case x =>
        throw new MappingException("Cannot convert object " + pretty(render(x)) + " to closure formals.")
    },
    {
      case x: ClosureFormals => write(x)
    }
    ))

  object VariableDeclarationSerializer extends CustomSerializer[VariableDeclaration](format => (
    {
      case JObject(JField("Case", JString(s)) :: JField("Fields", JArray(id :: Nil)) :: Nil) =>
        s match {
          case "VariableDeclaration" =>
            val identifier = id.extract[Identifier]
            VariableDeclaration(identifier)
          case x =>
            throw new MappingException("Expected a VariableDeclaration but got  " + x)
        }
      case x =>
        throw new MappingException("Cannot convert object " + pretty(render(x)) + " to a variable declaration.")
    },
    {
      case x: VariableDeclaration => write(x)
    }
    ))

  object ProcedureDefinitionSerializer extends CustomSerializer[ProcedureDefinition](format => (
    {
      case JObject(JField("Case", JString(s)) :: JField("Fields", JArray(id :: proc :: Nil)) :: Nil) =>
        s match {
          case "ProcedureDefinition" =>
            val identifier = id.extract[Identifier]
            val procedure = proc.extract[ClosureDefinition]
            ProcedureDefinition(identifier, procedure)
          case x =>
            throw new MappingException("Expected a ProcedureDefinition but got " + x)
        }
      case x =>
        throw new MappingException("Cannot convert object " + pretty(render(x)) + " to a procedure definition.")
    },
    {
      case x: ProcedureDefinition => write(x)
    }
    ))

  object LiteralValueSerializer extends CustomSerializer[LiteralValue](format => (
    {
      case JObject(JField("Case", JString(s)) :: JField("Fields", JArray(lit :: Nil)) :: Nil) =>
        s match {
          case "Boolean" =>
            LiteralBoolean(lit.extract[Boolean])
          case "String" =>
            LiteralString(lit.extract[String])
          case "Number" =>
            LiteralNumber(lit.extract[Double])
          case "Symbol" =>
            LiteralSymbol(lit.extract[String])
          case "List" =>
            val list = lit.extract[List[LiteralValue]]
            LiteralList(list)
          case x =>
            throw new MappingException("Unknown literal type: " + x)
        }
      case x =>
        throw new MappingException("Cannot convert object " + pretty(render(x)) + "to a literal value.")
    },
    {
      case x: LiteralValue => write(x)
    }
    ))

  object ExpressionSerializer extends CustomSerializer[Expression](format => (
    {
      case JObject(JField("Case", JString(s)) :: JField("Fields", fs) :: Nil) =>
        def createInvalidParameterListException(caseName : String, params : JValue): MappingException = {
          new MappingException("Invalid parameter list for " + caseName + ": " + pretty(render(params)))
        }
        s match {
          case "VariableReference" =>
            fs match {
              case JArray(id :: Nil) =>
                val identifier = id.extract[Identifier]
                VariableReference(identifier)
              case x =>
                throw createInvalidParameterListException("VariableReference", x)
            }
          case "Closure" =>
            fs match {
              case JArray(c :: Nil) =>
                val closure = c.extract[ClosureDefinition]
                Closure(closure)
              case x =>
                throw createInvalidParameterListException("Closure", x)
            }
          case "ProcedureCall" =>
            fs match {
              case JArray(proc :: args :: JBool(isTailCall) :: Nil) =>
                val procedure = proc.extract[Expression]
                val arguments = args.extract[List[Expression]]
                ProcedureCall(procedure, arguments, isTailCall)
              case x =>
                throw createInvalidParameterListException("ProcedureCall", x)
            }
          case "ValueExpression" =>
            fs match {
              case JArray(lit :: Nil) =>
                val literalValue = lit.extract[LiteralValue]
                ValueExpression(literalValue)
              case x =>
                throw createInvalidParameterListException("ValueExpression", x)
            }
          case "Assignment" =>
            fs match {
              case JArray(id :: expr :: Nil) =>
                val identifier = id.extract[Identifier]
                val valueExpression = expr.extract[Expression]
                Assignment(identifier, valueExpression)
              case x =>
                throw createInvalidParameterListException("Assignment", x)
            }
          case "Conditional" =>
            fs match {
              case JArray(cond :: thenBranch :: elseBranch :: Nil) =>
                val conditionExpr = cond.extract[Expression]
                val thenExpr = thenBranch.extract[Expression]
                val elseExpr = elseBranch.extract[Expression]
                Conditional(conditionExpr, thenExpr, elseExpr)
              case x =>
                throw createInvalidParameterListException("Conditional", x)
            }
          case "SequenceExpression" =>
            fs match {
              case JArray(seqType :: exprs :: Nil) =>
                val expressions = exprs.extract[List[Expression]]
                val sequenceType = seqType match {
                  case JObject(JField("Case", JString(seqName)) :: _) =>
                    seqName match {
                      case "AndSequence" => SequenceType.AndSequence
                      case "OrSequence" => SequenceType.OrSequence
                      case "BeginSequence" => SequenceType.BeginSequence
                      case x => throw new MappingException("Unknown sequence type: " + seqName)
                    }
                }
                SequenceExpression(sequenceType, expressions)
              case x =>
                throw createInvalidParameterListException("SequenceExpression", x)
            }
          case x =>
            throw new MappingException("Unknown expression type: " + x)
        }
      case JObject(JField("Case", JString(s)) :: Nil) =>
        s match {
          case "UndefinedValue" => UndefinedValue()
          case x => throw new MappingException("Unknown expression type: " + x)
        }
      case x =>
        throw new MappingException("Cannot convert object " + pretty(render(x)) + " to an expression.")
    },
    {
      case x: Expression => write(x)
    }
    ))

  def convertToAST(json : JValue) : ReadResult = {
    try {
      val programOk = (json \ "valid").extract[Boolean]
      if (programOk) {
        val ast = (json \ "program").extract[ProgramSyntaxTree]
        ReadSuccess(ast)
      } else {
        val errorMsg = (json \ "message").extract[String]
        ReadError(errorMsg)
      }
    } catch {
      case e: MappingException => ReadFailure(e.msg)
    }
  }

  def readAST(input : String) : ReadResult = {
    val json = parse(input)
    convertToAST(json)
  }

}
