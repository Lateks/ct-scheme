package backend.ast
import backend.ast.SequenceType._

abstract class Expression
case class VariableReference(id : Identifier) extends Expression
case class Closure(closure : ClosureDefinition) extends Expression
case class ProcedureClass(proc : Expression, args : List[Expression], isTailCall : Boolean) extends Expression
case class ValueExpression(lit : LiteralValue) extends Expression
case class Assignment(id : Identifier, valueExpr : Expression) extends Expression
case class Conditional(condition : Expression, thenBranch : Expression, elseBranch : Expression) extends Expression
case class SequenceExpression(sequenceType : SequenceType, expressions : List[Expression]) extends Expression
case class UndefinedValue() extends Expression