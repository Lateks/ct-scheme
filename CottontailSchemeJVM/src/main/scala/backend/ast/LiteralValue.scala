package backend.ast

abstract class LiteralValue
case class LiteralBoolean(b : Boolean) extends LiteralValue
case class LiteralNumber(f : Double) extends LiteralValue
case class LiteralString(s : String) extends LiteralValue
case class LiteralSymbol(s : Symbol) extends LiteralValue
case class LiteralList(l : List[LiteralValue]) extends LiteralValue
