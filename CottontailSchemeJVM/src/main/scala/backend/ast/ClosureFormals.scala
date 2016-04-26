package backend.ast

abstract class ClosureFormals
case class SingleArgFormals(id : Identifier) extends ClosureFormals
case class MultiArgFormals(ids : List[Identifier]) extends ClosureFormals
