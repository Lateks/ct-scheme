package backend.ast

class VariableDeclaration(id: Identifier) {
  val identifier = id
}

class ProcedureDefinition(id : Identifier, closure : ClosureDefinition) {
  val identifier = id
  val closureDefinition = closure
}

class ProgramSyntaxTree(name : String,
                        exprList : List[Expression],
                        procedures : List[ProcedureDefinition],
                        variables : List[VariableDeclaration]) {
  val programName = name
  val expressions = exprList
  val procedureDefinitions = procedures
  val variableDeclarations = variables
}
