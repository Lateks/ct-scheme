package backend.ast

class ClosureDefinition(args : ClosureFormals,
                        bodyExpressions : List[Expression],
                        procedures : List[ProcedureDefinition],
                        variables : List[VariableDeclaration],
                        env : List[Identifier],
                        tailRec : Boolean,
                        firstClass : Boolean,
                        reassigned : Boolean) {
  val formals = args
  val procedureDefinitions = procedures
  val variableDeclarations = variables
  val body = bodyExpressions
  val environment = env
  val isTailRecursive = tailRec
  val isUsedAsFirstClassValue = firstClass
  val isReassigned = reassigned
}
