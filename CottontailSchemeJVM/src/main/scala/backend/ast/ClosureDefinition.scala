package backend.ast

case class ClosureDefinition(functionName : Identifier,
                             formals : ClosureFormals,
                             body : List[Expression],
                             procedureDefinitions : List[ProcedureDefinition],
                             variableDeclarations : List[VariableDeclaration],
                             environment : List[Identifier],
                             isTailRecursive : Boolean,
                             isUsedAsFirstClassValue : Boolean,
                             isReassigned : Boolean)
