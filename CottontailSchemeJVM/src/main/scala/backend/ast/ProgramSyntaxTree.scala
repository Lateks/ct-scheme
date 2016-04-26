package backend.ast

case class VariableDeclaration(id: Identifier) { }

case class ProcedureDefinition(id : Identifier, closure : ClosureDefinition) { }

case class ProgramSyntaxTree(programName : String,
                             expressions : List[Expression],
                             procedureDefinitions : List[ProcedureDefinition],
                             variableDeclarations : List[VariableDeclaration]) { }
