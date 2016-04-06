module CottontailScheme.SemanticAnalysis

type SequenceExpressionType = BeginSequence | AndSequence | OrSequence

type Expression =
     | VariableReference of Scope.Identifier
     | Closure of ClosureDefinition
     | ProcedureCall of Expression * Expression list
     | ValueExpression of Literals.LiteralValue
     | Assignment of Scope.Identifier * Expression
     | Conditional of Expression * Expression * Expression
     | IdentifierDefinition of Scope.Identifier * Expression
     | SequenceExpression of SequenceExpressionType * Expression list
     | TailExpression of Expression
     | UndefinedValue
and ClosureFormals = SingleArgFormals of Scope.Identifier
                   | MultiArgFormals of Scope.Identifier list
and ClosureDefinition = { formals: ClosureFormals;
                          body: Expression list;
                          environment: Scope.Identifier list;
                          scope: Scope.Scope
                          isTailRecursive: bool;
                          functionName: Scope.Identifier option;
                          usedAsFirstClassValue: bool }

type ProgramStructure = { functionDefinitions: Expression list;
                          variableDefinitions: Expression list;
                          expressions : Expression list;
                          scope : Scope.Scope }

type Program =
    | ValidProgram of ProgramStructure
    | ProgramAnalysisError of string

val analyse: ASTBuilder.Expression list -> Program
