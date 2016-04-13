module CottontailScheme.SemanticAnalysis

type SequenceExpressionType = BeginSequence | AndSequence | OrSequence

type ClosureFormals = SingleArgFormals of Scope.Identifier
                    | MultiArgFormals of Scope.Identifier list

type Expression =
     | VariableReference of Scope.Identifier
     | Closure of ClosureDefinition
     | ProcedureCall of Expression * Expression list * bool
     | ValueExpression of Literals.LiteralValue
     | Assignment of Scope.Identifier * Expression
     | Conditional of Expression * Expression * Expression
     | SequenceExpression of SequenceExpressionType * Expression list
     | UndefinedValue
and ClosureDefinition = { formals: ClosureFormals;
                          procedureDefinitions: ProcedureDefinition list;
                          variableDeclarations : VariableDeclaration list;
                          body: Expression list;
                          environment: Scope.Identifier list;
                          scope: Scope.Scope
                          isTailRecursive: bool;
                          functionName: Scope.Identifier option;
                          isUsedAsFirstClassValue: bool;
                          isReassigned : bool }
and ProcedureDefinition = ProcedureDefinition of Scope.Identifier * ClosureDefinition
and VariableDeclaration = VariableDeclaration of Scope.Identifier

type ProgramStructure = { procedureDefinitions: ProcedureDefinition list;
                          variableDeclarations: VariableDeclaration list;
                          expressions : Expression list;
                          scope : Scope.Scope }

type Program =
    | ValidProgram of ProgramStructure
    | ProgramAnalysisError of string

val analyse: ASTBuilder.Expression list -> Program
