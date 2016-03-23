module CottontailScheme.ASTBuilder

open CottontailScheme.Parsing

type ASTError = { message: string; position: ParsePosition }

type Identifier = Identifier of string
                | IdentifierError of ASTError

type LambdaFormals = MultiArgFormals of Identifier list
                   | SingleArgFormals of Identifier

type Expression =
    | IdentifierExpression of Identifier
    | BooleanLiteral of bool
    | NumberLiteral of float
    | StringLiteral of string
    | SymbolLiteral of string
    | ListLiteral of Expression list
    | LambdaExpression of LambdaFormals * Expression list * Expression list
    | AssignmentExpression of Binding
    | ProcedureCallExpression of Expression * Expression list
    | ConditionalExpression of Expression * Expression * Expression option
    | Definition of Binding
    | ExpressionError of ASTError
and Binding = Binding of Identifier * Expression
            | BindingError of ASTError

type AnalysisStatus = AnalysisSuccess of Expression list
                    | AnalysisFailure of ASTError list

val buildAST: Parsing.CTProgram -> AnalysisStatus
