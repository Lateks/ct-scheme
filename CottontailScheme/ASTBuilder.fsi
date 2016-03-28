module CottontailScheme.ASTBuilder

open CottontailScheme.Parsing

type ASTError = { message: string; position: ParsePosition }

type Identifier = Identifier of string
                | IdentifierError of ASTError

type LambdaFormals = MultiArgFormals of Identifier list
                   | SingleArgFormals of Identifier

type LiteralValue =
    | Boolean of bool
    | Number of float
    | String of string
    | Symbol of string
    | List of LiteralValue list

type Expression =
    | IdentifierExpression of Identifier
    | LiteralExpression of LiteralValue
    | LambdaExpression of LambdaFormals * Expression list * Expression list
    | AssignmentExpression of Binding
    | ProcedureCallExpression of Expression * Expression list
    | ConditionalExpression of Expression * Expression * Expression option
    | Definition of Binding
    | BeginExpression of Expression list
    | ExpressionError of ASTError
and Binding = Binding of Identifier * Expression
            | BindingError of ASTError

type ASTBuildStatus = ASTBuildSuccess of Expression list
                    | ASTBuildFailure of ASTError list

val buildAST: Parsing.CTProgram -> ASTBuildStatus
