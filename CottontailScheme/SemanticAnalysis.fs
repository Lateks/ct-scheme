module CottontailScheme.SemanticAnalysis

open CottontailScheme.ASTBuilder

let placeholder name = failwithf "Not implemented yet: %s" name

let rec analyse exprs =
    exprs
    |> List.map (function
                 | IdentifierExpression id -> placeholder "identifier expressions"
                 | LambdaExpression (formals, defs, exprs) -> placeholder "lambda expressions"
                 | AssignmentExpression binding -> placeholder "assignments"
                 | Definition binding -> placeholder "definitions"
                 | ProcedureCallExpression (expr, exprs) -> placeholder "procedure calls"
                 | ConditionalExpression (cond, thenBranch, elseBranch) -> placeholder "conditionals"
                 | BooleanLiteral b -> placeholder "boolean literals"
                 | NumberLiteral n -> placeholder "number literals"
                 | StringLiteral s -> placeholder "string literals"
                 | SymbolLiteral s -> placeholder "symbol literals"
                 | ListLiteral elist -> placeholder "list literals"
                 | ExpressionError err -> failwith "Error, faulty AST given as input to analysis. Contains error \"%s\"." err.message
                )