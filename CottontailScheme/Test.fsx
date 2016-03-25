#r "..\\packages\\FParsec.1.0.2\\lib\\net40-client\\FParsecCS.dll"
#r "..\\packages\\FParsec.1.0.2\\lib\\net40-client\\FParsec.dll"
#load "Parsing.fs"
#load "ASTBuilder.fs"
#load "SemanticAnalysis.fs"

open FParsec
open CottontailScheme.Parsing
open CottontailScheme.ASTBuilder
open CottontailScheme.SemanticAnalysis

let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

let makeAST str = match run parseProgram str with
                  | Success(result, _, _) -> printfn "Success: %A" (buildAST result)
                  | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

test parseExpression "'()"
test parseProgram "'()"
test parseExpression "(quote (1 2 3))"
test parseProgram "(quote (1 2 3))"
test parseProgram "42(define pi 3.14159)(display pi)42"
test parseProgram "#t#f42"
test parseProgram "'42'52"
makeAST "(1)"
makeAST "(lambda () (define foo 42))"

let gensym = SymbolGenerator()
gensym.generateSymbol "display"
gensym.generateSymbol "display"
gensym.generateSymbol "myFunction"
gensym.generateSymbol "myFunction"
gensym.generateSymbol "display"

analyse [Definition (Binding (Identifier "pi", LiteralExpression (Number 3.14159)))
         IdentifierExpression (Identifier "pi")
         IdentifierExpression (Identifier "+")]
analyse [Definition (Binding (Identifier "pi", LiteralExpression (Number 3.14159)))
         Definition (Binding (Identifier "pi", LiteralExpression (Number 3.0)))]
analyse [ConditionalExpression (LiteralExpression (Boolean true), LiteralExpression (String "true"), Some (LiteralExpression (String "false")))]
analyse [ConditionalExpression (LiteralExpression (Boolean true), LiteralExpression (String "true"), None)]
analyse [Definition (Binding (Identifier "pi", LiteralExpression (Number 3.14159)))
         ConditionalExpression (ProcedureCallExpression (IdentifierExpression (Identifier "<"), [IdentifierExpression (Identifier "pi"); LiteralExpression (Number 3.0)]),
                                ProcedureCallExpression (IdentifierExpression (Identifier "display"), [LiteralExpression (String "smaller than")]),
                                Some (ProcedureCallExpression (IdentifierExpression (Identifier "display"), [LiteralExpression (String "greater than")])))]
analyse [AssignmentExpression (Binding (Identifier "pi", LiteralExpression (Number 3.14159)))]
analyse [Definition (Binding (Identifier "pi", LiteralExpression (Number -1.0)))
         AssignmentExpression (Binding (Identifier "pi", LiteralExpression (Number 3.14159)))]
analyse [Definition (Binding (Identifier "say-hello", LambdaExpression (CottontailScheme.ASTBuilder.MultiArgFormals [(Identifier "name")],
                                                                        [],
                                                                        [ProcedureCallExpression (IdentifierExpression (Identifier "display"), [LiteralExpression (String "Hello, ")])
                                                                         ProcedureCallExpression (IdentifierExpression (Identifier "display"), [IdentifierExpression (Identifier "name")])
                                                                         ProcedureCallExpression (IdentifierExpression (Identifier "display"), [LiteralExpression (String "!\n")])])))
         ProcedureCallExpression (IdentifierExpression (Identifier "say-hello"), [LiteralExpression (String "Laura")])]
