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

let analyse str = match run parseProgram str with
                  | Success(result, _, _) -> printfn "Success: %A" (buildAST result)
                  | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

test parseExpression "'()"
test parseProgram "'()"
test parseExpression "(quote (1 2 3))"
test parseProgram "(quote (1 2 3))"
test parseProgram "42(define pi 3.14159)(display pi)42"
test parseProgram "#t#f42"
test parseProgram "'42'52"
analyse "(1)"
analyse "(lambda () (define foo 42))"

let gensym = SymbolGenerator()
gensym.generateSymbol "display"
gensym.generateSymbol "display"
gensym.generateSymbol "myFunction"
gensym.generateSymbol "myFunction"
gensym.generateSymbol "display"