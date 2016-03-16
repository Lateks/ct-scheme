#r "..\\packages\\FParsec.1.0.2\\lib\\net40-client\\FParsecCS.dll"
#r "..\\packages\\FParsec.1.0.2\\lib\\net40-client\\FParsec.dll"
#load "Parsing.fs"

open FParsec
open CottontailScheme.Parsing

let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

test parseExpression "(if foo bar baz)"
test parseExpression "(set! answer 42)"
test parseExpression "(quote (1 2 3))"
test parseExpression "(quoter 1 2 3)"
test parseExpression "(lambda l (display l))"

test parseExpression "(define pi 3.14159)"
