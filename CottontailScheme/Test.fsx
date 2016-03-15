#r "..\\packages\\FParsec.1.0.2\\lib\\net40-client\\FParsecCS.dll"
#r "..\\packages\\FParsec.1.0.2\\lib\\net40-client\\FParsec.dll"
#load "Parsing.fs"

open FParsec
open CottontailScheme.Parsing

let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

test parseIfExpression "if foo bar baz"
test parseIfExpression "if #false '(1 2 3 4) 42"
test parseIfExpression "if #t 42"
test parseIfExpression "if (< 1 2) (display \"less than\") (display \"greater than\")" // not working yet!
