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
test parseIfExpression "if (< 1 2) (display \"less than\") (display \"greater than\")"

test parseAssignmentExpression "set! answer 42"
test parseAssignmentExpression "set! +string \"plus\""
test parseAssignmentExpression "set! #f #t"

test parseQuotationExpression "quote (1 2 3)"

test parseProcedureCallExpression "display \"Hello, world!\""

test parseLambdaExpression "lambda (a b c) (display a) (display b) (display c)"
test parseLambdaExpression "lambda (a) (define b (* a 2)) (display b)"
test parseLambdaExpression "lambda l (display (car l))"

test parseParenthesisedExpression "(if foo bar baz)"
test parseParenthesisedExpression "(set! answer 42)"
test parseParenthesisedExpression "(quote (1 2 3))"
test parseParenthesisedExpression "(quoter 1 2 3)"
test parseParenthesisedExpression "(lambda l (display l))"

test parseDefinition "(define pi 3.14159)"
