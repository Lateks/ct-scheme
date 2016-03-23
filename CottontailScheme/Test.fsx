#r "..\\packages\\FParsec.1.0.2\\lib\\net40-client\\FParsecCS.dll"
#r "..\\packages\\FParsec.1.0.2\\lib\\net40-client\\FParsec.dll"
#load "Parsing.fs"
#load "ASTBuilder.fs"

open FParsec
open CottontailScheme.Parsing
open CottontailScheme.ASTBuilder

let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

let analyse str = match run parseProgram str with
                  | Success(result, _, _) -> printfn "Success: %A" (buildAST result)
                  | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

analyse "(display \"Hello, world!\")\n42\n"
analyse "(define pi 3.14159)"
analyse "(define foo (set! bar \"error\"))"
analyse "(define foo (define bar \"error\"))"
analyse "(define define (display \"error\"))"
analyse "(set! pi 3.14159)"
analyse "(set! lambda (display \"error\"))"
analyse "(define lst (quote (1 2 3)))"
analyse "(if foo (display \"hello!\") (+ 1 2))"
analyse "(if (< (+ 1 2) 5) (display \"smaller\"))"
analyse "(do-side-effect)"
analyse "(lambda (a b) (display \"I was called!\") (+ a b))"
analyse "(define average\n\
            (lambda lst\n\
            (define n (sum lst))\n\
            (if (zero? n)\n\
                0\n\
                (/ n (length lst)))))\n\
         (display (average 1 2 3 4 5))\n"
analyse "(define foo '())"
analyse "'()"

test parseExpression "'()"
test parseProgram "'()"
test parseExpression "(quote (1 2 3))"
test parseProgram "(quote (1 2 3))"
test parseProgram "42(define pi 3.14159)(display pi)42"
test parseProgram "#t#f42"
test parseProgram "'42'52"
analyse "(1)"
