#r "..\\packages\\FParsec.1.0.2\\lib\\net40-client\\FParsecCS.dll"
#r "..\\packages\\FParsec.1.0.2\\lib\\net40-client\\FParsec.dll"
#load "Parsing.fs"

open FParsec
open CottontailScheme.Parsing

let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

test parseList "(; starting list\n\t1 ; first element \n\t2 ; second element \n\t3 ; third element\n\t)"

test parseSugaredQuotation "'(1 2 3)"
test parseSugaredQuotation "'()"
test parseSugaredQuotation "'foo"
test parseSugaredQuotation "'#t"
