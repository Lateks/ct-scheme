open FParsec

open FParsec

let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

test pfloat "1.25"

test pfloat "1.25E 3"

let str s = pstring s
let betweenStrings s1 s2 p = str s1 >>. p .>> str s2

let floatBetweenBrackets = pfloat |> betweenStrings "[" "]"
let floatBetweenDoubleBrackets = pfloat |> betweenStrings "[[" "]]"

test floatBetweenBrackets "[1.0]"
test floatBetweenBrackets "[]"
test floatBetweenBrackets "[1.0"
test floatBetweenDoubleBrackets "[[9.2]]"


System.Console.ReadKey() |> ignore
