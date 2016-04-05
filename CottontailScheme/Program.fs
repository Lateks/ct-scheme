open CottontailScheme.Parsing
open FParsec

open CottontailScheme.SemanticAnalysis
open CottontailScheme.CodeGenerator

let testParser p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

[<EntryPoint>]
let main args =
    generateCodeFor (ProgramAnalysisError "placeholder") "hello"
    0
