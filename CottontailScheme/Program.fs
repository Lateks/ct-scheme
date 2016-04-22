open CottontailScheme.Parsing
open CottontailScheme.ASTBuilder
open CottontailScheme.SymbolGenerator
open CottontailScheme.SemanticAnalysis
open CottontailScheme.CodeGenerator

open FParsec
open System
open Newtonsoft.Json

type OutputType = Json
                | Exe

let generateCode p programName =
    match generateCodeFor p programName with
    | CodeGenSuccess msg -> printfn "Success: %s" msg
    | CodeGenInternalError msg -> printfn "Internal error occurred: %s" msg

let outputJson p =
    let json = JsonConvert.SerializeObject(p, Formatting.Indented)
    printfn "%s" json

[<EntryPoint>]
let main args =
    if args.Length < 1 then
        printfn "Error: no input file specified"
    else
        let outputType = ref Exe
        let fileNamePosition = args.Length - 1
        for flag in Array.take fileNamePosition args do
            match flag with
            | "-json" -> outputType := Json
            | f -> printfn "Unrecognized flag %s" f

        let inputFileName = IO.Path.GetFileNameWithoutExtension args.[fileNamePosition]
        let programName = inputFileName.Split([|"."; "_"|], StringSplitOptions.RemoveEmptyEntries)
                          |> fun xs -> String.Join("-", xs)
                          |> kebabCaseToCamelCase
        let programCode = IO.File.ReadAllText(args.[fileNamePosition])

        match run CottontailScheme.Parsing.parseProgram programCode with
        | Success(result, _, _)
            -> match buildAST result with
                | ASTBuildSuccess exprs
                   -> match analyse exprs with
                      | ValidProgram p -> match !outputType with
                                          | Json -> outputJson p
                                          | Exe -> generateCode p programName
                      | ProgramAnalysisError err -> printfn "Error: %s" err
                | ASTBuildFailure errs -> errs |> List.map (fun e -> printfn "Error (line %i, column %i): %A" e.position.line e.position.column e.message) |> ignore
        | Failure(errorMsg, _, _) -> printfn "Error: %s" errorMsg
    0
