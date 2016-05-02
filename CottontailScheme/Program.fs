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

type ValidOutput = { valid : bool; program : ProgramStructure }
type ErrorOutput = { valid : bool; message : String }

let generateCode p =
    match generateCodeFor p with
    | CodeGenSuccess msg -> printfn "Success: %s" msg
    | CodeGenInternalError msg -> printfn "Internal error occurred: %s" msg

let printJson op =
    let json = JsonConvert.SerializeObject(op, Formatting.Indented)
    printfn "%s" json

let outputJson p =
    { valid = true; program = p } |> printJson

let outputError outputType msg =
    match outputType with
    | Exe -> printfn "%s" msg
    | Json -> { valid = false; message = msg } |> printJson
              
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

        let displayError = outputError !outputType
        let programCode = try
                             let text = IO.File.ReadAllText(args.[fileNamePosition])
                             Some text
                          with
                          | e -> let err = sprintf "Error reading file %s:\n%A" args.[fileNamePosition] e
                                 displayError err
                                 None

        try
            match programCode with
            | Some programText ->
                match run CottontailScheme.Parsing.parseProgram programText with
                | Success(result, _, _)
                    -> match buildAST result with
                        | ASTBuildSuccess exprs
                           -> match analyse exprs programName with
                              | ValidProgram p -> match !outputType with
                                                  | Json -> outputJson p
                                                  | Exe -> generateCode p
                              | ProgramAnalysisError err -> sprintf "Error: %s" err |> displayError
                        | ASTBuildFailure errs -> errs |> List.map (fun e -> sprintf "Error (line %i, column %i): %A" e.position.line e.position.column e.message)
                                                       |> String.concat "\n"
                                                       |> displayError
                | Failure(errorMsg, _, _) -> sprintf "Error: %s" errorMsg |> displayError
            | None -> if !outputType = Exe then printfn "Exiting..."
        with
        | e -> sprintf "Unexpected error occurred:\n%A" e |> displayError
    0
