open CottontailScheme.Parsing
open CottontailScheme.ASTBuilder
open CottontailScheme.SymbolGenerator
open CottontailScheme.SemanticAnalysis
open CottontailScheme.CodeGenerator

open FParsec
open System

[<EntryPoint>]
let main args =
    if args.Length < 1 then
        printfn "Error: no input file specified"
    else
        let inputFileName = IO.Path.GetFileNameWithoutExtension args.[0]
        let programName = inputFileName.Split([|"."; "_"|], StringSplitOptions.RemoveEmptyEntries)
                          |> fun xs -> String.Join("-", xs)
                          |> kebabCaseToCamelCase
        let programCode = IO.File.ReadAllText(args.[0])

        match run CottontailScheme.Parsing.parseProgram programCode with
        | Success(result, _, _)
            -> match buildAST result with
                | ASTBuildSuccess exprs
                   -> match analyse exprs with
                      | ValidProgram (exprs, scope) -> generateCodeFor exprs scope programName
                      | ProgramAnalysisError err -> printfn "Error: %s" err
                | ASTBuildFailure errs -> errs |> List.map (fun e -> printfn "Error (line %i, column %i): %A" e.position.line e.position.column e.message) |> ignore
        | Failure(errorMsg, _, _) -> printfn "Error: %s" errorMsg
    0
