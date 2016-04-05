open CottontailScheme.Parsing
open CottontailScheme.ASTBuilder
open CottontailScheme.SemanticAnalysis
open CottontailScheme.CodeGenerator

open FParsec

[<EntryPoint>]
let main args =
    let programName = "hello"
    let programCode = ""

    match run CottontailScheme.Parsing.parseProgram programCode with
    | Success(result, _, _)
        -> match buildAST result with
            | ASTBuildSuccess exprs
               -> match analyse exprs with
                  | ValidProgram (exprs, scope) -> generateCodeFor exprs scope programName
                  | ProgramAnalysisError err -> failwithf "Program analysis failed: %A" err
            | ASTBuildFailure errs -> failwithf "AST building failed with errors: %A" errs
    | Failure(errorMsg, _, _) -> failwithf "Parsing failed: %A" errorMsg
    0
