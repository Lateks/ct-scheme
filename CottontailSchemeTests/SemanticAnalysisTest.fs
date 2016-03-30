module SemanticAnalysisTest

open NUnit.Framework
open FsUnit
open FParsec

open CottontailScheme.ASTBuilder
open CottontailScheme.Literals
open CottontailScheme.SemanticAnalysis

let parseAndBuild str = match run CottontailScheme.Parsing.parseProgram str with
                        | Success(result, _, _)
                            -> match buildAST result with
                               | ASTBuildSuccess exprs -> analyse exprs
                               | ASTBuildFailure errs -> failwithf "AST building failed with errors: %A" errs
                        | Failure(errorMsg, _, _) -> failwithf "Parsing failed: %A" errorMsg

let shouldProduceError expectedMsg program =
    match program with
    | ValidProgram (_, _) as prog -> sprintf "Expected ProgramAnalysisError but got ValidProgram: %A" prog
                                     |> Assert.Fail
    | ProgramAnalysisError errMsg -> errMsg |> should equal expectedMsg

let shouldBeValid program =
    match program with
    | ValidProgram (_, _) -> ()
    | ProgramAnalysisError errMsg -> sprintf "Expected ValidProgram but got ProgramAnalysisError: %s" errMsg
                                     |> Assert.Fail

let buildArgumentErrorMsgExactCount name expected got =
    sprintf "Arity mismatch for function %s: expected %i arguments, got %i" name expected got

let buildArgumentErrorMsgAtLeast name expected got =
    sprintf "Arity mismatch for function %s: expected at least %i arguments, got %i" name expected got

// TODO: transformations made by semantic analysis
// - top level definitions -> assignments
// TODO: lambda labeling
// - used as first class value
// - tail recursive
// TODO: variable reference bindings

[<TestFixture>]
type ``Errors detected by semantic analysis`` () =
    [<Test>]
    member x.``detects reference to an undefined variable`` () =
        let errorMsg = "Reference to undefined identifier x"
        parseAndBuild "(x)" |> shouldProduceError errorMsg
        parseAndBuild "(lambda () x)" |> shouldProduceError errorMsg
        parseAndBuild "(lambda () (+ x 1))" |> shouldProduceError errorMsg
        parseAndBuild "(define y x)" |> shouldProduceError errorMsg
        parseAndBuild "(lambda () (define y x) y)" |> shouldProduceError errorMsg

    [<Test>]
    member x.``does not allow setting an undefined variable`` () =
        let errorMsg = "Trying to set! an undefined identifier x"
        parseAndBuild "(set! x 3.14159)" |> shouldProduceError errorMsg
        parseAndBuild "(lambda () (set! x 3.14159))" |> shouldProduceError errorMsg

    [<Test>]
    member x.``detects reference to potentially uninitialized variable`` () =
        let errorMsg = "Variable x may be uninitialized"
        parseAndBuild "(define x (+ x 1))" |> shouldProduceError errorMsg
        parseAndBuild "(define y (+ x 1)) (define x 1)" |> shouldProduceError errorMsg
        parseAndBuild "(lambda () (define y (+ x 1)) (define x 1) (+ x y))" |> shouldProduceError errorMsg

    [<Test>]
    member x.``checks parameter counts for built-in functions`` () =
        parseAndBuild "(display)" |> shouldProduceError (buildArgumentErrorMsgExactCount "display" 1 0)
        parseAndBuild "(display 1 2)" |> shouldProduceError (buildArgumentErrorMsgExactCount "display" 1 2)
        parseAndBuild "(cons 1)" |> shouldProduceError (buildArgumentErrorMsgExactCount "cons" 2 1)
        parseAndBuild "(< 1)" |> shouldProduceError (buildArgumentErrorMsgAtLeast "<" 2 1)
        parseAndBuild "(/)" |> shouldProduceError (buildArgumentErrorMsgAtLeast "/" 1 0)
        parseAndBuild "(define test (lambda (name) (display \"Hello, \" name \"!\")))" |> shouldProduceError (buildArgumentErrorMsgExactCount "display" 1 3)

    [<Test>]
    member x.``checks parameter counts for user defined functions when possible`` () =
        let parameterlessFunctionDefinition = "(define test (lambda () (display \"Hello\")))"
        let singleParamFunctionDefinition = "(define test (lambda (x) (+ x 1)))"
        let multiParamFunctionDefinition = "(define test (lambda (x y z) (+ x y z)))"
        parseAndBuild (parameterlessFunctionDefinition + "(test 1 2)") |> shouldProduceError (buildArgumentErrorMsgExactCount "test" 0 2)
        parseAndBuild (singleParamFunctionDefinition + "(test 1 2)") |> shouldProduceError (buildArgumentErrorMsgExactCount "test" 1 2)
        parseAndBuild (singleParamFunctionDefinition + "(test)") |> shouldProduceError (buildArgumentErrorMsgExactCount "test" 1 0)
        parseAndBuild (multiParamFunctionDefinition + "(test 1 2)") |> shouldProduceError (buildArgumentErrorMsgExactCount "test" 3 2)
        parseAndBuild (multiParamFunctionDefinition + "(test 1 2 3 4)") |> shouldProduceError (buildArgumentErrorMsgExactCount "test" 3 4)
        parseAndBuild "(define outer\
                         (lambda ()\
                           (define test\
                             (lambda (x) x))\
                               (test 1 2)))" |> shouldProduceError (buildArgumentErrorMsgExactCount "test" 1 2)

    [<Test>]
    member x.``detects duplicate definitions for identifiers in inner scopes`` () =
        parseAndBuild "(lambda ()\
                         (define x 1)\
                         (define y 1)\
                         (define x 2)\
                         (+ x y))" |> shouldProduceError "Duplicate definition for identifier x"

