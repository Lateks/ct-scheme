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

let getTree program =
    match program with
    | ValidProgram (tree, _) -> tree
    | ProgramAnalysisError errMsg -> failwithf "Expected ValidProgram but got ProgramAnalysisError: %s" errMsg

let buildArgumentErrorMsgExactCount name expected got =
    sprintf "Arity mismatch for function %s: expected %i arguments, got %i" name expected got

let buildArgumentErrorMsgAtLeast name expected got =
    sprintf "Arity mismatch for function %s: expected at least %i arguments, got %i" name expected got

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

[<TestFixture>]
type ``Transformations made during semantic analysis`` () =
    [<Test>]
    member x.``duplicate definitions on the top level are changed into assignments`` () =
        let gotWrongExpression e = sprintf "Expected an Assignment but got %A" e
                                   |> Assert.Fail
        parseAndBuild "(define x 1)(define y 2)(define x 3)"
        |> getTree
        |> fun exprs -> let def1 = List.head exprs
                        let def2 = List.last exprs
                        match def1 with
                        | IdentifierDefinition (id1, expr1) ->
                            match def2 with
                            | Assignment (id2, expr2) ->
                                id1 |> should equal id2
                                expr1 |> should equal (ValueExpression (Number 1.0))
                                expr2 |> should equal (ValueExpression (Number 3.0))
                            | IdentifierDefinition (_, _) ->
                                Assert.Fail "Expected identifier definition to have been converted to assignment but it wasn't"
                            | e -> gotWrongExpression e
                        | e -> gotWrongExpression e

    [<Test>]
    member x.``tail expressions are labeled`` () =
        let getTailExpression closure =
            match closure with
            | Closure c -> List.last c.body
            | e -> failwithf "Expected a Closure but got %A" e
        let getTail = getTree >> List.head >> getTailExpression
        let testConditional =
            function
            | Conditional (e1, e2, e3) -> match e1, e2, e3 with
                                          | VariableReference _, TailExpression _, Some (TailExpression _) -> ()
                                          | f1, f2, f3 -> sprintf "Expected branches to be tagged as TailExpressions but got: %A\n%A\n%A\n" f1 f2 f3
                                                          |> Assert.Fail 
            | e -> sprintf "Expected a Conditional but got a %A" e
                   |> Assert.Fail

        parseAndBuild "(lambda () 1)"
        |> getTail
        |> should equal (TailExpression (ValueExpression (Number 1.0)))

        parseAndBuild "(lambda () (+ 1 2))"
        |> getTail
        |> should equal (TailExpression (ProcedureCall (VariableReference { name = "+"; uniqueName = "+" },
                                                        [ValueExpression (Number 1.0); ValueExpression (Number 2.0)])))

        parseAndBuild "(lambda (v) (display \"started\\n\") (if v (display \"true\") (display \"false\")))"
        |> getTail
        |> testConditional

        // TODO: should the missing branch be tagged in some way,
        // e.g. marked as returning an undefined value?
        parseAndBuild "(lambda (v) (if v (display \"v is truthy\")))"
        |> getTail
        |> function
           | Conditional (e1, e2, e3) ->
                match e1, e2, e3 with
                | VariableReference _, TailExpression _, None -> ()
                | f1, f2, f3 -> sprintf "Expected branches to be tagged as TailExpressions but got: %A\n%A\n%A\n" f1 f2 f3
                                |> Assert.Fail 
           | e -> sprintf "Expected a Conditional but got a %A" e
                  |> Assert.Fail

        parseAndBuild "(lambda (x y z) (and x y z))"
        |> getTail
        |> function
           | SequenceExpression (t, exprs) -> match List.last exprs with
                                              | TailExpression _ -> ()
                                              | e -> sprintf "Expected a tail expression but got %A" e
                                                     |> Assert.Fail
           | e -> sprintf "Expected a SequenceExpression but got %A" e
                  |> Assert.Fail

        parseAndBuild "(lambda (x y z v) (and v (if x y z)))"
        |> getTail
        |> function
           | SequenceExpression (t, exprs) ->
                List.last exprs
                |> testConditional
           | e -> sprintf "Expected a SequenceExpression but got %A" e
                  |> Assert.Fail

[<TestFixture>]
type ``Lambda labeling`` () =
    let getFunction =
        getTree >> List.head >> function
                                | IdentifierDefinition (id, (Closure c)) -> c
                                | e -> failwithf "Expected a function definition but got %A" e

    [<Test>]
    member x.``lambdas are correctly labeled as tail recursive`` () =
        let isTailRecursive = getFunction >> fun c -> c.isTailRecursive
        let shouldBeTailRecursive = isTailRecursive >> should equal true
        let shouldNotBeTailRecursive = isTailRecursive >> should equal false

        parseAndBuild "(define sum\
                         (lambda (l acc)\
                           (if (null? l)\
                               acc\
                               (sum (cdr l) (+ (car l) acc)))))"
        |> shouldBeTailRecursive

        parseAndBuild "(define print-list\
                          (lambda (l)\
                             (if (null? l)\
                                 (display \"\\n\")\
                                 (begin\
                                   (display (car l))\
                                   (print-list (cdr l))))))"
        |> shouldBeTailRecursive

        parseAndBuild "(define hello (lambda () (display \"hello\")))"
        |> shouldNotBeTailRecursive

        parseAndBuild "(define factorial\
                         (lambda (n)\
                           (if (zero? n)\
                               1\
                               (* n (factorial (- n 1))))))"
        |> shouldNotBeTailRecursive

    [<Test>]
    member x.``lambdas are correctly labeled for first class use`` () =
        let isFirstClassValue = getFunction >> fun c -> c.usedAsFirstClassValue
        let shouldBeFirstClass = isFirstClassValue >> should equal true
        let shouldNotBeFirstClass = isFirstClassValue >> should equal false

        parseAndBuild "(define sum\
                         (lambda (l acc)\
                           (if (null? l)\
                               acc\
                               (sum (cdr l) (+ (car l) acc)))))\
                       (sum '(1 2 3 4 5) 0)"
        |> shouldNotBeFirstClass

        parseAndBuild "(define square (lambda (x) (* x x)))\
                       (define apply (lambda (f x) (f x)))\
                       (apply square 4)"
        |> shouldBeFirstClass

        parseAndBuild "(define square (lambda (x) (* x x)))\
                       (define get-square (lambda () square))"
        |> shouldBeFirstClass

        // If the function value can be overwritten, it is treated as a first class value here.
        parseAndBuild "(define square (lambda (x) (display \"not implemented!\")))\
                       (set! square (lambda (x) (* x x)))"
        |> shouldBeFirstClass

        parseAndBuild "(define square (lambda (x) (display \"not implemented!\")))\
                       (define square (lambda (x) (* x x)))"
        |> shouldBeFirstClass

        parseAndBuild "(define f1 (lambda (x) (+ x 1)))\
                       (define f2 (lambda (x) (* x 2)))\
                       (define get (lambda (v) (if v f1 f2)))"
        |> shouldBeFirstClass

        // Testing the other conditional branch.
        parseAndBuild "(define f1 (lambda (x) (+ x 1)))\
                       (define f2 (lambda (x) (* x 2)))\
                       (define get (lambda (v) (if v f2 f1)))"
        |> shouldBeFirstClass

        parseAndBuild "(define add\
                         (lambda (x)
                           (lambda (y) (+ x y))))"
        |> getFunction
        |> fun c -> List.last c.body
                    |> function
                       | TailExpression e ->
                           match e with
                           | Closure c -> c.usedAsFirstClassValue |> should equal true
                           | e -> sprintf "Expected Closure but got %A" e
                                  |> Assert.Fail
                       | e -> sprintf "Expected TailExpression but got %A" e
                              |> Assert.Fail

        parseAndBuild "(lambda (x) (+ x 1))"
        |> getTree
        |> List.head
        |> function
           | Closure c -> c.usedAsFirstClassValue |> should equal true
           | e -> sprintf "Expected Closure but got %A" e
                  |> Assert.Fail

        parseAndBuild "((lambda (x) (+ x 1)) 5)"
        |> getTree
        |> List.head
        |> function
           | ProcedureCall ((Closure c), _) -> c.usedAsFirstClassValue |> should equal false
           | e -> sprintf "Expected ProcedureCall calling the anonymous function but got %A" e
                  |> Assert.Fail