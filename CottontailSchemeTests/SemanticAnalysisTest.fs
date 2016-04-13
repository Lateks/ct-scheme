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
    | ValidProgram p as prog -> sprintf "Expected ProgramAnalysisError but got ValidProgram: %A" prog
                                |> Assert.Fail
    | ProgramAnalysisError errMsg -> errMsg |> should equal expectedMsg

let shouldBeValid program =
    match program with
    | ValidProgram p -> ()
    | ProgramAnalysisError errMsg -> sprintf "Expected ValidProgram but got ProgramAnalysisError: %s" errMsg
                                     |> Assert.Fail

let getStructure program =
    match program with
    | ValidProgram p -> p
    | ProgramAnalysisError errMsg -> failwithf "Expected ValidProgram but got ProgramAnalysisError: %s" errMsg

let buildArgumentErrorMsgExactCount name expected got =
    sprintf "Arity mismatch for function %s: expected %i arguments, got %i" name expected got

let buildArgumentErrorMsgAtLeast name expected got =
    sprintf "Arity mismatch for function %s: expected at least %i arguments, got %i" name expected got

let getFunction =
    getStructure
    >> fun p -> p.procedureDefinitions
    >> List.head
    >> fun (ProcedureDefinition (id, c)) -> c

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
        parseAndBuild "(define apply\
                         (lambda (f x)\
                           (f x)))\
                       (display (apply + 1000 8000))"
                      |> shouldProduceError (buildArgumentErrorMsgExactCount "apply" 2 3)

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
        |> getStructure
        |> fun p -> let (VariableDeclaration id1) = List.head p.variableDeclarations
                    let def = List.last p.expressions
                    match def with
                    | Assignment (id2, expr) ->
                        id1 |> should equal id2
                        expr |> should equal (ValueExpression (Number 3.0))
                    | e -> gotWrongExpression e

[<TestFixture>]
type ``Lambda labeling`` () =
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
    member x.``tail calls are correctly labeled`` () =
        let fail () = Assert.Fail "Lambda body was not as expected"
        parseAndBuild "(define sum\
                         (lambda (l acc)\
                           (if (null? l)\
                               acc\
                               (sum (cdr l) (+ (car l) acc)))))"
        |> getFunction
        |> fun c -> match List.head c.body with
                    | Conditional (cond, thenExpr, elseExpr)
                        -> match cond with
                           | ProcedureCall (_, _, tailCall) -> tailCall |> should equal false
                           | _ -> fail ()
                           match elseExpr with
                           | ProcedureCall (_, args, tailCall)
                               -> tailCall |> should equal true
                                  match List.head args with
                                  | ProcedureCall (_, _, tailCall) -> tailCall |> should equal false
                                  | _ -> fail ()
                           | _ -> fail ()
                    | _ -> fail ()

        parseAndBuild "(define println\
                         (lambda (s)\
                           (display s)\
                           (newline)))"
        |> getFunction
        |> fun c -> match List.head c.body with
                    | ProcedureCall (_, _, tailCall) -> tailCall |> should equal false
                    | _ -> fail ()
                    match List.last c.body with
                    | ProcedureCall (_, _, tailCall) -> tailCall |> should equal true
                    | _ -> fail ()

        parseAndBuild "(define println\
                        (lambda (s)\
                          (begin\
                            (display s)\
                            (newline))))"
        |> getFunction
        |> fun c -> match List.head c.body with
                    | SequenceExpression (_, exprs) ->
                        match List.head exprs with
                        | ProcedureCall (_, _, tailCall) -> tailCall |> should equal false
                        | _ -> fail ()
                        match List.last exprs with
                        | ProcedureCall (_, _, tailCall) -> tailCall |> should equal true
                        | _ -> fail ()
                    | _ -> fail ()
        
        parseAndBuild "(define println\
                        (lambda (s)\
                          (and\
                            (display s)\
                            (newline))))"
        |> getFunction
        |> fun c -> match List.head c.body with
                    | SequenceExpression (_, exprs) ->
                        match List.head exprs with
                        | ProcedureCall (_, _, tailCall) -> tailCall |> should equal false
                        | _ -> fail ()
                        match List.last exprs with
                        | ProcedureCall (_, _, tailCall) -> tailCall |> should equal true
                        | _ -> fail ()
                    | _ -> fail ()

    [<Test>]
    member x.``lambdas are correctly labeled for first class use and identifier reassignment`` () =
        let isFirstClassValue = getFunction >> fun c -> c.isUsedAsFirstClassValue
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

        parseAndBuild "(define square (lambda (x) (display \"not implemented!\")))\
                       (set! square (lambda (x) (* x x)))"
        |> getFunction
        |> fun c -> c.isReassigned |> should equal true
                    c.isUsedAsFirstClassValue |> should equal false

        parseAndBuild "(define square (lambda (x) (display \"not implemented!\")))\
                       (define square (lambda (x) (* x x)))"
        |> getFunction
        |> fun c -> c.isReassigned |> should equal true
                    c.isUsedAsFirstClassValue |> should equal false


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
                       | Closure c -> c.isUsedAsFirstClassValue |> should equal true
                       | e -> sprintf "Expected Closure but got %A" e
                              |> Assert.Fail

        parseAndBuild "(lambda (x) (+ x 1))"
        |> getStructure
        |> fun p -> p.expressions
        |> List.head
        |> function
           | Closure c -> c.isUsedAsFirstClassValue |> should equal true
           | e -> sprintf "Expected Closure but got %A" e
                  |> Assert.Fail

        parseAndBuild "((lambda (x) (+ x 1)) 5)"
        |> getStructure
        |> fun p -> p.expressions
        |> List.head
        |> function
           | ProcedureCall ((Closure c), _, _) -> c.isUsedAsFirstClassValue |> should equal false
           | e -> sprintf "Expected ProcedureCall calling the anonymous function but got %A" e
                  |> Assert.Fail

    [<Test>]
    member x.``free variables are correctly recognised`` () =
        let shouldBeNamed name (id: CottontailScheme.Scope.Identifier) = id.name |> should equal name

        parseAndBuild "(define add\
                         (lambda (x)\
                           (lambda (y)\
                             (+ x y))))"
        |> getStructure
        |> fun p -> p.procedureDefinitions
        |> List.head
        |> fun (ProcedureDefinition (id, c))
              -> List.isEmpty c.environment |> should equal true
                 match List.head c.body with
                 | Closure c2 ->
                     c2.environment.Length |> should equal 2
                     c2.environment |> List.head |> shouldBeNamed "+"
                     c2.environment |> List.last |> shouldBeNamed "x"
                 | _ -> Assert.Fail "Lambda body was not as expected"

        parseAndBuild "(define y 1)\
                       (define test\
                         (lambda (x)\
                           (+ x y)))"
        |> getStructure
        |> fun p -> p.procedureDefinitions
        |> List.head
        |> fun (ProcedureDefinition (id, c))
               -> c.environment.Length |> should equal 2
                  c.environment |> List.head |> shouldBeNamed "+"
                  c.environment |> List.last |> shouldBeNamed "y"

[<TestFixture>]
type ``Name bindings`` () =
    let getDeclaredId (VariableDeclaration id) = id
    let rec getId = function
                    | VariableReference id -> id
                    | Assignment (id, expr) -> id
                    | e -> failwithf "Expected a variable reference or assignment but got %A" e

    let handleClosure f closure =
        match closure with
        | Closure c -> f c
        | e -> sprintf "Expected a Closure but got %A" e
               |> Assert.Fail

    [<Test>]
    member x.``variable references are bound to the definition in the nearest scope`` () =
        parseAndBuild "(define x 1)\
                       x"
        |> getStructure
        |> fun p -> getDeclaredId (List.head p.variableDeclarations) |> should equal (getId (List.last p.expressions))

        parseAndBuild "(define x 1)\
                       (lambda ()\
                         (define x 2)
                         x)"
        |> getStructure
        |> fun p -> let firstX = List.head p.variableDeclarations |> getDeclaredId
                    List.last p.expressions
                    |> handleClosure (fun c ->
                                            let secondX = List.head c.body |> getId
                                            let thirdX = List.last c.body |> getId
                                            firstX |> should not' (equal secondX)
                                            secondX |> should equal thirdX)

        parseAndBuild "(define x 1)\
                       (lambda () x)"
        |> getStructure
        |> fun p -> let firstX = List.head p.variableDeclarations |> getDeclaredId
                    List.last p.expressions
                    |> handleClosure (fun c ->
                                            let secondX = List.head c.body |> getId
                                            firstX |> should equal secondX)

        parseAndBuild "(define test (lambda () test))"
        |> getFunction
        |> fun c -> List.head c.body
                    |> getId
                    |> should equal c.functionName.Value
