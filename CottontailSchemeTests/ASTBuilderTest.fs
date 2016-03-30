module ASTBuilderTest

open NUnit.Framework
open FsUnit
open FParsec

open CottontailScheme.ASTBuilder
open CottontailScheme.Literals

module ASTBuilderTest =
    let build str = match run CottontailScheme.Parsing.parseProgram str with
                    | Success(result, _, _) -> buildAST result
                    | Failure(errorMsg, _, _) -> failwithf "Parsing failed: %A" errorMsg

[<TestFixture>]
type ``Builds AST for simple valid programs`` () =
    [<Test>]
    member x.``builds AST for a valid Hello, World program`` () =
        ASTBuilderTest.build "(display \"Hello, world!\")"
        |> should equal (ASTBuildSuccess
                            [ProcedureCallExpression
                                (IdentifierExpression (Identifier "display"), [LiteralExpression (String "Hello, world!")])])

    [<Test>]
    member x.``builds AST for a longer valid program`` () =
        ASTBuilderTest.build "(define average\n\
                                 (lambda lst\n\
                                    (define n (sum lst))\n\
                                    (if (zero? n)\n\
                                        0\n\
                                        (/ n (length lst)))))\n\
                              (display (average 1 2 3 4 5))\n"
        |> should equal (ASTBuildSuccess
                            [Definition (Binding (Identifier "average",
                                                  LambdaExpression (SingleArgFormals (Identifier "lst"),
                                                                    [Definition (Binding (Identifier "n",
                                                                                          ProcedureCallExpression (IdentifierExpression (Identifier "sum"),
                                                                                                                   [IdentifierExpression (Identifier "lst")])))],
                                                                    [ConditionalExpression (ProcedureCallExpression (IdentifierExpression (Identifier "zero?"),
                                                                                                                     [IdentifierExpression (Identifier "n")]),
                                                                                            (LiteralExpression (Number 0.0)),
                                                                                            Some (ProcedureCallExpression (IdentifierExpression (Identifier "/"),
                                                                                                                           [IdentifierExpression (Identifier "n")
                                                                                                                            ProcedureCallExpression (IdentifierExpression (Identifier "length"),
                                                                                                                                                     [IdentifierExpression (Identifier "lst")])])))])))
                             ProcedureCallExpression (IdentifierExpression (Identifier "display"),
                                                      [ProcedureCallExpression (IdentifierExpression (Identifier "average"),
                                                                                [LiteralExpression (Number 1.0);
                                                                                 LiteralExpression (Number 2.0);
                                                                                 LiteralExpression (Number 3.0);
                                                                                 LiteralExpression (Number 4.0);
                                                                                 LiteralExpression (Number 5.0)])])])

[<TestFixture>]
type ``Classifies different constructs correctly`` () =
    let testSingleExpression str expectedExpr =
        ASTBuilderTest.build str |> should equal (ASTBuildSuccess [expectedExpr])

    [<Test>]
    member x.``classifies a numeric literal expression correctly`` () =
        testSingleExpression "42" (LiteralExpression (Number 42.0))
        testSingleExpression "3.14159" (LiteralExpression (Number 3.14159))

    [<Test>]
    member x.``classifies a boolean literal expression correctly`` () =
        testSingleExpression "#t" (LiteralExpression (Boolean true))
        testSingleExpression "#f" (LiteralExpression (Boolean false))

    [<Test>]
    member x.``classifies a (quoted) symbol literal expression correctly`` () =
        testSingleExpression "'+" (LiteralExpression (Symbol "+"))
        testSingleExpression "(quote +)" (LiteralExpression (Symbol "+"))

    [<Test>]
    member x.``classifies a (quoted) list literal expression correctly`` () =
        let expectedExpression = LiteralExpression (List [Number 1.0; Number 2.0; Number 3.0])
        testSingleExpression "'(1 2 3)" expectedExpression
        testSingleExpression "(quote (1 2 3))" expectedExpression

    [<Test>]
    member x.``classifies a string literal expression correctly`` () =
        testSingleExpression "\"Hello, world!\"" (LiteralExpression (String "Hello, world!"))

    [<Test>]
    member x.``classifies a lambda expression correctly`` () =
        testSingleExpression "(lambda (name) (display \"Hello, \") (display name))"
        <| LambdaExpression (MultiArgFormals [Identifier "name"],
                                             [],
                                             [ProcedureCallExpression (IdentifierExpression (Identifier "display"), [LiteralExpression (String "Hello, ")])
                                              ProcedureCallExpression (IdentifierExpression (Identifier "display"), [IdentifierExpression (Identifier "name")])])

    [<Test>]
    member x.``classifies an assignment expression correctly`` () =
        testSingleExpression "(set! say-hello (lambda () (display \"Hello!\")))"
        <| AssignmentExpression (Binding (Identifier "say-hello",
                                          LambdaExpression (MultiArgFormals [], [],
                                                            [ProcedureCallExpression (IdentifierExpression (Identifier "display"), [LiteralExpression (String "Hello!")])])))

    [<Test>]
    member x.``classifies a procedure call expression correctly`` () =
        testSingleExpression "(+ 5 2)"
        <| ProcedureCallExpression (IdentifierExpression (Identifier "+"), [LiteralExpression (Number 5.0); LiteralExpression (Number 2.0)])

    [<Test>]
    member x.``classifies a conditional expression correctly`` () =
        testSingleExpression "(if (pred?) 1 0)"
        <| ConditionalExpression (ProcedureCallExpression (IdentifierExpression (Identifier "pred?"), []),
                                  LiteralExpression (Number 1.0),
                                  Some (LiteralExpression (Number 0.0)))

    [<Test>]
    member x.``classifies a definition correctly`` () =
        testSingleExpression "(define pi 3.14159)"
        <| Definition (Binding (Identifier "pi", LiteralExpression (Number 3.14159)))

    [<Test>]
    member x.``classifies a begin block correctly`` () =
        testSingleExpression "(begin #t #f)"
        <| BeginExpression [(LiteralExpression (Boolean true))
                            (LiteralExpression (Boolean false))]

    [<Test>]
    member x.``classifies a boolean expression correctly`` () =
        testSingleExpression "(and #t #t)"
        <| BooleanExpression (AndExpression,
                              [(LiteralExpression (Boolean true))
                               (LiteralExpression (Boolean true))])
        testSingleExpression "(or #f #t)"
        <| BooleanExpression (OrExpression,
                              [(LiteralExpression (Boolean false))
                               (LiteralExpression (Boolean true))])


[<TestFixture>]
type ``Produces appropriate error messages when given a program with faulty semantics`` () =
    let testErrors str expectedErrors =
        ASTBuilderTest.build str |> should equal (ASTBuildFailure expectedErrors)

    let createNumArgsErrorList procName numLines =
        let msg = "Invalid number of arguments to " + procName
        [for x in 1L..numLines -> { message = msg; position = { line = x; column = 2L} }]

    [<Test>]
    member x.``produces an error when attempting to set! or shadow (define) a special built-in procedure`` () =
        let errors = [{ message = "Redefining built-in procedure define"; position = { line = 1L; column = 2L }}
                      { message = "Redefining built-in procedure set!"; position = { line = 2L; column = 2L }}
                      { message = "Redefining built-in procedure lambda"; position = { line = 3L; column = 2L }}
                      { message = "Redefining built-in procedure if"; position = { line = 4L; column = 2L }}]
        testErrors "(set! define #f)\n(set! set! #f)\n(set! lambda #f)\n(set! if #f)" errors
        testErrors "(define define #f)\n(define set! #f)\n(define lambda #f)\n(define if #f)" errors

    [<Test>]
    member x.``produces an error when wrong number of arguments given to define or set!`` () =
        testErrors "(define)\n(define foo 1 2)\n(define foo)" (createNumArgsErrorList "define" 3L)
        testErrors "(set!)\n(set! foo 1 2)\n(set! foo)" (createNumArgsErrorList "set!" 3L)

    [<Test>]
    member x.``produces an error when wrong number of arguments given to if`` () =
        testErrors "(if)\n(if foo)\n(if foo bar baz buzz)" (createNumArgsErrorList "if" 3L)

    [<Test>]
    member x.``produces an error when wrong number of arguments given to lambda`` () =
        testErrors "(lambda)" [{ message = "Invalid lambda syntax: missing arguments and body"; position = { line = 1L; column = 2L } }]
        testErrors "(lambda ())" [{ message = "Lambda body is empty"; position = { line = 1L; column = 2L }}]

    [<Test>]
    member x.``produces an error when lambda is missing an expression body`` () =
        testErrors "(lambda () (define answer 42))" [{ message = "Lambda body contains no expressions"; position = { line = 1L; column = 2L }}]

    [<Test>]
    member x.``produces an error when a begin block is empty`` () =
        testErrors "(begin)" [{ message = "Empty begin block"; position = { line = 1L; column = 2L }}]

    [<Test>]
    member x.``produces an error when a definition is used in an unexpected context`` () =
        let exprContextError = { message = "Procedure define used in a context where an expression was expected";
                                 position = { line = 1L; column = 2L } }
        testErrors "(define foo (define bar 1))" [exprContextError]
        testErrors "(if (define bar 1) #t #f)" [exprContextError]
        testErrors "(if #t (define bar 1) #f)" [exprContextError]
        testErrors "(if #t #f (define bar 1))" [exprContextError]
        testErrors "(lambda (x)\n(display x)\n(define x-add-1 (+ x 1))\nx-add-1)"
                   [{message = "Definitions must be in the beginning of the lambda body";
                     position = { line = 1L; column = 2L }}]
        testErrors "(begin (define pi 3.14159))"
                   [{message = "A begin block may not introduce new variables";
                     position = { line = 1L; column = 2L }}]
        testErrors "(and (define p 3) (set! a 2))"
                   [exprContextError]
        testErrors "(or (define p 3) (set! a 2))"
                   [exprContextError]

    // TODO: datum/literal printing in error messages!
    [<Test>]
    member x.``produces an error when attempting to call a non-procedure`` () =
        testErrors "(#f)" [{ message = "Not a procedure: CTBool false"; position = { line = 1L; column = 2L }}]
        testErrors "(1 2 3)" [{ message = "Not a procedure: CTNumber 1.0"; position = { line = 1L; column = 2L }}]

    [<Test>]
    member x.``produces an error when attempting to use a literal in place of an identifier`` () =
        testErrors "(define 42 0)" [{ message = "Not an identifier: LiteralExpression (Number 42.0)"; position = { line = 1L; column = 2L }}]
        testErrors "(set! #true 0)" [{ message = "Not an identifier: LiteralExpression (Boolean true)"; position = { line = 1L; column = 2L }}]
        testErrors "(lambda \"foo\" (display \"foo\"))" [{ message = "Invalid syntax in lambda expression"; position = { line = 1L; column = 9L }}]
        testErrors "(lambda (42) (display \"foo\"))" [{ message = "Invalid syntax in lambda expression"; position = { line = 1L; column = 9L }}]
