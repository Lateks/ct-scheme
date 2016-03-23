module ASTBuilderTest

open NUnit.Framework
open FsUnit
open FParsec

open CottontailScheme.ASTBuilder

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
                                (IdentifierExpression (Identifier "display"), [StringLiteral "Hello, world!"])])

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
                                                                                            NumberLiteral 0.0,
                                                                                            Some (ProcedureCallExpression (IdentifierExpression (Identifier "/"),
                                                                                                                           [IdentifierExpression (Identifier "n")
                                                                                                                            ProcedureCallExpression (IdentifierExpression (Identifier "length"),
                                                                                                                                                     [IdentifierExpression (Identifier "lst")])])))])))
                             ProcedureCallExpression (IdentifierExpression (Identifier "display"),
                                                      [ProcedureCallExpression (IdentifierExpression (Identifier "average"),
                                                                                [NumberLiteral 1.0;
                                                                                 NumberLiteral 2.0;
                                                                                 NumberLiteral 3.0;
                                                                                 NumberLiteral 4.0;
                                                                                 NumberLiteral 5.0;])])])

[<TestFixture>]
type ``Classifies different constructs correctly`` () =
    let testSingleExpression str expectedExpr =
        ASTBuilderTest.build str |> should equal (ASTBuildSuccess [expectedExpr])

    [<Test>]
    member x.``classifies a numeric literal expression correctly`` () =
        testSingleExpression "42" (NumberLiteral 42.0)
        testSingleExpression "3.14159" (NumberLiteral 3.14159)

    [<Test>]
    member x.``classifies a boolean literal expression correctly`` () =
        testSingleExpression "#t" (BooleanLiteral true)
        testSingleExpression "#f" (BooleanLiteral false)

    [<Test>]
    member x.``classifies a (quoted) symbol literal expression correctly`` () =
        testSingleExpression "'+" (SymbolLiteral "+")
        testSingleExpression "(quote +)" (SymbolLiteral "+")

    [<Test>]
    member x.``classifies a (quoted) list literal expression correctly`` () =
        let expectedExpression = ListLiteral [NumberLiteral 1.0; NumberLiteral 2.0; NumberLiteral 3.0]
        testSingleExpression "'(1 2 3)" expectedExpression
        testSingleExpression "(quote (1 2 3))" expectedExpression

    [<Test>]
    member x.``classifies a string literal expression correctly`` () =
        testSingleExpression "\"Hello, world!\"" (StringLiteral "Hello, world!")