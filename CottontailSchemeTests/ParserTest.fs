namespace CottontailSchemeTests

open NUnit.Framework
open FsUnit
open FParsec

open CottontailScheme.Parsing

module ParserTest =
    let testEquals p expected str =
        match run p str with
        | Success(result, _, _) -> result |> should equal expected
        | Failure(errorMsg, _, _) -> Assert.Fail("Parser gave error: " + errorMsg)

    let testRejects p str =
        match run p str with
        | Success(result, _, _) -> Assert.Fail("Erroneously accepted \"" + str + "\"")
        | Failure(errorMsg, _, _) -> ()

[<TestFixture>]
type ``Boolean literal parser`` () =
    [<Test>]
    member x.``accepts literal values representing true`` () =
        let testAccepts = ParserTest.testEquals parseDatum (CTBool true)
        testAccepts "#true"
        testAccepts "#t"

    [<Test>]
    member x.``accepts literal values representing false`` () =
        let testAccepts = ParserTest.testEquals parseDatum (CTBool false)
        testAccepts "#false"
        testAccepts "#f"

    [<Test>]
    member x.``rejects other inputs`` () =
        let testRejects = ParserTest.testRejects parseDatum
        testRejects "#False"
        testRejects "#"

[<TestFixture>]
type ``Number parser`` () =
    let testParsesAs = ParserTest.testEquals parseDatum

    [<Test>]
    member x.``accepts floating point and integer values with or without signs`` () =
        testParsesAs (CTNumber 3.14159) "3.14159"
        testParsesAs (CTNumber 42.0) "42"
        testParsesAs (CTNumber 9.5) "+9.5"
        testParsesAs (CTNumber -54.2) "-54.2"
        testParsesAs (CTNumber 0.5) "0.5"

    [<Test>]
    member x.``rejects invalid numbers`` () =
        let testRejects = ParserTest.testRejects parseExpression
        testRejects ".59"
        testParsesAs (CTNumber 0.0) "0xFF"
        testParsesAs (CTNumber 0.2) "0.2e10"

[<TestFixture>]
type ``String literal parser`` () =
    let testParsesAs = ParserTest.testEquals parseDatum

    [<Test>]
    member x.``accepts well formed string literals with escapes`` () =
        testParsesAs (CTString "Hello, world!") "\"Hello, world!\""
        testParsesAs (CTString "foo\nbar\tbaz\r") "\"foo\\nbar\\tbaz\\r\""
        testParsesAs (CTString "") "\"\""

    [<Test>]
    member x.``rejects invalid strings`` () =
        let testRejects = ParserTest.testRejects parseExpression
        testRejects ""
        testRejects "\"Hello"
        testParsesAs (CTString "She said, ") "\"She said, \"hello\"\""

[<TestFixture>]
type ``Symbol parser`` () =
    let testParsesAs = ParserTest.testEquals parseDatum

    [<Test>]
    member x.``accepts well formed basic symbols`` () =
        testParsesAs (CTSymbol "fib") "fib"
        testParsesAs (CTSymbol "call-with-current-continuation") "call-with-current-continuation"
        testParsesAs (CTSymbol "+") "+"
        testParsesAs (CTSymbol "-") "-"
        testParsesAs (CTSymbol "<") "<"
        testParsesAs (CTSymbol ">") ">"
        testParsesAs (CTSymbol "set!") "set!"
        testParsesAs (CTSymbol "add5") "add5"
        testParsesAs (CTSymbol "-one") "-one"
        testParsesAs (CTSymbol "foo_bar") "foo_bar"
        testParsesAs (CTSymbol "odd?") "odd?"

    [<Test>]
    member x.``rejects ill formed symbols`` () =
        let testRejects = ParserTest.testRejects parseDatum
        testRejects ""
        testParsesAs (CTSymbol "x") "x'"

[<TestFixture>]
type ``List parser`` () =
    let testParsesAs = ParserTest.testEquals parseDatum

    [<Test>]
    member x.``accepts well formed lists of datum objects (including recursive lists)`` () =
        testParsesAs (CTList []) "()"
        testParsesAs (CTList [CTNumber 1.0; CTNumber 2.0; CTNumber 3.0]) "(1 2 3)"
        testParsesAs (CTList [CTSymbol "+"; CTNumber 1.0; CTNumber 2.0]) "(+ 1 2)"
        testParsesAs (CTList [CTBool true; CTBool false]) "(#t #f)"
        testParsesAs (CTList [CTList [CTNumber 1.0; CTNumber 2.0]; CTList [CTSymbol "fib"; CTNumber 42.0]]) "((1 2) (fib 42))"
        testParsesAs (CTList [CTList [CTList [CTList []]]]) "(((())))"

    [<Test>]
    member x.``allows extra whitespace between elements`` () =
        testParsesAs (CTList [CTNumber 1.0; CTNumber 2.0; CTNumber 3.0]) "(\n\r\t 1\n\r\t 2\n\r\t 3\n\r\t )"

    [<Test>]
    member x.``allows single line comments between elements`` () =
        testParsesAs (CTList [CTNumber 1.0; CTNumber 2.0; CTNumber 3.0]) "(; starting list\n\t1 ; first element \n\t2 ; second element \n\t3 ; third element\n\t)"

    [<Test>]
    member x.``rejects ill formed lists and lists of invalid datum objects`` () =
        let testRejects = ParserTest.testRejects parseDatum
        testRejects "('foo)"
        testRejects "(foo bar"
        testRejects "(#foo)"

[<TestFixture>]
type ``Parsing quotations`` () =
    [<Test>]
    member x.``parses syntax sugared quotations`` () =
        let testParsesAs = ParserTest.testEquals parseExpression
        let startPosition = {line = 1L; column = 1L}
        testParsesAs (CTLiteralExpression (startPosition, CTList [])) "'()"
        testParsesAs (CTLiteralExpression (startPosition, CTNumber 3.14159)) "'+3.14159"
        testParsesAs (CTLiteralExpression (startPosition, CTString "Hello, world!")) "'\"Hello, world!\""
        testParsesAs (CTLiteralExpression (startPosition, CTBool true)) "'#true"
        testParsesAs (CTLiteralExpression (startPosition, CTSymbol "call-with-current-continuation")) "'call-with-current-continuation"
        testParsesAs (CTLiteralExpression (startPosition, CTSymbol "+")) "'+"

[<TestFixture>]
type ``List expression parser`` () =
    let testParsesAs = ParserTest.testEquals parseExpression

    [<Test>]
    member x.``accepts nonempty lists of expressions`` () =
        testParsesAs (CTListExpression ({ line = 1L; column = 1L },
                                        [CTIdentifierExpression ({ line = 1L; column = 2L }, "if")
                                         CTLiteralExpression ({ line = 1L; column = 5L }, CTBool true)
                                         CTListExpression ({ line = 1L; column = 8L },
                                                           [CTIdentifierExpression ({ line = 1L; column = 9L}, "display");
                                                            CTLiteralExpression ({ line = 1L; column = 17L }, CTString "Hello")])
                                         CTLiteralExpression ({ line = 1L; column = 26L}, CTNumber 42.0)])) "(if #t (display \"Hello\") 42)"

    [<Test>]
    member x.``accepts empty lists of expressions`` () =
        testParsesAs (CTListExpression ({ line = 1L; column = 1L }, [])) "()"

    [<Test>]
    member x.``parses quotations as datum objects`` () =
        testParsesAs (CTLiteralExpression
                        ({ line = 1L; column = 1L },
                         CTList [CTNumber 1.0; CTNumber 2.0; CTNumber 3.0]))
                     "(quote (1 2 3))"


[<TestFixture>]
type ``Whole program parser`` () =
    let testParsesAs = ParserTest.testEquals parseProgram

    [<Test>]
    member x.``parses a small program`` () =
        let program = "(define pi 3.14159) ; the definition of pi\n"+
                      "(define circle-area\n"+
                      "  (lambda (r)\n"+
                      "    (* pi (* r r)))) ; area of circle = pi * r^2\n"+
                      "(display (circle-area 5))\n"+
                      "42\n"
        let expectedParse =
            CTProgram
                [CTListExpression ({ line = 1L; column = 1L },
                                   [CTIdentifierExpression ({ line = 1L; column = 2L}, "define")
                                    CTIdentifierExpression ({ line = 1L; column = 9L}, "pi")
                                    CTLiteralExpression ({ line = 1L; column = 12L }, CTNumber 3.14159)])
                 CTListExpression ({ line = 2L; column = 1L },
                                   [CTIdentifierExpression ({ line = 2L; column = 2L }, "define")
                                    CTIdentifierExpression ({ line = 2L; column = 9L }, "circle-area")
                                    CTListExpression ({ line = 3L; column = 3L},
                                                      [CTIdentifierExpression ({ line = 3L; column = 4L }, "lambda")
                                                       CTListExpression ({ line = 3L; column = 11L},
                                                                         [CTIdentifierExpression ({ line = 3L; column = 12L }, "r")])
                                                       CTListExpression ({ line = 4L; column = 5L },
                                                                         [CTIdentifierExpression ({ line = 4L; column = 6L }, "*")
                                                                          CTIdentifierExpression ({ line = 4L; column = 8L }, "pi")
                                                                          CTListExpression ({ line = 4L; column = 11L },
                                                                                            [CTIdentifierExpression ({ line = 4L; column = 12L }, "*")
                                                                                             CTIdentifierExpression ({ line = 4L; column = 14L }, "r")
                                                                                             CTIdentifierExpression ({ line = 4L; column = 16L}, "r")])])])])
                 CTListExpression ({ line = 5L; column = 1L },
                                   [CTIdentifierExpression ({ line = 5L; column = 2L }, "display")
                                    CTListExpression ({ line = 5L; column = 10L },
                                                      [CTIdentifierExpression ({ line = 5L; column = 11L }, "circle-area")
                                                       CTLiteralExpression ({ line = 5L; column = 23L }, CTNumber 5.0)])])
                 CTLiteralExpression ({ line = 6L; column = 1L }, CTNumber 42.0)]
        testParsesAs expectedParse program

