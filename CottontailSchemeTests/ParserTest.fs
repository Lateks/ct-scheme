namespace CottontailSchemeTests

open NUnit.Framework
open FsUnit
open FParsec

open CottontailScheme.Parsing
open CottontailScheme.Literals

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
        let testAccepts = ParserTest.testEquals parseDatum (Boolean true)
        testAccepts "#true"
        testAccepts "#t"

    [<Test>]
    member x.``accepts literal values representing false`` () =
        let testAccepts = ParserTest.testEquals parseDatum (Boolean false)
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
        testParsesAs (Number 3.14159) "3.14159"
        testParsesAs (Number 42.0) "42"
        testParsesAs (Number 9.5) "+9.5"
        testParsesAs (Number -54.2) "-54.2"
        testParsesAs (Number 0.5) "0.5"

    [<Test>]
    member x.``rejects invalid numbers`` () =
        let testRejects = ParserTest.testRejects parseExpression
        testRejects ".59"
        testParsesAs (Number 0.0) "0xFF"
        testParsesAs (Number 0.2) "0.2e10"

[<TestFixture>]
type ``String literal parser`` () =
    let testParsesAs = ParserTest.testEquals parseDatum

    [<Test>]
    member x.``accepts well formed string literals with escapes`` () =
        testParsesAs (String "Hello, world!") "\"Hello, world!\""
        testParsesAs (String "foo\nbar\tbaz\r") "\"foo\\nbar\\tbaz\\r\""
        testParsesAs (String "") "\"\""

    [<Test>]
    member x.``rejects invalid strings`` () =
        let testRejects = ParserTest.testRejects parseExpression
        testRejects ""
        testRejects "\"Hello"
        testParsesAs (String "She said, ") "\"She said, \"hello\"\""

[<TestFixture>]
type ``Symbol parser`` () =
    let testParsesAs = ParserTest.testEquals parseDatum

    [<Test>]
    member x.``accepts well formed basic symbols`` () =
        testParsesAs (Symbol "fib") "fib"
        testParsesAs (Symbol "call-with-current-continuation") "call-with-current-continuation"
        testParsesAs (Symbol "+") "+"
        testParsesAs (Symbol "-") "-"
        testParsesAs (Symbol "<") "<"
        testParsesAs (Symbol ">") ">"
        testParsesAs (Symbol "set!") "set!"
        testParsesAs (Symbol "add5") "add5"
        testParsesAs (Symbol "-one") "-one"
        testParsesAs (Symbol "foo_bar") "foo_bar"
        testParsesAs (Symbol "odd?") "odd?"

    [<Test>]
    member x.``rejects ill formed symbols`` () =
        let testRejects = ParserTest.testRejects parseDatum
        testRejects ""
        testParsesAs (Symbol "x") "x'"

[<TestFixture>]
type ``List parser`` () =
    let testParsesAs = ParserTest.testEquals parseDatum

    [<Test>]
    member x.``accepts well formed lists of datum objects (including recursive lists)`` () =
        testParsesAs (List []) "()"
        testParsesAs (List [Number 1.0; Number 2.0; Number 3.0]) "(1 2 3)"
        testParsesAs (List [Symbol "+"; Number 1.0; Number 2.0]) "(+ 1 2)"
        testParsesAs (List [Boolean true; Boolean false]) "(#t #f)"
        testParsesAs (List [List [Number 1.0; Number 2.0]; List [Symbol "fib"; Number 42.0]]) "((1 2) (fib 42))"
        testParsesAs (List [List [List [List []]]]) "(((())))"

    [<Test>]
    member x.``allows extra whitespace between elements`` () =
        testParsesAs (List [Number 1.0; Number 2.0; Number 3.0]) "(\n\r\t 1\n\r\t 2\n\r\t 3\n\r\t )"

    [<Test>]
    member x.``allows single line comments between elements`` () =
        testParsesAs (List [Number 1.0; Number 2.0; Number 3.0]) "(; starting list\n\t1 ; first element \n\t2 ; second element \n\t3 ; third element\n\t)"

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
        testParsesAs (CTLiteralExpression (startPosition, List [])) "'()"
        testParsesAs (CTLiteralExpression (startPosition, Number 3.14159)) "'+3.14159"
        testParsesAs (CTLiteralExpression (startPosition, String "Hello, world!")) "'\"Hello, world!\""
        testParsesAs (CTLiteralExpression (startPosition, Boolean true)) "'#true"
        testParsesAs (CTLiteralExpression (startPosition, Symbol "call-with-current-continuation")) "'call-with-current-continuation"
        testParsesAs (CTLiteralExpression (startPosition, Symbol "+")) "'+"

[<TestFixture>]
type ``List expression parser`` () =
    let testParsesAs = ParserTest.testEquals parseExpression

    [<Test>]
    member x.``accepts nonempty lists of expressions`` () =
        testParsesAs (CTListExpression ({ line = 1L; column = 1L },
                                        [CTIdentifierExpression ({ line = 1L; column = 2L }, "if")
                                         CTLiteralExpression ({ line = 1L; column = 5L }, Boolean true)
                                         CTListExpression ({ line = 1L; column = 8L },
                                                           [CTIdentifierExpression ({ line = 1L; column = 9L}, "display");
                                                            CTLiteralExpression ({ line = 1L; column = 17L }, String "Hello")])
                                         CTLiteralExpression ({ line = 1L; column = 26L}, Number 42.0)])) "(if #t (display \"Hello\") 42)"

    [<Test>]
    member x.``accepts empty lists of expressions`` () =
        testParsesAs (CTListExpression ({ line = 1L; column = 1L }, [])) "()"

    [<Test>]
    member x.``parses quotations as datum objects`` () =
        testParsesAs (CTLiteralExpression
                        ({ line = 1L; column = 1L },
                         List [Number 1.0; Number 2.0; Number 3.0]))
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
                                    CTLiteralExpression ({ line = 1L; column = 12L }, Number 3.14159)])
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
                                                       CTLiteralExpression ({ line = 5L; column = 23L }, Number 5.0)])])
                 CTLiteralExpression ({ line = 6L; column = 1L }, Number 42.0)]
        testParsesAs expectedParse program

