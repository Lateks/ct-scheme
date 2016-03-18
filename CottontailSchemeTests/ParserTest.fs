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
        testRejects "('foo)" // TODO?
        testRejects "(foo bar"
        testRejects "(#foo)"

[<TestFixture>]
type ``Parsing quotations`` () =
    [<Test>]
    member x.``parses syntax sugared quotations`` () =
        let testParsesAs = ParserTest.testEquals parseExpression
        testParsesAs (CTLiteralExpression (CTList [])) "'()"
        testParsesAs (CTLiteralExpression (CTNumber 3.14159)) "'+3.14159"
        testParsesAs (CTLiteralExpression (CTString "Hello, world!")) "'\"Hello, world!\""
        testParsesAs (CTLiteralExpression (CTBool true)) "'#true"
        testParsesAs (CTLiteralExpression (CTSymbol "call-with-current-continuation")) "'call-with-current-continuation"
        testParsesAs (CTLiteralExpression (CTSymbol "+")) "'+"

[<TestFixture>]
type ``List expression parser`` () =
    let testParsesAs = ParserTest.testEquals parseExpression

    [<Test>]
    member x.``accepts nonempty lists of expressions`` () =
        testParsesAs (CTListExpression [CTIdentifierExpression "if"
                                        CTLiteralExpression (CTBool true)
                                        CTListExpression [CTIdentifierExpression "display"; CTLiteralExpression (CTString "Hello")]
                                        CTLiteralExpression (CTNumber 42.0)]) "(if #t (display \"Hello\") 42)"

    [<Test>]
    member x.``accepts empty lists of expressions`` () =
        testParsesAs (CTListExpression []) "()"

[<TestFixture>]
type ``Whole program parser`` () =
    let testParsesAs = ParserTest.testEquals parseProgram

    [<Test>]
    member x.``parses a small program`` () =
        let program = "(define pi 3.14159) ; the definition of pi\n"+
                      "(define circle-area (lambda (r) (* pi (* r r)))) ; area of circle = pi * r^2\n"+
                      "(display (circle-area 5))\n"+
                      "42"
        let expectedParse = CTProgram [CTListExpression [CTIdentifierExpression "define"
                                                         CTIdentifierExpression "pi"
                                                         CTLiteralExpression (CTNumber 3.14159)]
                                       CTListExpression [CTIdentifierExpression "define"
                                                         CTIdentifierExpression "circle-area"
                                                         CTListExpression [CTIdentifierExpression "lambda"
                                                                           CTListExpression [CTIdentifierExpression "r"]
                                                                           CTListExpression [CTIdentifierExpression "*"
                                                                                             CTIdentifierExpression "pi"
                                                                                             CTListExpression [CTIdentifierExpression "*"
                                                                                                               CTIdentifierExpression "r"
                                                                                                               CTIdentifierExpression "r"]]]]
                                       CTListExpression [CTIdentifierExpression "display"
                                                         CTListExpression [CTIdentifierExpression "circle-area"
                                                                           CTLiteralExpression (CTNumber 5.0)]]
                                       CTLiteralExpression (CTNumber 42.0)]
        testParsesAs expectedParse program

