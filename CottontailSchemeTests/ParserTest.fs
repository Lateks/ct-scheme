namespace CottontailSchemeTests

open NUnit.Framework
open FsUnit
open FParsec

open CottontailScheme
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
        let testAccepts = ParserTest.testEquals parseBoolean (CTBool true)
        testAccepts "#true"
        testAccepts "#t"

    [<Test>]
    member x.``accepts literal values representing false`` () =
        let testAccepts = ParserTest.testEquals parseBoolean (CTBool false)
        testAccepts "#false"
        testAccepts "#f"

    [<Test>]
    member x.``rejects other inputs`` () =
        let testRejects = ParserTest.testRejects parseBoolean
        testRejects "#False"
        testRejects "#"
        testRejects "true"
        testRejects "42"
        testRejects ""

[<TestFixture>]
type ``Number parser`` () =
    [<Test>]
    member x.``accepts floating point and integer values with or without signs`` () =
        let testParsesAs = ParserTest.testEquals parseNumber
        testParsesAs (CTNumber 3.14159) "3.14159"
        testParsesAs (CTNumber 42.0) "42"
        testParsesAs (CTNumber 9.5) "+9.5"
        testParsesAs (CTNumber -54.2) "-54.2"
        testParsesAs (CTNumber 0.5) "0.5"

    [<Test>]
    member x.``rejects invalid numbers`` () =
        let testRejects = ParserTest.testRejects parseNumber
        let testParsesAs = ParserTest.testEquals parseNumber
        testRejects ""
        testRejects ".59"
        testRejects "AB"
        testParsesAs (CTNumber 0.0) "0xFF"
        testParsesAs (CTNumber 0.2) "0.2e10"

[<TestFixture>]
type ``String literal parser`` () =
    [<Test>]
    member x.``accepts well formed string literals with escapes`` () =
        let testParsesAs = ParserTest.testEquals parseStringLiteral
        testParsesAs (CTString "Hello, world!") "\"Hello, world!\""
        testParsesAs (CTString "foo\nbar\tbaz\r") "\"foo\\nbar\\tbaz\\r\""
        testParsesAs (CTString "") "\"\""

    [<Test>]
    member x.``rejects invalid strings`` () =
        let testRejects = ParserTest.testRejects parseStringLiteral
        let testParsesAs = ParserTest.testEquals parseStringLiteral
        testRejects "\"Hello"
        testRejects "15"
        testRejects "Hello"
        testParsesAs (CTString "She said, ") "\"She said, \"hello\"\""

[<TestFixture>]
type ``Symbol parser`` () =
    [<Test>]
    member x.``accepts well formed basic symbols`` () =
        let testParsesAs = ParserTest.testEquals parseSymbol
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
        let testRejects = ParserTest.testRejects parseSymbol
        let testParsesAs = ParserTest.testEquals parseSymbol
        testRejects ""
        testRejects "42"
        testRejects "1+"
        testParsesAs (CTSymbol "+") "+1"
        testParsesAs (CTSymbol "x") "x'"

[<TestFixture>]
type ``List parser`` () =
    let testParsesAs = ParserTest.testEquals parseList

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
        let testRejects = ParserTest.testRejects parseList
        testRejects ""
        testRejects "('foo)"
        testRejects "(foo bar"
        testRejects "foo bar)"
        testRejects "(#foo)"

[<TestFixture>]
type ``Datum parser`` () =
    let testParsesAs = ParserTest.testEquals parseDatum

    [<Test>]
    member x.``accepts all kinds of datum objects`` () =
        testParsesAs (CTList []) "()"
        testParsesAs (CTNumber 3.14159) "+3.14159"
        testParsesAs (CTString "Hello, world!") "\"Hello, world!\""
        testParsesAs (CTBool true) "#true"
        testParsesAs (CTSymbol "call-with-current-continuation") "call-with-current-continuation"
        testParsesAs (CTSymbol "+") "+"

    [<Test>]
    member x.``rejects invalid datum objects`` () =
        let testRejects = ParserTest.testRejects parseDatum
        testRejects ""
        testRejects "#bar"
        testRejects "(foo"
        testParsesAs (CTNumber 1.0) "1+"


[<TestFixture>]
type ``Parsing quotations`` () =
    [<Test>]
    member x.``parses syntax sugared quotations`` () =
        let testParsesAs = ParserTest.testEquals parseSugaredQuotation
        testParsesAs (CTList []) "'()"
        testParsesAs (CTNumber 3.14159) "'+3.14159"
        testParsesAs (CTString "Hello, world!") "'\"Hello, world!\""
        testParsesAs (CTBool true) "'#true"
        testParsesAs (CTSymbol "call-with-current-continuation") "'call-with-current-continuation"
        testParsesAs (CTSymbol "+") "'+"

    [<Test>]
    member x.``rejects pieces of code that are not quotations`` () =
        let testRejects = ParserTest.testRejects parseSugaredQuotation
        testRejects ""
        testRejects "()"
        testRejects "foo"