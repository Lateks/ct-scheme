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
        | Success(result, _, _) -> Assert.Fail("Erroneously accepted " + str)
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
