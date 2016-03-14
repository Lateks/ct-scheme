namespace CottontailSchemeTests

open NUnit.Framework
open FsUnit
open FParsec

open CottontailScheme
open CottontailScheme.Parsing

module ParserTest =
    let testEquals expected p str =
        match run p str with
        | Success(result, _, _) -> result |> should equal expected
        | Failure(errorMsg, _, _) -> Assert.Fail("Parser gave error: " + errorMsg)

    let testRejects p str =
        match run p str with
        | Success(result, _, _) -> Assert.Fail("Erroneously accepted " + str)
        | Failure(errorMsg, _, _) -> Assert.Pass()

[<TestFixture>]
type ``Boolean literal parser`` () =
    [<Test>]
    member x.``recognizes literal values representing true`` () =
        let testAccepts = ParserTest.testEquals (CTBool true) parseBoolean
        testAccepts "#true"
        testAccepts "#t"

    [<Test>]
    member x.``recognizes literal values representing false`` () =
        let testAccepts = ParserTest.testEquals (CTBool false) parseBoolean
        testAccepts "#false"
        testAccepts "#f"

    [<Test>]
    member x.``rejects other values`` () =
        let testRejects = ParserTest.testRejects parseBoolean
        testRejects "#False"
        testRejects "#"
        testRejects "true"
        testRejects "42"
        testRejects ""
