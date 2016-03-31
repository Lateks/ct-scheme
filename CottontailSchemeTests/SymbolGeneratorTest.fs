module SymbolGeneratorTest

open NUnit.Framework
open FsUnit
open CottontailScheme.SymbolGenerator

[<TestFixture>]
type ``Symbol generation`` () =
    let getConvertedName (name : string) = name.Split [|'$'|] |> Array.head

    let shouldBeNamed (expected : string) =
        getConvertedName >> should equal expected

    [<Test>]
    member x.``replaces special characters`` () =
        let gen = SymbolGenerator()
        gen.generateSymbol "*" |> shouldBeNamed "Star"
        gen.generateSymbol "+something" |> shouldBeNamed "PlusSomething"
        gen.generateSymbol "something+" |> shouldBeNamed "SomethingPlus"
        gen.generateSymbol "+!$%&*/:<=>?^~" |> shouldBeNamed "PlusExclamationDollarPercentAndStarDivColonLtEqGtQuestionHatTilde"
        gen.generateSymbol "-" |> shouldBeNamed "Minus"

    [<Test>]
    member x.``underscores are accepted`` () =
        let gen = SymbolGenerator()
        gen.generateSymbol "foo_bar" |> shouldBeNamed "Foo_bar"

    [<Test>]
    member x.``kebab case is converted to capitalized camel case`` () =
        let gen = SymbolGenerator ()
        gen.generateSymbol "get-counter" |> shouldBeNamed "GetCounter"
        gen.generateSymbol "call-with-current-continuation" |> shouldBeNamed "CallWithCurrentContinuation"

    [<Test>]
    member x.``exclamation mark in mutator name is replaced with prefix "do"`` () =
        let gen = SymbolGenerator()
        gen.generateSymbol "set-counter!" |> shouldBeNamed "DoSetCounter"
        gen.generateSymbol "set-counter!!" |> shouldBeNamed "DoSetCounterExclamation"

    [<Test>]
    member x.``question mark in predicate name is replaced with prefix "is"`` () =
        let gen = SymbolGenerator()
        gen.generateSymbol "water-cooler?" |> shouldBeNamed "IsWaterCooler"
        gen.generateSymbol "water-cooler??" |> shouldBeNamed "IsWaterCoolerQuestion"

    [<Test>]
    member x.``removes extra dashes in kebab case identifier`` () =
        let gen = SymbolGenerator()
        gen.generateSymbol "foo--bar--baz" |> shouldBeNamed "FooBarBaz"

    [<Test>]
    member x.``replaces extra dashes at beginning or end with "minus"`` () =
        let gen = SymbolGenerator()
        gen.generateSymbol "-5" |> shouldBeNamed "Minus5"
        gen.generateSymbol "-test-" |> shouldBeNamed "MinusTestMinus"
        gen.generateSymbol "--c" |> shouldBeNamed "MinusMinusC"
        gen.generateSymbol "---" |> shouldBeNamed "MinusMinusMinus"

    [<Test>]
    member x.``generates a unique name each time`` () =
        let gen = SymbolGenerator()
        gen.generateSymbol "test" |> should not' (equal (gen.generateSymbol "test"))
        gen.generateSymbol "is-something" |> should not' (equal (gen.generateSymbol "something?"))
        gen.generateSymbol "-" |> should not' (equal (gen.generateSymbol "-"))
        gen.generateSymbol "CallWithCurrentContinuation" |> should not' (equal (gen.generateSymbol "call-with-current-continuation"))
        gen.generateSymbol "foo----bar" |> should not' (equal (gen.generateSymbol "foo-bar"))

    [<Test>]
    member x.``avoids name clashes with reserved words`` () =
        let gen = SymbolGenerator()
        gen.generateSymbol "delegate" |> should not' (equal "delegate")
        gen.generateSymbol "class" |> should not' (equal "class")
