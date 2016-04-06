module LibTest

open System
open NUnit.Framework
open FsUnit
open CottontailSchemeLib

[<TestFixture>]
type ``List and pair operations`` () =
    let car = CTNumber(1.0)
    let cdr = CTNumber(2.0)
    let p = CTPair(car, cdr)
    let empty = CTEmptyList();
    let l = CTPair(car, CTPair(cdr, CTEmptyList()));

    [<Test>]
    member x.``list equality`` () =
       let l2 = CTPair(car, CTPair(cdr, CTEmptyList()));
       l = l2 |> should equal true
       l = p |> should equal false

    [<Test>]
    member x.``implements car`` () =
        ListOperations.Car(p) |> should equal car

    [<Test>]
    [<ExpectedException(typeof<TypeError>)>]
    member x.``car checks parameter type`` () =
        ListOperations.Car(car) |> ignore

    [<Test>]
    member x.``implements cdr`` () =
        ListOperations.Cdr(p) |> should equal cdr

    [<Test>]
    [<ExpectedException(typeof<TypeError>)>]
    member x.``cdr checks parameter type`` () =
        ListOperations.Cdr(car) |> ignore

    [<Test>]
    member x.``implements cons`` () =
        ListOperations.Cons(car, cdr) |> should equal p

    [<Test>]
    member x.``implements null?`` () =
        ListOperations.IsNull(p) |> should equal false
        ListOperations.IsNull(empty) |> should equal true
        ListOperations.IsNull(car) |> should equal false

    [<Test>]
    member x.``implements list`` () =
        let members : CTObject array = [|car; cdr|]
        ListOperations.List(members) |> should equal l
        ListOperations.List([||]) |> should equal (CTEmptyList())

[<TestFixture>]
type ``numeric operations`` () =
    [<Test>]
    member x.``addition`` () =
        NumberOperations.Plus([||]) |> should equal (CTNumber(0.0))
        NumberOperations.Plus([|CTNumber(5.0); CTNumber(10.0); CTNumber(9.0)|]) |> should equal (CTNumber(24.0))
        NumberOperations.Plus([|CTNumber(6.0); CTNumber(-1.0)|]) |> should equal (CTNumber(5.0))
        NumberOperations.Plus([|CTNumber(3.5); CTNumber(2.4)|]) |> should equal (CTNumber(5.9))

    [<Test>]
    [<ExpectedException(typeof<TypeError>)>]
    member x.``addition checks parameter types`` () =
        NumberOperations.Plus([|CTNumber(3.14159); Constants.False|]) |> ignore

    [<Test>]
    member x.``subtraction`` () =
        NumberOperations.Minus([|CTNumber(5.0); CTNumber(2.0)|]) |> should equal (CTNumber(3.0))
        NumberOperations.Minus([|CTNumber(10.0); CTNumber(15.0); CTNumber(3.5)|]) |> should equal (CTNumber(-8.5))
        NumberOperations.Minus([|CTNumber(1.0)|]) |> should equal (CTNumber(-1.0))

    [<Test>]
    [<ExpectedException(typeof<TypeError>)>]
    member x.``subtraction checks parameter types`` () =
        NumberOperations.Minus([|CTString("foo")|]) |> ignore

    [<Test>]
    member x.``division`` () =
        NumberOperations.Div([|CTNumber(5.0)|]) |> should equal (CTNumber(1.0/5.0))
        NumberOperations.Div([|CTNumber(5.0); CTNumber(2.0)|]) |> should equal (CTNumber(2.5))
        NumberOperations.Div([|CTNumber(6.0); CTNumber(2.0); CTNumber(2.0)|]) |> should equal (CTNumber(1.5))

    [<Test>]
    member x.``division by zero`` () =
        NumberOperations.Div([|CTNumber(5.0); CTNumber(0.0)|]) |> should equal (CTNumber(Double.PositiveInfinity))

    [<Test>]
    [<ExpectedException(typeof<TypeError>)>]
    member x.``division checks parameter types`` () =
        NumberOperations.Div([|CTString("error")|]) |> ignore

    [<Test>]
    member x.``multiplication`` () =
        NumberOperations.Mult([||]) |> should equal (CTNumber(1.0))
        NumberOperations.Mult([|CTNumber(5.0)|]) |> should equal (CTNumber(5.0))
        NumberOperations.Mult([|CTNumber(2.0); CTNumber(3.0); CTNumber(4.0)|]) |> should equal (CTNumber(24.0))

    [<Test>]
    [<ExpectedException(typeof<TypeError>)>]
    member x.``multiplication checks parameter types`` () =
        NumberOperations.Mult([|CTString("error")|]) |> ignore

    [<Test>]
    member x.``zero?`` () =
        NumberOperations.IsZero(CTNumber(0.0)) |> should equal Constants.True
        NumberOperations.IsZero(CTNumber(-0.01)) |> should equal Constants.False
        NumberOperations.IsZero(CTNumber(0.01)) |> should equal Constants.False

    [<Test>]
    [<ExpectedException(typeof<TypeError>)>]
    member x.``zero? checks parameter type`` () =
        NumberOperations.IsZero(CTString("error")) |> ignore

    [<Test>]
    member x.``less than (<)`` () =
        NumberOperations.LessThan([|CTNumber(1.0); CTNumber(0.0)|]) |> should equal false
        NumberOperations.LessThan([|CTNumber(1.0); CTNumber(2.0)|]) |> should equal true
        NumberOperations.LessThan([|CTNumber(1.0); CTNumber(2.0); CTNumber(3.0)|]) |> should equal true
        NumberOperations.LessThan([|CTNumber(1.0); CTNumber(2.0); CTNumber(1.5)|]) |> should equal false

    [<Test>]
    [<ExpectedException(typeof<TypeError>)>]
    member x.``< checks parameter types`` () =
        NumberOperations.LessThan([|CTNumber(1.0); Constants.True|]) |> ignore

    [<Test>]
    member x.``greater than (>)`` () =
        NumberOperations.GreaterThan([|CTNumber(2.0); CTNumber(1.0)|]) |> should equal true
        NumberOperations.GreaterThan([|CTNumber(1.0); CTNumber(2.0)|]) |> should equal false
        NumberOperations.GreaterThan([|CTNumber(3.0); CTNumber(2.0); CTNumber(1.0)|]) |> should equal true
        NumberOperations.GreaterThan([|CTNumber(3.0); CTNumber(2.0); CTNumber(2.5)|]) |> should equal false

    [<Test>]
    [<ExpectedException(typeof<TypeError>)>]
    member x.``> checks parameter types`` () =
        NumberOperations.GreaterThan([|CTNumber(1.0); Constants.True|]) |> ignore

[<TestFixture>]
type ``printing objects`` () =
    [<Test>]
    member x.``printing integers`` () =
        CTNumber(0.0).ToString() |> should equal "0"
        CTNumber(3.0).ToString() |> should equal "3"
        CTNumber(10000.0).ToString() |> should equal "10000"

    [<Test>]
    member x.``printing floating point numbers`` () =
        CTNumber(3.14159).ToString() |> should equal "3.14159"
        CTNumber(5.0/2.0).ToString() |> should equal "2.5"

    [<Test>]
    member x.``printing boolean values`` () =
        Constants.True.ToString() |> should equal "#t"
        Constants.False.ToString() |> should equal "#f"

    [<Test>]
    member x.``printing an empty list`` () =
        CTEmptyList().ToString() |> should equal "()"

    [<Test>]
    member x.``printing a pair`` () =
        CTPair(CTNumber(1.0), CTNumber(2.0)).ToString() |> should equal "(1 . 2)"
        CTPair(Constants.False, CTNumber(3.14159)).ToString() |> should equal "(#f . 3.14159)"

    [<Test>]
    member x.``printing a list`` () =
        CTPair(Constants.True, CTEmptyList()).ToString() |> should equal "(#t)"
        CTPair(CTNumber(1.0), CTPair(CTNumber(2.0), CTPair(CTNumber(3.0), CTEmptyList()))).ToString()
        |> should equal "(1 2 3)"

    [<Test>]
    member x.``printing pairs of lists`` () =
        CTPair(CTPair(CTNumber(1.0), CTPair(CTNumber(2.0), CTPair(CTNumber(3.0), CTEmptyList()))), Constants.False)
            .ToString()
        |> should equal "((1 2 3) . #f)"
        CTPair(CTPair(CTNumber(1.0), CTPair(CTNumber(2.0), CTPair(CTNumber(3.0), CTEmptyList()))),
               CTPair(Constants.True, CTPair(Constants.False, CTEmptyList())))
               .ToString()
        |> should equal "((1 2 3) #t #f)"

    [<Test>]
    member x.``printing strings`` () =
        CTString("Hello, world!\n").ToString() |> should equal "Hello, world!\n"
        CTString("Hello!").REPLDisplayValue() |> should equal "\"Hello!\""

    [<Test>]
    member x.``printing symbols`` () =
        CTSymbol("fib").ToString() |> should equal "fib"
