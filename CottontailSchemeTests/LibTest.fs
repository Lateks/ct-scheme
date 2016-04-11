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
        BuiltIns.Car(p) |> should equal car

    [<Test>]
    [<ExpectedException(typeof<TypeError>)>]
    member x.``car checks parameter type`` () =
        BuiltIns.Car(car) |> ignore

    [<Test>]
    member x.``implements cdr`` () =
        BuiltIns.Cdr(p) |> should equal cdr

    [<Test>]
    [<ExpectedException(typeof<TypeError>)>]
    member x.``cdr checks parameter type`` () =
        BuiltIns.Cdr(car) |> ignore

    [<Test>]
    member x.``implements cons`` () =
        BuiltIns.Cons(car, cdr) |> should equal p

    [<Test>]
    member x.``implements null?`` () =
        BuiltIns.IsNull(p) |> should equal Constants.False
        BuiltIns.IsNull(empty) |> should equal Constants.True
        BuiltIns.IsNull(car) |> should equal Constants.False

    [<Test>]
    member x.``implements list`` () =
        let members : CTObject array = [|car; cdr|]
        BuiltIns.List(members) |> should equal l
        BuiltIns.List([||]) |> should equal (CTEmptyList())

[<TestFixture>]
type ``numeric operations`` () =
    [<Test>]
    member x.``addition`` () =
        BuiltIns.Plus([||]) |> should equal (CTNumber(0.0))
        BuiltIns.Plus([|CTNumber(5.0); CTNumber(10.0); CTNumber(9.0)|]) |> should equal (CTNumber(24.0))
        BuiltIns.Plus([|CTNumber(6.0); CTNumber(-1.0)|]) |> should equal (CTNumber(5.0))
        BuiltIns.Plus([|CTNumber(3.5); CTNumber(2.4)|]) |> should equal (CTNumber(5.9))

    [<Test>]
    [<ExpectedException(typeof<TypeError>)>]
    member x.``addition checks parameter types`` () =
        BuiltIns.Plus([|CTNumber(3.14159); Constants.False|]) |> ignore

    [<Test>]
    member x.``subtraction`` () =
        BuiltIns.Minus([|CTNumber(5.0); CTNumber(2.0)|]) |> should equal (CTNumber(3.0))
        BuiltIns.Minus([|CTNumber(10.0); CTNumber(15.0); CTNumber(3.5)|]) |> should equal (CTNumber(-8.5))
        BuiltIns.Minus([|CTNumber(1.0)|]) |> should equal (CTNumber(-1.0))

    [<Test>]
    [<ExpectedException(typeof<TypeError>)>]
    member x.``subtraction checks parameter types`` () =
        BuiltIns.Minus([|CTString("foo")|]) |> ignore

    [<Test>]
    member x.``division`` () =
        BuiltIns.Div([|CTNumber(5.0)|]) |> should equal (CTNumber(1.0/5.0))
        BuiltIns.Div([|CTNumber(5.0); CTNumber(2.0)|]) |> should equal (CTNumber(2.5))
        BuiltIns.Div([|CTNumber(6.0); CTNumber(2.0); CTNumber(2.0)|]) |> should equal (CTNumber(1.5))

    [<Test>]
    member x.``division by zero`` () =
        BuiltIns.Div([|CTNumber(5.0); CTNumber(0.0)|]) |> should equal (CTNumber(Double.PositiveInfinity))

    [<Test>]
    [<ExpectedException(typeof<TypeError>)>]
    member x.``division checks parameter types`` () =
        BuiltIns.Div([|CTString("error")|]) |> ignore

    [<Test>]
    member x.``multiplication`` () =
        BuiltIns.Mult([||]) |> should equal (CTNumber(1.0))
        BuiltIns.Mult([|CTNumber(5.0)|]) |> should equal (CTNumber(5.0))
        BuiltIns.Mult([|CTNumber(2.0); CTNumber(3.0); CTNumber(4.0)|]) |> should equal (CTNumber(24.0))

    [<Test>]
    [<ExpectedException(typeof<TypeError>)>]
    member x.``multiplication checks parameter types`` () =
        BuiltIns.Mult([|CTString("error")|]) |> ignore

    [<Test>]
    member x.``zero?`` () =
        BuiltIns.IsZero(CTNumber(0.0)) |> should equal Constants.True
        BuiltIns.IsZero(CTNumber(-0.01)) |> should equal Constants.False
        BuiltIns.IsZero(CTNumber(0.01)) |> should equal Constants.False

    [<Test>]
    [<ExpectedException(typeof<TypeError>)>]
    member x.``zero? checks parameter type`` () =
        BuiltIns.IsZero(CTString("error")) |> ignore

    [<Test>]
    member x.``less than (<)`` () =
        BuiltIns.LessThan([|CTNumber(1.0); CTNumber(0.0)|]) |> should equal Constants.False
        BuiltIns.LessThan([|CTNumber(1.0); CTNumber(2.0)|]) |> should equal Constants.True
        BuiltIns.LessThan([|CTNumber(1.0); CTNumber(2.0); CTNumber(3.0)|]) |> should equal Constants.True
        BuiltIns.LessThan([|CTNumber(1.0); CTNumber(2.0); CTNumber(1.5)|]) |> should equal Constants.False

    [<Test>]
    [<ExpectedException(typeof<TypeError>)>]
    member x.``< checks parameter types`` () =
        BuiltIns.LessThan([|CTNumber(1.0); Constants.True|]) |> ignore

    [<Test>]
    member x.``greater than (>)`` () =
        BuiltIns.GreaterThan([|CTNumber(2.0); CTNumber(1.0)|]) |> should equal Constants.True
        BuiltIns.GreaterThan([|CTNumber(1.0); CTNumber(2.0)|]) |> should equal Constants.False
        BuiltIns.GreaterThan([|CTNumber(3.0); CTNumber(2.0); CTNumber(1.0)|]) |> should equal Constants.True
        BuiltIns.GreaterThan([|CTNumber(3.0); CTNumber(2.0); CTNumber(2.5)|]) |> should equal Constants.False

    [<Test>]
    [<ExpectedException(typeof<TypeError>)>]
    member x.``> checks parameter types`` () =
        BuiltIns.GreaterThan([|CTNumber(1.0); Constants.True|]) |> ignore

[<TestFixture>]
type ``boolean operations`` () =
    let isTruthy (v : CTObject) = v.ToBool() |> should equal true
    let isFalsy (v : CTObject) = v.ToBool() |> should equal false

    [<Test>]
    member x.``everything is truthy, except #f`` () =
        CTNumber(0.0) |> isTruthy
        CTNumber(1.0) |> isTruthy
        CTNumber(-1.0) |> isTruthy
        CTString("") |> isTruthy
        CTString("foo") |> isTruthy
        CTEmptyList() |> isTruthy
        BuiltIns.List([|CTNumber(1.0); CTNumber(2.0)|]) |> isTruthy
        CTSymbol("foo") |> isTruthy
        CTProcedure(0) |> isTruthy
        Constants.Undefined |> isTruthy
        Constants.True |> isTruthy
        Constants.False |> isFalsy

    [<Test>]
    member x.``not inverts the boolean value of any object`` () =
        BuiltIns.Not(CTNumber(0.0)) |> should equal Constants.False
        BuiltIns.Not(Constants.True) |> should equal Constants.False
        BuiltIns.Not(Constants.False) |> should equal Constants.True

    [<Test>]
    member x.``eq? compares equality`` () =
        BuiltIns.AreEq(CTNumber(1.0), CTNumber(1.0)) |> isTruthy
        BuiltIns.AreEq(CTNumber(1.0), CTNumber(2.0)) |> isFalsy
        BuiltIns.AreEq(Constants.True, Constants.True) |> isTruthy
        BuiltIns.AreEq(Constants.True, Constants.False) |> isFalsy
        BuiltIns.AreEq(CTString("test"), CTString("test")) |> isTruthy
        BuiltIns.AreEq(CTString("test"), CTString("testy")) |> isFalsy
        BuiltIns.AreEq(CTProcedure(0), CTProcedure(0)) |> isFalsy // procedures are compared for instance equality
        BuiltIns.AreEq(BuiltIns.List([|CTNumber(1.0); CTNumber(2.0); CTNumber(3.0)|]),
                       BuiltIns.List([|CTNumber(1.0); CTNumber(2.0); CTNumber(3.0)|]))
                       |> isTruthy
        BuiltIns.AreEq(CTEmptyList(), CTEmptyList()) |> isTruthy
        BuiltIns.AreEq(BuiltIns.List([|CTNumber(1.0); CTNumber(2.0); CTNumber(3.0)|]),
                       CTPair(CTNumber(1.0), CTPair(CTNumber(2.0), CTNumber(3.0))))
                       |> isFalsy
        BuiltIns.AreEq(CTPair(CTNumber(1.0), CTNumber(2.0)),
                               CTPair(CTNumber(1.0), CTNumber(2.0)))
                               |> isTruthy

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
