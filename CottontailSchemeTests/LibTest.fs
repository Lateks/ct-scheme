module LibTest

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
    member x.``implements cdr`` () =
        ListOperations.Cdr(p) |> should equal cdr

    [<Test>]
    member x.``implements cons`` () =
        ListOperations.Cons(car, cdr) |> should equal p

    [<Test>]
    member x.``implements null?`` () =
        ListOperations.IsNull(p) |> should equal false
        ListOperations.IsNull(empty) |> should equal true

    [<Test>]
    member x.``implements list`` () =
        let members : CTObject array = [|car; cdr|]
        ListOperations.List(members) |> should equal l

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
