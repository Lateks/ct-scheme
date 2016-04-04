module LibTest

open NUnit.Framework
open FsUnit
open CottontailSchemeLib

[<TestFixture>]
type ``List and pair operations`` () =
    let car = CTNumber(1.0)
    let cdr = CTNumber(2.0)
    let p = new CTPair(car, cdr)
    let empty = new CTEmptyList();
    let l = new CTPair(car, new CTPair(cdr, new CTEmptyList()));

    [<Test>]
    member x.``list equality`` () =
       let l2 = new CTPair(car, new CTPair(cdr, new CTEmptyList()));
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
