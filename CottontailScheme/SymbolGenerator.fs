module CottontailScheme.SymbolGenerator

open System.Text.RegularExpressions

// TODO: handle multiple dashes without a name clash
let kebabCaseToCamelCase (name : string) =
    name.Split [|'-'|]
    |> Array.map (fun s -> if s.Length > 0 then
                              s.[0].ToString().ToUpper() + s.Substring (1)
                           else s)
    |> Array.toList
    |> String.concat ""

let convertPredicateName (name : string) =
    let regex = new Regex(".*\?$")
    if regex.IsMatch(name) then
        name.Substring (0, name.Length - 1)
        |> fun s -> "is-" + s
    else
        name

type SymbolGenerator () =
    let counters = new System.Collections.Generic.Dictionary<string, int>()

    // TODO: use a prefix?
    // TODO: replace symbols and other identifiers not allowed in .NET
    // TODO: reserved words?
    member this.generateSymbol name =
        if not (counters.ContainsKey name) then
            counters.Add (name, 1)
        let counter = counters.[name]
        counters.[name] <- counter + 1
        let convertedName = name
                            |> convertPredicateName
                            |> kebabCaseToCamelCase
        sprintf "%s$%d" convertedName counter