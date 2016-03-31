module CottontailScheme.SymbolGenerator

let kebabCaseToCamelCase (name : string) =
    name.Split [|'-'|]
    |> Array.map (fun s -> if s.Length > 0 then
                              s.[0].ToString().ToUpper() + s.Substring (1)
                           else "Dash")
    |> Array.toList
    |> String.concat ""

let replaceSuffixWithPrefix (name : string) (suffix : string) (replacement : string) =
    if name.EndsWith(suffix) then
        name.Substring (0, name.Length - suffix.Length)
        |> fun s -> replacement + s
    else
        name

let convertPredicateName (name : string) =
    replaceSuffixWithPrefix name "?" "is-"

let convertMutatorName (name : string) =
    replaceSuffixWithPrefix name "!" "do-"

let replaceSymbols (name: string) =
    let replaceExtraDashAtEnd (newName: string) =
        if newName.Chars (newName.Length - 1) <> name.Chars (name.Length - 1) then
            newName.TrimEnd [|'-'|]
        else
            newName

    let replaceExtraDashAtBeginning (newName : string) =
        if newName.Chars 0 <> name.Chars 0 then
            newName.TrimStart [|'-'|]
        else
            newName

    name.Replace("!", "-exclamation-")
        .Replace("$", "-dollar-")
        .Replace("%", "-percent-")
        .Replace("&", "-and-")
        .Replace("*", "-star-")
        .Replace("/", "-div-")
        .Replace(":", "-colon-")
        .Replace("<", "-lt-")
        .Replace("=", "-eq-")
        .Replace(">", "-gt-")
        .Replace("?", "-question-")
        .Replace("^", "-hat-")
        .Replace("~", "-tilde-")
        .Replace("+", "-plus-")
    |> replaceExtraDashAtBeginning
    |> replaceExtraDashAtEnd

type SymbolGenerator () =
    let counters = new System.Collections.Generic.Dictionary<string, int>()

    member this.generateSymbol name =
        let convertedName = name
                            |> convertPredicateName
                            |> convertMutatorName
                            |> replaceSymbols
                            |> kebabCaseToCamelCase

        if not (counters.ContainsKey convertedName) then
            counters.Add (convertedName, 1)
        let counter = counters.[convertedName]
        counters.[convertedName] <- counter + 1

        sprintf "%s$%d" convertedName counter