module CottontailScheme.SymbolGenerator

open System.Text.RegularExpressions

let toProcedureObjectName = sprintf "Obj$%s"

let capitalizeWord (w: string) = w.[0].ToString().ToUpper() + w.Substring (1)

let kebabCaseToCamelCase (name : string) =
    name.Split([|'-'|])
    |> Array.map (fun s -> if s.Length > 0 then
                              capitalizeWord s
                           else "")
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
    let removeExtraDashAtEnd (newName: string) =
        if newName.Chars (newName.Length - 1) <> name.Chars (name.Length - 1) then
            newName.TrimEnd [|'-'|]
        else
            newName

    let replaceNonKebabCaseDashes =
        fun s -> let allDashes = new Regex("^\-+$")
                 if allDashes.IsMatch s then s.Replace("-", "Minus") else s
        >> fun s -> let dashesAtBeginning = new Regex("^\-+")
                    dashesAtBeginning.Replace(s, fun m -> (m.Groups.Item 0).Value.Replace("-", "Minus") + "-")
        >> fun s -> let dashesAtEnd = new Regex("\-+$")
                    dashesAtEnd.Replace(s, fun m -> "-" + (m.Groups.Item 0).Value.Replace("-", "Minus"))

    name.Replace("!", "Exclamation-")
        .Replace("$", "Dollar-")
        .Replace("%", "Percent-")
        .Replace("&", "And-")
        .Replace("*", "Star-")
        .Replace("/", "Div-")
        .Replace(":", "Colon-")
        .Replace("<", "Lt-")
        .Replace("=", "Eq-")
        .Replace(">", "Gt-")
        .Replace("?", "Question-")
        .Replace("^", "Hat-")
        .Replace("~", "Tilde-")
        .Replace("+", "Plus-")
    |> removeExtraDashAtEnd
    |> replaceNonKebabCaseDashes

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