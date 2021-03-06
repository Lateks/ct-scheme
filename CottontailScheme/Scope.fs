﻿module CottontailScheme.Scope

type Identifier = { name: string; uniqueName: string; argIndex: int option }

type Scope = { definitions: Identifier list; parent: Scope option }

let addDefinition scope identifier = { definitions = identifier::scope.definitions; parent = scope.parent}

let getBuiltIns scope =
    let rec findRootScope scope =
        match scope.parent with
        | None -> scope
        | Some s -> findRootScope s

    let rootScope = findRootScope scope
    rootScope.definitions

let findProgramScope scope =
    let rec find prev scope =
        match scope.parent with
        | Some parentScope -> find scope parentScope 
        | None -> prev

    find scope scope

let rec findDefinition scope name =
    scope.definitions |> List.tryFind (fun id -> id.name = name)
and findDefinitionRec scope name =
    let definition = findDefinition scope name
    match definition, scope.parent with
    | Some _, _ -> definition
    | None, None -> None
    | None, Some parent -> findDefinitionRec parent name

let rec findDefiningScope scope id =
    let definition = scope.definitions |> List.tryFind (fun id2 -> id = id2)
    match definition, scope.parent with
    | Some _, _ -> scope
    | None, None -> failwithf "No definition found for id %A" id
    | None, Some parent -> findDefiningScope parent id
