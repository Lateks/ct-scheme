module CottontailScheme.SemanticAnalysis

// TODO in this phase:
// - alpha conversion for variables
// - recognize references to undefined variables
// - closure conversion?
//   -> find and label variables that are referenced from the environment
// - label tail calls

// Notes:
//
// Lambda expressions compile into closure objects that are parameterised with
// their environment.
//
// Directly tail recursive functions compile into a loop in the function body.
// -> Can use a loop element as an intermediate language construct.
// -> What should the loop element's parameters be?
//
// Other tail calls just need to be labeled so we can use the .tail annotation.
//
// "Values" in tail position need to be identified to insert return statements.
// Do we need to label expressions or is it redundant?
//
// Lambda expression formals can be shadowed by local definitions.

open CottontailScheme.ASTBuilder
open System.Text.RegularExpressions

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

exception AnalysisException of string

type Identifier = { name: string; uniqueName: string; }

type Scope = { definitions: Identifier list; parent: Scope option }

type Expression =
     | VariableReference of Identifier
     | Closure of ClosureDefinition
     | ProcedureCall of Expression * Expression list
     | ValueExpression of LiteralValue
     | Assignment of Identifier * Expression
     | Conditional of Expression * Expression * Expression option
     | Loop of LoopDefinition
     | IdentifierDefinition of Identifier * Expression
and ClosureFormals = SingleArgFormals of Identifier
                   | MultiArgFormals of Identifier list
and ClosureDefinition = { formals: ClosureFormals;
                          body: Expression list;
                          environment: Identifier list;
                          scope: Scope }
and LoopDefinition = { test: Expression
                       loopVars: Identifier list
                       returnVar: Identifier
                       loopBody: Expression list }

type Program =
    | ValidProgram of Expression list * Scope
    | ProgramAnalysisError of string

let symbolGen = SymbolGenerator ()

let placeholder name = failwithf "Not implemented yet: %s" name

let failWithErrorNode err = failwithf "Error, faulty AST given as input to analysis. Contains error \"%s\"." err.message

let addDefinition scope identifier = { definitions = identifier::scope.definitions; parent = scope.parent}

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

let getIdentifierName =
    function
    | Identifier id -> id
    | IdentifierError err -> failWithErrorNode err

let findDefinitionForId scope id =
    getIdentifierName id |> findDefinition scope

let findDefinitionForIdRec scope id =
    getIdentifierName id |> findDefinitionRec scope

let newIdentifierFor name =
    { name = name; uniqueName = symbolGen.generateSymbol name }

let newIdentifierForId id =
    getIdentifierName id |> newIdentifierFor

let buildScope parentScope exprs =
    let expandScopeWithBinding scope =
        function
        | Binding (id, _) ->
            let name = getIdentifierName id
            match findDefinition scope name with
            | Some _ ->
                sprintf "Duplicate definition for identifier %s" name |> AnalysisException |> raise
            | None ->
                newIdentifierFor name |> addDefinition scope
        | BindingError err -> failWithErrorNode err

    let checkAssignment scope =
        function
        | Binding (id, _) ->
            let name = getIdentifierName id
            match findDefinitionRec scope name with
            | Some _ -> scope
            | None -> sprintf "Trying to set! an undefined identifier %s" name |> AnalysisException |> raise
        | BindingError err -> failWithErrorNode err

    let addToScope scope =
        function
        | Definition binding -> expandScopeWithBinding scope binding
        | AssignmentExpression binding -> checkAssignment scope binding
        | ExpressionError err -> failWithErrorNode err
        | _ -> scope

    let scope = { definitions = []; parent = Some parentScope }
    List.fold addToScope scope exprs

let buildLambdaScope parentScope formals =
    let identifiers = match formals with
                      | SingleArgFormals arg -> [arg]
                      | MultiArgFormals args -> args
    { definitions = identifiers; parent = Some parentScope }

let convertFormals =
    function
    | ASTBuilder.SingleArgFormals id -> newIdentifierForId id |> SingleArgFormals
    | ASTBuilder.MultiArgFormals ids -> ids |> List.map newIdentifierForId |> MultiArgFormals

let collectFreeVariables bodyScope body =
    assert bodyScope.parent.IsSome

    let lambdaScope = bodyScope.parent.Value
    let programScope = findProgramScope lambdaScope

    let collect id =
        let findDefinitionInScope scope = findDefinition scope id.name
        match findDefinitionInScope bodyScope, findDefinitionInScope lambdaScope with
        | None, None ->
            match findDefinitionRec lambdaScope id.name, findDefinitionRec programScope id.name with
            | Some id1, Some id2 -> if id1 <> id2 then [id] else []
            | Some id', None       -> [id]
            | None, _ -> assert false
                         []
        | _, _ -> []

    let rec collectFreeVariables expr =
        match expr with
        | VariableReference id -> collect id
        | ProcedureCall (proc, args) ->
            args
            |> collectFromExprList
            |> List.append (collectFreeVariables proc)
        | Assignment (id, expr) ->
            List.append (collect id) (collectFreeVariables expr)
        | Conditional (cond, thenExpr, elseExpr) ->
            match elseExpr with
            | Some expr ->
                [cond; thenExpr; expr]
            | None ->
                [cond; thenExpr]
            |> collectFromExprList
        | _ -> []
    and collectFromExprList =
        List.map collectFreeVariables >> List.concat

    collectFromExprList body
    |> List.distinct

let bindingForName scope name =
    match findDefinitionRec scope name with
    | None -> sprintf "Reference to undefined identifier %s" name |> AnalysisException |> raise
    | Some id -> id

let bindingForVariableReference scope id =
    getIdentifierName id |> bindingForName scope

let handleIdentifierExpression scope id =
    VariableReference (bindingForVariableReference scope id)

// TODO: could definitions be handled separately?
// TODO: source code positions for exceptions
// TODO for lambda expressions:
// - a lambda can reference other identifiers that are defined after the lambda definition in the source code
//   -> it is possible to accidentally reference an undefined variable at runtime
// - compute list of captured variables (not defined inside lambda body or its formal parameter list)
// - recognize tail recursive cases and represent them as loops
//   (the closure can be removed completely in this case)
// TODO: organize definitions in the following way:
// - lambda definitions first
// - other definitions in order of appearance after lambdas
//   -> verify that there are no circular references!
let rec handleExpression scope =
    function
    | IdentifierExpression id -> handleIdentifierExpression scope id
    | LambdaExpression (fs, ds, es) -> handleLambdaExpression scope fs ds es
    | AssignmentExpression binding -> handleAssignment scope binding
    | ProcedureCallExpression (expr, exprs) -> handleProcedureCall scope expr exprs
    | ConditionalExpression (cond, thenBranch, elseBranch) -> handleConditional scope cond thenBranch elseBranch
    | LiteralExpression lit -> ValueExpression lit
    | Definition binding -> handleDefinition scope binding
    | ExpressionError err -> failWithErrorNode err
and handleDefinition scope =
    function
    | Binding (id, expr) ->
        let variableRef = bindingForVariableReference scope id
        let valueExpr = handleExpression scope expr
        IdentifierDefinition (variableRef, valueExpr)
    | BindingError err -> failWithErrorNode err
and handleConditional scope cond thenBranch elseBranch =
    let condExpr = handleExpression scope cond
    let thenExpr = handleExpression scope thenBranch
    let elseExpr = elseBranch |> Option.map (handleExpression scope)
    Conditional (condExpr, thenExpr, elseExpr)
and handleProcedureCall scope procExpr exprs =
    let proc = handleExpression scope procExpr
    let args = List.map (handleExpression scope) exprs
    ProcedureCall (proc, args)
and handleAssignment scope binding =
    match binding with
    | Binding (id, expr) ->
        let variableRef = bindingForVariableReference scope id
        let valueExpr = handleExpression scope expr
        Assignment (variableRef, valueExpr)
    | BindingError err -> failWithErrorNode err
and handleLambdaExpression scope formals defs exprs =
    let body = List.append defs exprs
    let newFormals = convertFormals formals
    let bodyScope = buildLambdaScope scope newFormals
                    |> fun s -> buildScope s body
    let bodyExprs = body |> List.map (handleExpression bodyScope)
    let freeVars = collectFreeVariables bodyScope bodyExprs
    { formals = newFormals; body = bodyExprs; environment = freeVars; scope = bodyScope }
    |> Closure

let makeBuiltInId name = { name = name; uniqueName = name }

let builtIns =
    [makeBuiltInId "display"
     makeBuiltInId "list"
     makeBuiltInId "car"
     makeBuiltInId "cdr"
     makeBuiltInId "cons"
     makeBuiltInId "null?"
     makeBuiltInId "+"
     makeBuiltInId "-"
     makeBuiltInId "*"
     makeBuiltInId "/"
     makeBuiltInId "<"
     makeBuiltInId ">"
     makeBuiltInId "eq?"
     makeBuiltInId "zero?"]

let analyse exprs =
    try
        let builtInScope = { definitions = builtIns; parent = None }
        let topLevelScope = buildScope builtInScope exprs
        exprs
        |> List.map (handleExpression topLevelScope)
        |> fun es -> ValidProgram (es, topLevelScope)
    with
        | AnalysisException msg -> ProgramAnalysisError msg
