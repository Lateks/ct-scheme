﻿module CottontailScheme.SemanticAnalysis

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

type SymbolGenerator () =
    let counters = new System.Collections.Generic.Dictionary<string, int>()

    // TODO: use a prefix?
    member this.generateSymbol name =
        if not (counters.ContainsKey name) then
            counters.Add (name, 1)
        let counter = counters.[name]
        counters.[name] <- counter + 1
        sprintf "%s$%d" name counter

exception AnalysisException of string

type Identifier = { name: string; uniqueName: string; }

type Expression =
     | VariableReference of Identifier
     | Closure of ClosureDefinition
     | ProcedureCall of Expression * Expression list
     | ValueExpression of LiteralValue
     | Assignment of Identifier * Expression
     | Conditional of Expression * Expression * Expression option
     | Loop of LoopDefinition
     | IdentifierDefinition of Identifier * Expression
     | LambdaPlaceholder of ASTBuilder.LambdaFormals * ASTBuilder.Expression list * ASTBuilder.Expression list
and ClosureFormals = SingleArgFormals of Identifier
                   | MultiArgFormals of Identifier list
and ClosureDefinition = { formals: ClosureFormals;
                          definitions: Expression list;
                          body: Expression list;
                          environment: Identifier list }
and LoopDefinition = { test: Expression
                       loopVars: Identifier list
                       returnVar: Identifier
                       loopBody: Expression list }

type Program =
    | ValidProgram of Expression list
    | ProgramAnalysisError of string

type Scope = { definitions: Identifier list; parent: Scope option }

let addDefinition scope identifier = { definitions = identifier::scope.definitions; parent = scope.parent}

let rec findDefinition scope name =
    scope.definitions |> List.tryFind (fun id -> id.name = name)
and findDefinitionRec scope name =
    let definition = findDefinition scope name
    match definition, scope.parent with
    | Some _, _ -> definition
    | None, None -> None
    | None, Some parent -> findDefinitionRec parent name

let symbolGen = SymbolGenerator ()

let placeholder name = failwithf "Not implemented yet: %s" name

let failWithErrorNode err = failwithf "Error, faulty AST given as input to analysis. Contains error \"%s\"." err.message

let getIdentifierName =
    function
    | Identifier id -> id
    | IdentifierError err -> failWithErrorNode err

let bindingForName scope name =
    match findDefinitionRec scope name with
    | None -> sprintf "Reference to undefined identifier %s" name |> AnalysisException |> raise
    | Some id -> id

let bindingForVariableReference scope id =
    getIdentifierName id |> bindingForName scope

let handleIdentifierExpression scope id =
    VariableReference (bindingForVariableReference scope id), scope

// TODO: could definitions be handled separately?
// TODO: source code positions for exceptions
// TODO for lambda expressions:
// - how can mutual recursion be made possible?
//   -> should lambdas be represented by placeholders that are evaluated in a second pass?
//      -> this would require a new pass for each scope that introduces new lambdas
//   -> or, in each new scope, have a separate pass before other analysis that just builds the scope?
// - a lambda can reference other identifiers that are defined after the lambda definition in the source code
//   -> it is possible to accidentally reference an undefined variable at runtime
// - allow shadowing of formal parameter names inside the lambda body
//   (lambda body has its own scope)
// - compute list of captured variables (not defined inside lambda body or its formal parameter list)
// - recognize tail recursive cases and represent them as loops
//   (the closure can be removed completely in this case)
let rec handleExpression scope =
    function
    | IdentifierExpression id -> handleIdentifierExpression scope id
    | LambdaExpression (fs, ds, es) as l -> LambdaPlaceholder (fs, ds, es), scope
    | AssignmentExpression binding -> handleAssignment scope binding
    | ProcedureCallExpression (expr, exprs) -> handleProcedureCall scope expr exprs
    | ConditionalExpression (cond, thenBranch, elseBranch) -> handleConditional scope cond thenBranch elseBranch
    | LiteralExpression lit -> ValueExpression lit, scope
    | Definition binding -> handleDefinition scope binding
    | ExpressionError err -> failWithErrorNode err
and handleNonScopeChangingExpression scope expr =
    // TODO: assert that expr is not a definition
    let expr, _ = handleExpression scope expr
    expr
and handleDefinition scope binding =
    match binding with
    | Binding (id, expr) ->
        let name = getIdentifierName id
        match findDefinition scope name with
        | Some definition ->
            sprintf "Duplicate definition for identifier %s" name |> AnalysisException |> raise
        | None ->
            let identifier = { name = name; uniqueName = symbolGen.generateSymbol name; }
            let value = handleNonScopeChangingExpression scope expr
            let newScope = addDefinition scope identifier
            IdentifierDefinition (identifier, value), newScope
    | BindingError err -> failWithErrorNode err
and handleConditional scope cond thenBranch elseBranch =
    let condExpr = handleNonScopeChangingExpression scope cond
    let thenExpr = handleNonScopeChangingExpression scope thenBranch
    let elseExpr = elseBranch |> Option.map (handleNonScopeChangingExpression scope)
    Conditional (condExpr, thenExpr, elseExpr), scope
and handleProcedureCall scope procExpr exprs =
    let proc = handleNonScopeChangingExpression scope procExpr
    let args = List.map (handleNonScopeChangingExpression scope) exprs
    ProcedureCall (proc, args), scope
and handleAssignment scope binding =
    match binding with
    | Binding (id, expr) ->
        let variableRef = bindingForVariableReference scope id
        let valueExpr = handleNonScopeChangingExpression scope expr
        Assignment (variableRef, valueExpr), scope
    | BindingError err -> failWithErrorNode err

let rec handleExpressionList scope exprs =
    let rec f scope res =
        function
        | [] -> res
        | x::xs ->
            let expr, newScope = handleExpression scope x
            f newScope (expr::res) xs
    List.rev <| f scope [] exprs

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
        let programRootScope = { definitions = []; parent = Some builtInScope }
        handleExpressionList programRootScope exprs |> ValidProgram
    with
        | AnalysisException msg -> ProgramAnalysisError msg
