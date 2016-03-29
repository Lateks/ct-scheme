module CottontailScheme.SemanticAnalysis

// TODO in this phase:
// - recognize references to undefined variables
// - closure conversion?
//   -> find and label variables that are referenced from the environment
// - label tail calls
// - find inlinable lambdas?

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
open CottontailScheme.Scope
open CottontailScheme.SymbolGenerator

exception AnalysisException of string

type SequenceExpressionType = BeginSequence | AndSequence | OrSequence

type Expression =
     | VariableReference of Identifier
     | Closure of ClosureDefinition
     | ProcedureCall of Expression * Expression list
     | ValueExpression of LiteralValue
     | Assignment of Identifier * Expression
     | Conditional of Expression * Expression * Expression option
     | IdentifierDefinition of Identifier * Expression
     | SequenceExpression of SequenceExpressionType * Expression list
     | TailExpression of Expression
and ClosureFormals = SingleArgFormals of Identifier
                   | MultiArgFormals of Identifier list
and ClosureDefinition = { formals: ClosureFormals;
                          body: Expression list;
                          environment: Identifier list;
                          scope: Scope
                          isTailRecursive: bool;
                          functionName: Identifier option;
                          usedAsFirstClassValue: bool }

type Program =
    | ValidProgram of Expression list * Scope
    | ProgramAnalysisError of string

let symbolGen = SymbolGenerator ()

let placeholder name = failwithf "Not implemented yet: %s" name

let failWithErrorNode err = failwithf "Error, faulty AST given as input to analysis. Contains error \"%s\"." err.message

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
        | SequenceExpression (_, exprs) ->
            collectFromExprList exprs
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

let rec toTailExpression expr =
    let isValidArg =
        match expr with
        | IdentifierDefinition (_, _)
        | TailExpression _
            -> false
        | _ -> true

    assert isValidArg

    match expr with
    | Conditional (cond, thenExpr, elseExpr)
        -> Conditional (cond, toTailExpression thenExpr, Option.map toTailExpression elseExpr)
    | SequenceExpression (t, exprs) as seq
        -> if List.isEmpty exprs then
              TailExpression seq
           else
               let init = List.take (exprs.Length - 1) exprs
               let tailExpr = List.last exprs |> toTailExpression
               List.append init [tailExpr]
               |> fun es -> SequenceExpression (t, es)
    | expr -> TailExpression expr

let rec findTailExprs expr =
    let isValidArg =
        match expr with
        | Conditional (_, _, _)
        | TailExpression _
            -> true
        | _ -> false

    assert isValidArg

    match expr with
    | Conditional (cond, thenExpr, elseExpr)
        -> match elseExpr with
           | Some e -> List.map findTailExprs [thenExpr; e]
                       |> List.concat
           | None -> findTailExprs thenExpr
    | SequenceExpression (_, exprs)
        -> List.last exprs
           |> findTailExprs
    | TailExpression e -> [e]
    | _ -> []

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
    | BeginExpression exprs -> handleBeginExpression scope exprs
    | BooleanExpression (t, exprs) -> handleBooleanExpression scope t exprs
    | ExpressionError err -> failWithErrorNode err
and handleDefinition scope =
    function
    | Binding (id, expr) ->
        let variableRef = bindingForVariableReference scope id
        let valueExpr = match expr with
                        | LambdaExpression (fs, ds, es) ->
                            handleLambdaExpression scope fs ds es
                        | _ -> handleExpression scope expr
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
    let bodyExprs = List.last body
                    |> handleExpression bodyScope
                    |> toTailExpression
                    |> fun e -> [e]
                    |> List.append (List.take (body.Length - 1) body
                                    |> List.map (handleExpression bodyScope))
    let freeVars = collectFreeVariables bodyScope bodyExprs
    { formals = newFormals;
      body = bodyExprs;
      environment = freeVars;
      scope = bodyScope;
      functionName = None;
      isTailRecursive = false;
      usedAsFirstClassValue = false }
    |> Closure
and handleBeginExpression scope exprs =
    exprs
    |> List.map (handleExpression scope)
    |> fun es -> SequenceExpression (BeginSequence, es)
and handleBooleanExpression scope exprType exprs =
    exprs
    |> List.map (handleExpression scope)
    |> fun es -> match exprType with
                 | AndExpression -> (AndSequence, es)
                 | OrExpression -> (OrSequence, es)
    |> SequenceExpression

let isProcedureCall =
    function
    | ProcedureCall (_, _) -> true
    | _ -> false

// TODO (extra, not required): is the lambda expression inlinable?
// - not used as a first class value in the surrounding scope
// - does not contain recursive calls that are not in tail position
let isTailRecursive lambdaBody lambdaName =
    let isRecursiveCall expr =
        isProcedureCall expr && (match expr with
                                 | ProcedureCall (VariableReference id, _) -> id = lambdaName
                                 | _ -> false)

    let tailExprs = findTailExprs <| List.last lambdaBody
    let recursiveCalls = tailExprs |> List.filter isRecursiveCall
    not <| List.isEmpty recursiveCalls

let isUsedAsFirstClassValue name c parentExprs =
    let isNameReference id =
        match name with
        | Some n -> id = n
        | None -> false
    let rec usedAsFirstClassValueInExpr =
        function
        | VariableReference id -> isNameReference id
        | Closure c' -> c = c' || usedAsFirstClassValueInList c'.body
        | ProcedureCall (_, args) -> usedAsFirstClassValueInList args
        | ValueExpression _ -> false
        | Assignment (id, expr) // treat as if used as first class value if value can be overwritten
            -> isNameReference id || usedAsFirstClassValueInExpr expr
        | IdentifierDefinition (id, expr)
            -> if isNameReference id then
                   match expr with
                   | Closure c' -> assert (c = c')
                                   usedAsFirstClassValueInList c'.body
                   | _ -> false
               else
                   usedAsFirstClassValueInExpr expr
        | Conditional (e1, e2, e3) ->
            let usedInCondition = usedAsFirstClassValueInExpr e1
            let usedInThenBranch = usedAsFirstClassValueInExpr e2
            let usedInElseBranch = match e3 with
                                   | Some e -> usedAsFirstClassValueInExpr e
                                   | None -> false
            usedInCondition || usedInThenBranch || usedInElseBranch
        | SequenceExpression (_, exprs) ->
            usedAsFirstClassValueInList exprs
        | TailExpression e -> usedAsFirstClassValueInExpr e
    and usedAsFirstClassValueInList exprs =
        exprs
        |> List.fold (fun usedSoFar e -> usedSoFar || usedAsFirstClassValueInExpr e) false

    usedAsFirstClassValueInList parentExprs

let rec labelLambdas exprs =
    let rec label name =
        function
        | Closure c -> Closure <| labelClosure c name exprs
        | Assignment (id, expr)
            -> Assignment (id, label (Some id) expr)
        | IdentifierDefinition (id, expr)
            -> IdentifierDefinition (id, label (Some id) expr)
        | Conditional (e1, e2, e3)
            -> Conditional (label None e1, label name e2, Option.map (label name) e3)
        | ProcedureCall (proc, args)
            -> ProcedureCall (label None proc, List.map (label None) args)
        | e -> e

    exprs |> List.map (label None)
and labelClosure c name parentScopeExprs =
    let labeledBody = labelLambdas c.body
    let tailRecursive = match name with
                        | None -> false
                        | Some n -> isTailRecursive labeledBody n
    let usedAsFirstClassValue = isUsedAsFirstClassValue name c parentScopeExprs
    { c with body = labeledBody;
             functionName = name;
             isTailRecursive = tailRecursive;
             usedAsFirstClassValue = usedAsFirstClassValue }

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
     makeBuiltInId "zero?"
     makeBuiltInId "not"]

let analyse exprs =
    try
        let builtInScope = { definitions = builtIns; parent = None }
        let topLevelScope = buildScope builtInScope exprs
        exprs
        |> List.map (handleExpression topLevelScope)
        |> labelLambdas
        |> fun es -> ValidProgram (es, topLevelScope)
    with
        | AnalysisException msg -> ProgramAnalysisError msg
