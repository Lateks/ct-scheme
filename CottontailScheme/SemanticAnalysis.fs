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

type Identifier = { name: string; uniqueName: string; }

type VariableReferenceDetails = { id: Identifier; fromExternalScope: bool }

type Expression =
     | VariableReference of VariableReferenceDetails
     | Closure of ClosureDefinition
     | ProcedureCall of Expression * Expression list
     | ValueExpression of LiteralValue
     | Assignment of VariableReferenceDetails * Expression
     | Conditional of Expression * Expression * Expression option
     | Loop of LoopDefinition
     | IdentifierDefinition of Identifier * Expression
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

// TODO: could definitions be handled separately?
// TODO: can formals parameters be shadowed?
// TODO: source code positions for errors
let rec handleExpression scope =
    function
    | IdentifierExpression id -> placeholder "identifier expressions"
    | LambdaExpression (formals, defs, exprs) -> placeholder "lambda expressions"
    | AssignmentExpression binding -> placeholder "assignments"
    | ProcedureCallExpression (expr, exprs) -> placeholder "procedure calls"
    | ConditionalExpression (cond, thenBranch, elseBranch) -> placeholder "conditionals"
    | LiteralExpression lit -> ValueExpression lit, scope
    | Definition binding -> handleDefinition scope binding
    | ExpressionError err -> failWithErrorNode err
and handleDefinition scope binding =
    match binding with
    | Binding (id, expr) ->
        let name = getIdentifierName id
        match findDefinition scope name with
        | Some definition ->
            failwithf "Duplicate definition for identifier %s" name
        | None ->
            let identifier = { name = name; uniqueName = symbolGen.generateSymbol name; }
            let value, _ = handleExpression scope expr // This expression cannot change the contents of the scope
            let newScope = addDefinition scope identifier
            IdentifierDefinition (identifier, value), newScope
    | BindingError err -> failWithErrorNode err

let rec handleExpressionList scope =
    function
    | [] -> []
    | x::xs ->
        let expr, newScope = handleExpression scope x
        expr :: (handleExpressionList newScope xs)

let makeBuiltInId name = { name = name; uniqueName = name }

let builtIns =
    [makeBuiltInId "display"
     makeBuiltInId "list"
     makeBuiltInId "car"
     makeBuiltInId "cdr"
     makeBuiltInId "cons"
     makeBuiltInId "+"
     makeBuiltInId "-"
     makeBuiltInId "*"
     makeBuiltInId "/"]

// TODO: exception handling
let analyse exprs =
    let builtInScope = { definitions = builtIns; parent = None }
    let programRootScope = { definitions = []; parent = Some builtInScope }
    handleExpressionList programRootScope exprs
