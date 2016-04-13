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
open CottontailScheme.Literals
open CottontailScheme.Scope
open CottontailScheme.SymbolGenerator

exception AnalysisException of string

type SequenceExpressionType = BeginSequence | AndSequence | OrSequence

type ClosureFormals = SingleArgFormals of Scope.Identifier
                    | MultiArgFormals of Scope.Identifier list

type Expression =
     | VariableReference of Scope.Identifier
     | Closure of ClosureDefinition
     | ProcedureCall of Expression * Expression list * bool
     | ValueExpression of Literals.LiteralValue
     | Assignment of Scope.Identifier * Expression
     | Conditional of Expression * Expression * Expression
     | SequenceExpression of SequenceExpressionType * Expression list
     | UndefinedValue
and ClosureDefinition = { formals: ClosureFormals;
                          procedureDefinitions: ProcedureDefinition list;
                          variableDeclarations : VariableDeclaration list;
                          body: Expression list;
                          environment: Identifier list;
                          scope: Scope
                          isTailRecursive: bool;
                          functionName: Identifier;
                          isUsedAsFirstClassValue: bool;
                          isReassigned : bool }
and ProcedureDefinition = ProcedureDefinition of Scope.Identifier * ClosureDefinition
and VariableDeclaration = VariableDeclaration of Scope.Identifier

type ProgramStructure = { procedureDefinitions: ProcedureDefinition list;
                          variableDeclarations: VariableDeclaration list;
                          expressions : Expression list;
                          scope : Scope.Scope }

type Program =
    | ValidProgram of ProgramStructure
    | ProgramAnalysisError of string

// Internal representation that handles definitions as expressions
type AnalysisExpression =
     | AnalysisVariableReference of Identifier
     | AnalysisClosure of AnalysisClosureDefinition
     | AnalysisProcedureCall of AnalysisExpression * AnalysisExpression list
     | AnalysisValueExpression of LiteralValue
     | AnalysisAssignment of Identifier * AnalysisExpression
     | AnalysisConditional of AnalysisExpression * AnalysisExpression * AnalysisExpression
     | AnalysisIdentifierDefinition of Identifier * AnalysisExpression
     | AnalysisSequenceExpression of SequenceExpressionType * AnalysisExpression list
     | AnalysisTailExpression of AnalysisExpression
     | AnalysisUndefinedValue
and AnalysisClosureDefinition = { formals: ClosureFormals;
                                  body: AnalysisExpression list;
                                  environment: Identifier list;
                                  scope: Scope
                                  isTailRecursive: bool;
                                  functionName: Identifier option;
                                  isUsedAsFirstClassValue: bool;
                                  isReassigned : bool }

let failWithErrorNode err = failwithf "Error, faulty AST given as input to analysis. Contains error \"%s\"." err.message

let isProcedureCall =
    function
    | AnalysisProcedureCall (_, _) -> true
    | _ -> false

module IdentifierHelpers =
    let symbolGen = SymbolGenerator ()

    let makeBuiltInId name = { name = name; uniqueName = name; argIndex = None }

    type BuiltInFunctionArgs = SetNumberOfArgs of int
                             | VarArgs
                             | VarArgsAtLeast of int

    let builtIns =
        Map.ofList
            [makeBuiltInId "display", SetNumberOfArgs 1
             makeBuiltInId "list", VarArgs
             makeBuiltInId "car", SetNumberOfArgs 1
             makeBuiltInId "cdr", SetNumberOfArgs 1
             makeBuiltInId "cons", SetNumberOfArgs 2
             makeBuiltInId "null?", SetNumberOfArgs 1
             makeBuiltInId "+", VarArgs
             makeBuiltInId "-", VarArgsAtLeast 1
             makeBuiltInId "*", VarArgs
             makeBuiltInId "/", VarArgsAtLeast 1
             makeBuiltInId "<", VarArgsAtLeast 2
             makeBuiltInId ">", VarArgsAtLeast 2
             makeBuiltInId "eq?", SetNumberOfArgs 2
             makeBuiltInId "zero?", SetNumberOfArgs 1
             makeBuiltInId "not", SetNumberOfArgs 1
             makeBuiltInId "newline", SetNumberOfArgs 0]

    let getIdentifierName =
        function
        | Identifier id -> id
        | IdentifierError err -> failWithErrorNode err

    let findDefinitionForId scope id =
        getIdentifierName id |> findDefinition scope

    let findDefinitionForIdRec scope id =
        getIdentifierName id |> findDefinitionRec scope

    let newIdentifierFor argIndex name =
        { name = name; uniqueName = symbolGen.generateSymbol name; argIndex = argIndex }

    let newIdentifierForId id argIndex =
        getIdentifierName id |> newIdentifierFor argIndex
    
    let bindingForName scope name =
        match findDefinitionRec scope name with
        | None -> sprintf "Reference to undefined identifier %s" name |> AnalysisException |> raise
        | Some id -> id

    let bindingForVariableReference scope id =
        getIdentifierName id |> bindingForName scope

    let generateAnonymousProcedureName () =
        symbolGen.generateSymbol "lambda"

module LambdaHelpers =
    let buildLambdaScope parentScope formals =
        let identifiers = match formals with
                          | SingleArgFormals arg -> [arg]
                          | MultiArgFormals args -> args
        { definitions = identifiers; parent = Some parentScope }

    let convertFormals =
        function
        | ASTBuilder.SingleArgFormals id -> IdentifierHelpers.newIdentifierForId id (Some 0) |> SingleArgFormals
        | ASTBuilder.MultiArgFormals ids -> seq{0..ids.Length-1}
                                            |> Seq.toList
                                            |> List.zip ids
                                            |> List.map (fun (id, index) ->
                                                             IdentifierHelpers.newIdentifierForId id (Some index))
                                            |> MultiArgFormals

    let collectFreeVariables bodyScope body =
        assert bodyScope.parent.IsSome

        let lambdaScope = bodyScope.parent.Value
        let programScope = findProgramScope lambdaScope

        let collect id =
            let findDefinitionInScope scope = findDefinition scope id.name
            match findDefinitionInScope bodyScope, findDefinitionInScope lambdaScope with
            | None, None -> [id]
            | _, _ -> []

        let rec collectFreeVariables expr =
            match expr with
            | AnalysisVariableReference id -> collect id
            | AnalysisProcedureCall (proc, args) ->
                args
                |> collectFromExprList
                |> List.append (collectFreeVariables proc)
            | AnalysisAssignment (id, expr) ->
                List.append (collect id) (collectFreeVariables expr)
            | AnalysisConditional (cond, thenExpr, elseExpr) ->
                [cond; thenExpr; elseExpr]
                |> collectFromExprList
            | AnalysisSequenceExpression (_, exprs) ->
                collectFromExprList exprs
            | AnalysisTailExpression e ->
                collectFreeVariables e
            | _ -> []
        and collectFromExprList =
            List.map collectFreeVariables >> List.concat

        collectFromExprList body
        |> List.distinct

    let rec toTailExpression expr =
        let isValidArg =
            match expr with
            | AnalysisIdentifierDefinition (_, _)
            | AnalysisTailExpression _
                -> false
            | _ -> true

        assert isValidArg

        match expr with
        | AnalysisConditional (cond, thenExpr, elseExpr)
            -> AnalysisConditional (cond, toTailExpression thenExpr, toTailExpression elseExpr)
        | AnalysisSequenceExpression (t, exprs) as seq
            -> if List.isEmpty exprs then
                  AnalysisTailExpression seq
               else
                   let init = List.take (exprs.Length - 1) exprs
                   let tailExpr = List.last exprs |> toTailExpression
                   List.append init [tailExpr]
                   |> fun es -> AnalysisSequenceExpression (t, es)
        | expr -> AnalysisTailExpression expr

    let rec findTailExprs expr =
        let isValidArg =
            match expr with
            | AnalysisConditional (_, _, _)
            | AnalysisSequenceExpression (_, _)
            | AnalysisTailExpression _
                -> true
            | _ -> false

        assert isValidArg

        match expr with
        | AnalysisConditional (cond, thenExpr, elseExpr)
            -> [thenExpr; elseExpr]
               |> List.map findTailExprs
               |> List.concat
        | AnalysisSequenceExpression (_, exprs)
            -> List.last exprs
               |> findTailExprs
        | AnalysisTailExpression e -> [e]
        | _ -> []

    let isTailRecursive lambdaBody lambdaName =
        let isRecursiveCall expr =
            isProcedureCall expr && (match expr with
                                     | AnalysisProcedureCall (AnalysisVariableReference id, _) -> id = lambdaName
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
            | AnalysisVariableReference id -> isNameReference id
            | AnalysisClosure c' -> c = c' || usedAsFirstClassValueInList c'.body
            | AnalysisProcedureCall (_, args) -> usedAsFirstClassValueInList args
            | AnalysisValueExpression _ -> false
            | AnalysisAssignment (_, expr) -> usedAsFirstClassValueInExpr expr
            | AnalysisIdentifierDefinition (id, expr)
                -> if isNameReference id then
                       match expr with
                       | AnalysisClosure c' -> usedAsFirstClassValueInList c'.body
                       | _ -> false
                   else
                       usedAsFirstClassValueInExpr expr
            | AnalysisConditional (e1, e2, e3) ->
                usedAsFirstClassValueInExpr e1 || usedAsFirstClassValueInExpr e2 || usedAsFirstClassValueInExpr e3
            | AnalysisSequenceExpression (_, exprs) ->
                usedAsFirstClassValueInList exprs
            | AnalysisTailExpression e -> usedAsFirstClassValueInExpr e
            | AnalysisUndefinedValue -> false
        and usedAsFirstClassValueInList exprs =
            exprs
            |> List.fold (fun usedSoFar e -> usedSoFar || usedAsFirstClassValueInExpr e) false

        usedAsFirstClassValueInList parentExprs

    let isReassigned closureId parentExprs =
        let rec isReassignedInExpr =
            function
            | AnalysisClosure c' -> List.contains closureId c'.environment && isReassignedInList c'.body
            | AnalysisProcedureCall (_, args) -> isReassignedInList args
            | AnalysisAssignment (id, expr) -> id = closureId || isReassignedInExpr expr
            | AnalysisIdentifierDefinition (_, expr)
                -> isReassignedInExpr expr
            | AnalysisConditional (e1, e2, e3) ->
                isReassignedInExpr e1 || isReassignedInExpr e2 || isReassignedInExpr e3
            | AnalysisSequenceExpression (_, exprs) ->
                isReassignedInList exprs
            | AnalysisTailExpression e -> isReassignedInExpr e
            | AnalysisValueExpression _
            | AnalysisVariableReference _
            | AnalysisUndefinedValue -> false
        and isReassignedInList exprs =
            exprs
            |> List.fold (fun reassignedSoFar e -> reassignedSoFar || isReassignedInExpr e) false

        isReassignedInList parentExprs

// Builds a new scope that has parentScope as a parent.
// - finds all definitions in scope
// - checks for duplicate definitions inside inner scopes
//   (top level scope allows duplicate definitions)
// - checks assignments for validity
let buildScope parentScope exprs =
    let isRootScope scope =
        findProgramScope scope = scope

    let expandScopeWithBinding scope =
        function
        | Binding (id, _) ->
            let name = IdentifierHelpers.getIdentifierName id
            match findDefinition scope name with
            | Some _ ->
                if isRootScope scope then
                    scope
                else
                    sprintf "Duplicate definition for identifier %s" name |> AnalysisException |> raise
            | None ->
                IdentifierHelpers.newIdentifierFor None name |> addDefinition scope
        | BindingError err -> failWithErrorNode err

    let checkAssignment scope =
        function
        | Binding (id, _) ->
            let name = IdentifierHelpers.getIdentifierName id
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

let handleIdentifierExpression scope id =
    AnalysisVariableReference (IdentifierHelpers.bindingForVariableReference scope id)

// TODO: source code positions for exceptions
// First pass through AST:
// - transforms AST into new format
// - finds bindings for all variable references and definitions
//   (previously generated during scope building)
// - labels tail expressions in lambdas
let rec handleExpression scope =
    function
    | IdentifierExpression id -> handleIdentifierExpression scope id
    | LambdaExpression (fs, ds, es) -> handleLambdaExpression scope fs ds es
    | AssignmentExpression binding -> handleAssignment scope binding
    | ProcedureCallExpression (expr, exprs) -> handleProcedureCall scope expr exprs
    | ConditionalExpression (cond, thenBranch, elseBranch) -> handleConditional scope cond thenBranch elseBranch
    | LiteralExpression lit -> AnalysisValueExpression lit
    | Definition binding -> handleDefinition scope binding
    | BeginExpression exprs -> handleBeginExpression scope exprs
    | BooleanExpression (t, exprs) -> handleBooleanExpression scope t exprs
    | ExpressionError err -> failWithErrorNode err
and handleDefinition scope =
    function
    | Binding (id, expr) ->
        let variableRef = IdentifierHelpers.bindingForVariableReference scope id
        let valueExpr = match expr with
                        | LambdaExpression (fs, ds, es) ->
                            handleLambdaExpression scope fs ds es
                        | _ -> handleExpression scope expr
        AnalysisIdentifierDefinition (variableRef, valueExpr)
    | BindingError err -> failWithErrorNode err
and handleConditional scope cond thenBranch elseBranch =
    let condExpr = handleExpression scope cond
    let thenExpr = handleExpression scope thenBranch
    let elseExpr = match elseBranch with
                   | Some e -> handleExpression scope e
                   | None   -> AnalysisUndefinedValue
    AnalysisConditional (condExpr, thenExpr, elseExpr)
and handleProcedureCall scope procExpr exprs =
    let proc = handleExpression scope procExpr
    let args = List.map (handleExpression scope) exprs
    AnalysisProcedureCall (proc, args)
and handleAssignment scope binding =
    match binding with
    | Binding (id, expr) ->
        let variableRef = IdentifierHelpers.bindingForVariableReference scope id
        let valueExpr = handleExpression scope expr
        AnalysisAssignment (variableRef, valueExpr)
    | BindingError err -> failWithErrorNode err
and handleLambdaExpression scope formals defs exprs =
    let body = List.append defs exprs
    let newFormals = LambdaHelpers.convertFormals formals
    let bodyScope = LambdaHelpers.buildLambdaScope scope newFormals
                    |> fun s -> buildScope s body
    let bodyExprs = List.last body
                    |> handleExpression bodyScope
                    |> LambdaHelpers.toTailExpression
                    |> fun e -> [e]
                    |> List.append (List.take (body.Length - 1) body
                                    |> List.map (handleExpression bodyScope))
    let freeVars = LambdaHelpers.collectFreeVariables bodyScope bodyExprs
    { formals = newFormals;
      body = bodyExprs;
      environment = freeVars;
      scope = bodyScope;
      functionName = None;
      isTailRecursive = false;
      isUsedAsFirstClassValue = false;
      isReassigned = false }
    |> AnalysisClosure
and handleBeginExpression scope exprs =
    exprs
    |> List.map (handleExpression scope)
    |> fun es -> AnalysisSequenceExpression (BeginSequence, es)
and handleBooleanExpression scope exprType exprs =
    exprs
    |> List.map (handleExpression scope)
    |> fun es -> match exprType with
                 | AndExpression -> (AndSequence, es)
                 | OrExpression -> (OrSequence, es)
    |> AnalysisSequenceExpression

// Second pass through AST:
// Checks that variables are initialized before they are
// referenced. (Note: e.g. a lambda body may still refer
// to an identifier from an external scope that may be
// uninitialized when that value is referenced. A reference
// like this should probably fail at runtime.)
//
// When there are multiple IdentifierDefinitions for the same name
// on the top level, all IdentifierDefinitions following the first
// one are converted to Assignments.
//
// Either results in an AnalysisException or returns a
// new list of Expressions.
let rec checkVariableInitialization scope exprs =
    let initialized = scope.definitions
                      |> List.fold (fun inits id -> Map.add id.uniqueName false inits)
                                   Map.empty
    let verifyInitialized inits id =
        match Map.tryFind id.uniqueName inits with
        | Some b -> if b then ()
                         else sprintf "Variable %s may be uninitialized" id.name |> AnalysisException |> raise
        | None   -> ()

    let rec check inits expr =
        let recur = check inits >> ignore
        match expr with
        | AnalysisIdentifierDefinition (id, expr) as e ->
            let markAsInitialized = Map.remove id.uniqueName
                                    >> Map.add id.uniqueName true
            recur expr
            match Map.tryFind id.uniqueName inits with
            | Some b -> if b then
                           inits, AnalysisAssignment (id, expr)
                        else
                           markAsInitialized inits, e
            | None -> markAsInitialized inits, e
        | e -> match e with
               | AnalysisVariableReference id -> verifyInitialized inits id
               | AnalysisClosure c -> checkVariableInitialization c.scope c.body |> ignore
               | AnalysisProcedureCall (proc, args) -> recur proc
                                                       List.map recur args |> ignore
               | AnalysisAssignment (id, expr) -> verifyInitialized inits id
                                                  recur expr
               | AnalysisConditional (e1, e2, e3) -> recur e1
                                                     recur e2
                                                     recur e3 |> ignore
               | AnalysisSequenceExpression (t, exprs) -> List.map recur exprs |> ignore
               | AnalysisTailExpression ex -> recur ex
               | _ -> ()

               inits, e
    and checkExprs inits acc =
        function
        | []    -> List.rev acc
        | x::xs -> let newInits, newX = check inits x
                   checkExprs newInits (newX::acc) xs

    checkExprs initialized [] exprs

// Third pass through AST:
// Labels closures with some additional information:
// - whether they are tail recursive
// - whether they are used as first class values or they
//   are bound to variables whose values may change at
//   runtime (the latter possibly necessitating a first
//   class value implementation)
//
// Produces a new list of Expressions. Should not
// result in any exceptions.
let rec labelLambdas exprs =
    let rec label name =
        function
        | AnalysisClosure c -> AnalysisClosure <| labelClosure c name exprs
        | AnalysisAssignment (id, expr)
            -> AnalysisAssignment (id, label None expr)
        | AnalysisIdentifierDefinition (id, expr)
            -> AnalysisIdentifierDefinition (id, label (Some id) expr)
        | AnalysisConditional (e1, e2, e3)
            -> AnalysisConditional (label None e1, label name e2, label name e3)
        | AnalysisProcedureCall (proc, args)
            -> AnalysisProcedureCall (label None proc, List.map (label None) args)
        | AnalysisSequenceExpression (t, exprs)
            -> AnalysisSequenceExpression (t, List.map (label None) exprs)
        | AnalysisTailExpression e
            -> AnalysisTailExpression (label None e)
        | e -> e

    exprs |> List.map (label None)
and labelClosure c name parentScopeExprs =
    let labeledBody = labelLambdas c.body
    let tailRecursive = match name with
                        | None -> false
                        | Some n -> LambdaHelpers.isTailRecursive labeledBody n
    let usedAsFirstClassValue = LambdaHelpers.isUsedAsFirstClassValue name c parentScopeExprs
    let reassigned = match name with
                     | None -> false
                     | Some n -> LambdaHelpers.isReassigned n parentScopeExprs
    { c with body = labeledBody;
             functionName = name;
             isTailRecursive = tailRecursive;
             isUsedAsFirstClassValue = usedAsFirstClassValue;
             isReassigned = reassigned }

// Fourth pass through AST:
// Verifies that procedure calls are given the correct number
// of arguments when possible (when procedures are not used as first
// class values).
//
// Either raises an AnalysisException (when errors found) or produces ()
// (when all checks passed).
//
// Note: procedures potentially used as first class values and
// procedure bindings whose value may be changed by set! are
// ignored. These will probably be checked at runtime (TODO?).
let checkProcedureCalls exprs =
    let findFunctionDefinitions exprs =
        exprs
        |> List.filter (function
                        | AnalysisIdentifierDefinition (_, expr) ->
                            match expr with
                            | AnalysisClosure c -> c.functionName.IsSome && (not c.isReassigned)
                            | _ -> false
                        | _ -> false)
        |> List.map (fun e -> match e with
                              | AnalysisIdentifierDefinition (_, AnalysisClosure c) -> c
                              | _ -> assert false
                                     "Analysis failure" |> AnalysisException |> raise)
        |> List.fold (fun m c -> Map.add c.functionName.Value.uniqueName c m)
                     Map.empty

    let checkNumArgs f s functionName expected got =
        if f got expected then
            sprintf s functionName expected got
            |> AnalysisException
            |> raise

    let checkNumArgsEqual =
        checkNumArgs (<>) "Arity mismatch for function %s: expected %i arguments, got %i"

    let checkNumArgsGreaterThan =
        checkNumArgs (<) "Arity mismatch for function %s: expected at least %i arguments, got %i"

    let checkProcedureCall functionDefs proc numArgs =
        match proc with
        | AnalysisVariableReference id ->
            match Map.tryFind id IdentifierHelpers.builtIns with
            | Some (IdentifierHelpers.SetNumberOfArgs n) -> checkNumArgsEqual id.name n numArgs
            | Some IdentifierHelpers.VarArgs -> ()
            | Some (IdentifierHelpers.VarArgsAtLeast n) -> checkNumArgsGreaterThan id.name n numArgs
            | None ->
                match Map.tryFind id.uniqueName functionDefs with
                | Some c ->
                    match c.formals with
                    | MultiArgFormals ids -> checkNumArgsEqual id.name ids.Length numArgs
                    | SingleArgFormals _ -> ()
                | _ -> ()
        | _ -> ()

    let rec checkCalls functionDefs e =
        let recur = checkCalls functionDefs
        match e with
        | AnalysisIdentifierDefinition (_, expr)
        | AnalysisAssignment (_, expr)
            -> recur expr
        | AnalysisClosure c ->
            let newFunctions = findFunctionDefinitions c.body
            newFunctions
            |> Map.fold (fun m k v -> Map.add k v m)
                        functionDefs
            |> fun fs -> List.map (checkCalls fs) c.body |> ignore
        | AnalysisConditional (e1, e2, e3) -> recur e1
                                              recur e2
                                              recur e3 |> ignore
        | AnalysisSequenceExpression (_, exprs) -> List.map recur exprs |> ignore
        | AnalysisTailExpression expr -> recur expr
        | AnalysisProcedureCall (proc, args) ->
            checkProcedureCall functionDefs proc args.Length
            List.map recur args |> ignore
        | _ -> ()

    let functions = findFunctionDefinitions exprs

    exprs
    |> List.map (checkCalls functions)
    |> ignore

let isFunctionDefinition =
    function
    | AnalysisIdentifierDefinition (_, AnalysisClosure _) -> true
    | _ -> false

let getValuesFromOptionList (exprs : 'a option list) =
    exprs
    |> List.filter Option.isSome
    |> List.map (fun opt -> opt.Value)

// This phase finalizes the AST representation:
// - Variable definitions and procedure definitions
//   are separated from expressions. (Procedure
//   definition = an identifier definition that
//   directly defines a procedure: the final
//   procedure value is known at compile time.
// - Variable definitions are split into a declaration
//   and an assignment.
let rec convertModuleOrProcedureBody exprs =
    let functionDefinitions = exprs
                              |> List.map toProcedureDefinition
                              |> getValuesFromOptionList
    let variableDeclarations = exprs
                               |> List.map toVariableDeclaration
                               |> getValuesFromOptionList
    let otherExpressions = exprs
                           |> List.filter (isFunctionDefinition >> not)
                           |> List.map convertExpression
    (functionDefinitions, variableDeclarations, otherExpressions)
and convertClosure c =
    let procs, vars, exprs = convertModuleOrProcedureBody c.body
    let name = match c.functionName with
               | Some n -> n
               | None -> let generatedName = IdentifierHelpers.generateAnonymousProcedureName ()
                         { name = generatedName; uniqueName = generatedName; argIndex = None }
    { formals = c.formals;
      procedureDefinitions = procs;
      variableDeclarations = vars;
      body = exprs
      environment = c.environment;
      scope = c.scope;
      isTailRecursive = c.isTailRecursive;
      functionName = name;
      isUsedAsFirstClassValue = c.isUsedAsFirstClassValue;
      isReassigned = c.isReassigned }
and convertExpression =
    function
    | AnalysisVariableReference id -> VariableReference id
    | AnalysisClosure c -> convertClosure c |> Closure
    | AnalysisProcedureCall (proc, args) -> let finalProc = convertExpression proc
                                            let finalArgs = List.map convertExpression args
                                            ProcedureCall (finalProc, finalArgs, false)
    | AnalysisValueExpression lit -> ValueExpression lit
    | AnalysisAssignment (id, expr)
    | AnalysisIdentifierDefinition (id, expr)
        -> Assignment (id, convertExpression expr)
    | AnalysisConditional (cond, thenExpr, elseExpr)
        -> let finalCond = convertExpression cond
           let finalThen = convertExpression thenExpr
           let finalElse = convertExpression elseExpr
           Conditional (finalCond, finalThen, finalElse)
    | AnalysisSequenceExpression (t, exprs)
        -> SequenceExpression (t, List.map convertExpression exprs)
    | AnalysisTailExpression expr
        -> let e = convertExpression expr
           match e with
           | ProcedureCall (p, args, isTailCall) -> ProcedureCall (p, args, true)
           | other -> other
    | AnalysisUndefinedValue
        -> UndefinedValue
and toProcedureDefinition =
    function
    | AnalysisIdentifierDefinition (id, AnalysisClosure c)
        -> Some <| ProcedureDefinition (id, convertClosure c)
    | _ -> None
and toVariableDeclaration =
    function
    | AnalysisIdentifierDefinition (id, expr)
        -> match expr with
           | AnalysisClosure _ -> None
           | _ -> Some <| VariableDeclaration id
    | _ -> None

// Fifth pass through AST.
//
// Finds top level procedures that are also used as first class values or the
// values of which are changed with set!.
//
// For each of these procedures, this phase introduces a new top level variable
// to hold the closure object. All first class uses of the procedure name are changed
// to refer to this new variable. If the value is changed with set!, procedure
// calls are also changed to refer to this closure object.
let rebindTopLevelFirstClassProcedureReferences (procs, vars, exprs) =
    let isFirstClassProcedure (ProcedureDefinition (_, clos)) = clos.isUsedAsFirstClassValue
    let isReassigned (ProcedureDefinition (_, clos)) = clos.isReassigned
    let procedureIdPair (ProcedureDefinition (id, _)) =
        let procedureObjectId = { name = id.name; uniqueName = SymbolGenerator.toProcedureObjectName id.uniqueName; argIndex = None }
        (id, procedureObjectId)

    let proceduresRequiringAClosure = procs |> List.filter (fun e -> isFirstClassProcedure e || isReassigned e)
    let reassignedProcedureIds = procs
                                 |> List.filter isReassigned
                                 |> List.map (fun (ProcedureDefinition (id, _)) -> id)
    let oldAndNewIds = proceduresRequiringAClosure |> List.map procedureIdPair
    let newVariableDeclarations = oldAndNewIds
                                  |> List.map (fun (_, newId) -> newId)
                                  |> List.map VariableDeclaration
    let newIdFor id = match List.tryFind (fun (oldId, _) -> oldId = id) oldAndNewIds with
                      | Some (_, newId) -> newId
                      | None -> id
    let rec rebind =
        function
        | ProcedureCall (proc, args, isTailCall) ->
            let newProc =
                match proc with
                | VariableReference id as e
                    -> if List.contains id reassignedProcedureIds then
                           rebind proc
                       else
                           proc
                | e -> rebind e
            ProcedureCall (newProc, List.map rebind args, isTailCall)
        | Conditional (cond, thenBranch, elseBranch) -> Conditional (rebind cond, rebind thenBranch, rebind elseBranch)
        | SequenceExpression (seqType, exprs) -> SequenceExpression (seqType, List.map rebind exprs)
        | Assignment (id, expr) -> Assignment (newIdFor id, rebind expr)
        | VariableReference id -> VariableReference (newIdFor id)
        | Closure clos -> let newClos = { clos with body = List.map rebind clos.body
                                                    environment = List.map newIdFor clos.environment }
                          Closure newClos
        | ValueExpression _ | UndefinedValue as e -> e

    let exprsAfterRebinding = List.map rebind exprs

    (procs, List.append vars newVariableDeclarations, exprsAfterRebinding)

// Performs several passes through the AST (passes documented
// above) producing a Program value (either ValidProgram or
// ProgramAnalysisError).
//
// TODO: prevent attempts to set constants (for simplicity)
let analyse exprs =
    try
        let builtInNames = Map.toList IdentifierHelpers.builtIns
                           |> List.map (fun (k, v) -> k)
        let builtInScope = { definitions = builtInNames; parent = None }
        let topLevelScope = buildScope builtInScope exprs
        let moduleBody = exprs
                         |> List.map (handleExpression topLevelScope)
                         |> checkVariableInitialization topLevelScope
                         |> labelLambdas

        checkProcedureCalls moduleBody

        let procs, vars, exprs = moduleBody
                                 |> convertModuleOrProcedureBody
                                 |> rebindTopLevelFirstClassProcedureReferences

        ValidProgram { procedureDefinitions = procs;
                       variableDeclarations = vars;
                       expressions = exprs;
                       scope = topLevelScope }
    with
        | AnalysisException msg -> ProgramAnalysisError msg
