module CottontailScheme.Analysis

open CottontailScheme.Parsing

type AnalysisError = { message: string; } //row: int; col: int }

type Identifier = Identifier of string
                | IdentifierError of AnalysisError

type LambdaFormals = MultiArgFormals of Identifier list
                   | SingleArgFormals of Identifier

type Expression =
    | IdentifierExpression of Identifier
    | BooleanLiteral of bool
    | NumberLiteral of float
    | StringLiteral of string
    | SymbolLiteral of string
    | ListLiteral of Expression list
    | LambdaExpression of LambdaFormals * Expression list * Expression list
    | AssignmentExpression of Binding
    | ProcedureCallExpression of Expression * Expression list
    | ConditionalExpression of Expression * Expression * Expression option
    | Definition of Binding
    | ExpressionError of AnalysisError
and Binding = Binding of Identifier * Expression
            | BindingError of AnalysisError

type AnalysisStatus = AnalysisSuccess of Expression list
                    | AnalysisFailure of AnalysisError list

let specialFunctions = ["define"; "if"; "lambda"; "set!"] //; "cons"; "car"; "cdr"; "list"; "quote"; "display"]

let isSpecialFunction name = List.contains name specialFunctions

// TODO: alpha conversion
let buildFromIdentifier = Identifier >> IdentifierExpression

let rec buildFromDatum = function
                         | CTBool b -> BooleanLiteral b
                         | CTNumber f -> NumberLiteral f
                         | CTString s -> StringLiteral s
                         | CTSymbol s -> SymbolLiteral s
                         | CTList l -> l |> List.map buildFromDatum |> ListLiteral

let isDefinition =
    function
    | Definition _ -> true
    | _ -> false

let buildBinding name =
    function
    | ident::expr::[] ->
        match ident with
        | IdentifierExpression id ->
            match id with
            | Identifier name
                -> if isSpecialFunction name then
                       BindingError { message = sprintf "Redefining built-in procedure %s" name }
                   elif isDefinition expr then
                       BindingError { message = "Procedure define used in a context where an expression was expected" }
                   else
                       Binding (id, expr)
            | IdentifierError msg
                -> BindingError msg
        | _ -> BindingError {message = sprintf "Not an identifier: %A" ident }
    | _  -> BindingError { message = sprintf "Invalid number of arguments to %s" name }

let buildDefinition = buildBinding "define" >> Definition
let buildAssignment = buildBinding "set!" >> AssignmentExpression

let buildConditionalWith cond thenExpr elseExpr =
    match cond, thenExpr, elseExpr with
    | (Definition _, _, _)
    | (_, Definition _, _)
    | (_, _, Some (Definition _))
        -> ExpressionError { message = "Procedure define used in a context where an expression was expected" }
    | _ -> ConditionalExpression (cond, thenExpr, elseExpr)

let buildConditional =
    function
    | cond::thenExpr::[] -> buildConditionalWith cond thenExpr None
    | cond::thenExpr::elseExpr::[] -> buildConditionalWith cond thenExpr (Some elseExpr)
    | _ -> ExpressionError { message = "Invalid number of arguments to if" }

let buildLambdaWith formals body =
    let definitions = body |> List.takeWhile isDefinition
    let expressions = body |> List.skip definitions.Length
    if (expressions |> List.filter isDefinition |> fun lst -> not lst.IsEmpty) then
        ExpressionError { message = "Definitions must be in the beginning of the lambda body" }
    else
        LambdaExpression (formals, definitions, body)

// TODO: proper error handling and error positions
// TODO: printing datum objects properly
// TODO: identify tailcalls
// TODO: identify tail recursive calls
let rec buildFromList =
    function
    | []    -> ExpressionError { message = "Empty procedure call expressions are not allowed" }
    | x::xs -> let args = lazy buildFromExprList xs
               let buildProcCall proc = ProcedureCallExpression (proc, args.Value)
               let buildCallToIdentifier =
                   function
                   | "define" -> buildDefinition args.Value
                   | "if" -> buildConditional args.Value
                   | "lambda" -> buildLambda xs
                   | "set!" -> buildAssignment args.Value
                   | x -> buildFromIdentifier x |> buildProcCall
               match x with
               | CTIdentifierExpression id -> buildCallToIdentifier id
               | CTLiteralExpression datum -> ExpressionError { message = sprintf "Not a procedure: %A" datum }
               | CTListExpression l -> buildFromList l |> buildProcCall
and buildFromExpression = function
                          | CTIdentifierExpression id -> buildFromIdentifier id
                          | CTLiteralExpression datum -> buildFromDatum datum
                          | CTListExpression exprs -> buildFromList exprs
and buildFromExprList = List.map buildFromExpression
and buildLambda =
    function
    | args::[] -> ExpressionError { message = "Lambda body is empty" }
    | args::body ->
        let bodyExpressions = buildFromExprList body
        let build f = buildLambdaWith f bodyExpressions
        match args with
        | CTIdentifierExpression id -> id |> Identifier |> SingleArgFormals |> build
        | CTListExpression lst ->
            lst |> List.map (function
                             | CTIdentifierExpression id -> Identifier id
                             | expr -> IdentifierError { message = sprintf "Invalid identifier used in lambda expression %A" expr })
                |> MultiArgFormals
                |> build
        | CTLiteralExpression datum -> ExpressionError { message = sprintf "Invalid identifier used in lambda expression: %A" datum }
    | [] -> ExpressionError { message = "Invalid lambda syntax: missing arguments and body" }

let rec listErrors exprs =
    let getIdError = function
                     | IdentifierError msg -> [msg]
                     | _ -> []
    let getErrorsFromFormals = function
                               | MultiArgFormals ids -> List.map getIdError ids
                                                     |> List.concat
                               | SingleArgFormals id -> getIdError id
    let getBindingError = function
                          | BindingError msg -> [msg]
                          | _ -> []
    exprs
    |> List.map (function
                 | ExpressionError msg -> [msg]
                 | IdentifierExpression id -> getIdError id
                 | ListLiteral elist -> listErrors exprs
                 | LambdaExpression (formals, defs, exprs) ->
                    let idErrors = getErrorsFromFormals formals
                    let defErrors = listErrors defs
                    let exprErrors = listErrors exprs
                    List.concat [idErrors; defErrors; exprErrors]
                 | AssignmentExpression binding
                 | Definition binding -> getBindingError binding
                 | ProcedureCallExpression (expr, exprs) ->
                    listErrors <| expr::exprs
                 | ConditionalExpression (cond, thenBranch, elseBranch) ->
                    match elseBranch with
                    | Some expr ->
                        cond::thenBranch::[expr]
                    | None ->
                        cond::[thenBranch]
                    |> listErrors
                 | _ -> []
                )
    |> List.concat

let buildAST (CTProgram lst) =
    let ast = buildFromExprList lst
    let errors = listErrors ast
    if errors.IsEmpty then
        AnalysisSuccess ast
    else
        AnalysisFailure errors
