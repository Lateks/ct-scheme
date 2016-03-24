module CottontailScheme.ASTBuilder

open CottontailScheme.Parsing

type ASTError = { message: string; position: ParsePosition }

type Identifier = Identifier of string
                | IdentifierError of ASTError

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
    | ExpressionError of ASTError
and Binding = Binding of Identifier * Expression
            | BindingError of ASTError

type ASTBuildStatus = ASTBuildSuccess of Expression list
                    | ASTBuildFailure of ASTError list

let specialFunctions = ["define"; "if"; "lambda"; "set!"] //; "cons"; "car"; "cdr"; "list"; "quote"; "display"]

let isSpecialFunction name = List.contains name specialFunctions

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

let buildBinding pos name =
    function
    | ident::expr::[] ->
        match ident with
        | IdentifierExpression id ->
            match id with
            | Identifier name
                -> if isSpecialFunction name then
                       BindingError { message = sprintf "Redefining built-in procedure %s" name;
                                      position = pos }
                   elif isDefinition expr then
                       BindingError { message = "Procedure define used in a context where an expression was expected";
                                      position = pos }
                   else
                       Binding (id, expr)
            | IdentifierError msg
                -> BindingError msg
        | _ -> BindingError { message = sprintf "Not an identifier: %A" ident;
                              position = pos }
    | _  -> BindingError { message = sprintf "Invalid number of arguments to %s" name;
                           position = pos }

let buildDefinition pos = buildBinding pos "define" >> Definition
let buildAssignment pos = buildBinding pos "set!" >> AssignmentExpression

let buildConditionalWith pos cond thenExpr elseExpr =
    match cond, thenExpr, elseExpr with
    | (Definition _, _, _)
    | (_, Definition _, _)
    | (_, _, Some (Definition _))
        -> ExpressionError { message = "Procedure define used in a context where an expression was expected";
                             position = pos }
    | _ -> ConditionalExpression (cond, thenExpr, elseExpr)

let buildConditional pos =
    function
    | cond::thenExpr::[] -> buildConditionalWith pos cond thenExpr None
    | cond::thenExpr::elseExpr::[] -> buildConditionalWith pos cond thenExpr (Some elseExpr)
    | _ -> ExpressionError { message = "Invalid number of arguments to if"; position = pos }

let buildLambdaWith pos formals body =
    let definitions = body |> List.takeWhile isDefinition
    let expressions = body |> List.skip definitions.Length
    if (expressions |> List.filter isDefinition |> fun lst -> not lst.IsEmpty) then
        ExpressionError { message = "Definitions must be in the beginning of the lambda body";
                          position = pos }
    elif (expressions.IsEmpty) then
        ExpressionError { message = "Lambda body contains no expressions";
                          position = pos }
    else
        LambdaExpression (formals, definitions, expressions)

// TODO: more exact error positions
// TODO: printing datum objects properly in error messages
// TODO: identify tailcalls
// TODO: identify tail recursive calls
// TODO: identify invalid uses of set!
let rec buildFromList pos =
    function
    | []    -> ExpressionError { message = "Empty procedure call expressions are not allowed";
                                 position = pos }
    | x::xs -> let args = lazy buildFromExprList xs
               let buildProcCall proc = ProcedureCallExpression (proc, args.Value)
               let buildCallToIdentifier pos =
                   function
                   | "define" -> buildDefinition pos args.Value
                   | "if" -> buildConditional pos args.Value
                   | "lambda" -> buildLambda pos xs
                   | "set!" -> buildAssignment pos args.Value
                   | x -> buildFromIdentifier x |> buildProcCall
               match x with
               | CTIdentifierExpression (pos, id) -> buildCallToIdentifier pos id
               | CTLiteralExpression (pos, datum) -> ExpressionError { message = sprintf "Not a procedure: %A" datum;
                                                                       position = pos }
               | CTListExpression (pos, l) -> buildFromList pos l |> buildProcCall
and buildFromExpression = function
                          | CTIdentifierExpression (_, id) -> buildFromIdentifier id
                          | CTLiteralExpression (_, datum) -> buildFromDatum datum
                          | CTListExpression (pos, exprs) -> buildFromList pos exprs
and buildFromExprList = List.map buildFromExpression
and buildLambda pos =
    function
    | args::[] -> ExpressionError { message = "Lambda body is empty"; position = pos }
    | args::body ->
        let bodyExpressions = buildFromExprList body
        let build f = buildLambdaWith pos f bodyExpressions
        match args with
        | CTIdentifierExpression (_, id) -> id |> Identifier |> SingleArgFormals |> build
        | CTListExpression (formalsPos, lst) ->
            lst |> List.map (function
                             | CTIdentifierExpression (_, id) -> Identifier id
                             | expr -> IdentifierError { message = "Invalid syntax in lambda expression";
                                                         position = formalsPos })
                |> MultiArgFormals
                |> build
        | CTLiteralExpression (litPos, _) ->
            ExpressionError { message = "Invalid syntax in lambda expression";
                              position = litPos }
    | [] -> ExpressionError { message = "Invalid lambda syntax: missing arguments and body";
                              position = pos }

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
                 | ListLiteral elist -> listErrors elist
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
        ASTBuildSuccess ast
    else
        ASTBuildFailure errors
