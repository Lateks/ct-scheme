module CottontailScheme.ASTBuilder

open CottontailScheme.Parsing
open CottontailScheme.Literals

type ASTError = { message: string; position: ParsePosition }

type Identifier = Identifier of string
                | IdentifierError of ASTError

type LambdaFormals = MultiArgFormals of Identifier list
                   | SingleArgFormals of Identifier

type BooleanExprType = AndExpression | OrExpression

type Expression =
    | IdentifierExpression of Identifier
    | LiteralExpression of LiteralValue
    | LambdaExpression of LambdaFormals * Expression list * Expression list
    | AssignmentExpression of Binding
    | ProcedureCallExpression of Expression * Expression list
    | ConditionalExpression of Expression * Expression * Expression option
    | Definition of Binding
    | BeginExpression of Expression list
    | ExpressionError of ASTError
    | BooleanExpression of BooleanExprType * Expression list
and Binding = Binding of Identifier * Expression
            | BindingError of ASTError

type ASTBuildStatus = ASTBuildSuccess of Expression list
                    | ASTBuildFailure of ASTError list

let specialFunctions = ["define"; "if"; "lambda"; "set!"; "and"; "or"; "quote"; "begin"; "zero?"; "null?"; "+"; "-"; "*"; "/"; "<"; ">"; "eq?"; "list"; "display"; "car"; "cdr"; "cons"; "not"; "newline"]

let argumentNumberLimit = 5

let isSpecialFunction name = List.contains name specialFunctions

let buildFromIdentifier = Identifier >> IdentifierExpression

let isDefinition =
    function
    | Definition _ -> true
    | _ -> false

let rec toCottontailSchemeType datum : CottontailSchemeLib.CTObject =
    match datum with
    | Number n -> CottontailSchemeLib.CTNumber n :> CottontailSchemeLib.CTObject
    | String s -> CottontailSchemeLib.CTString s :> CottontailSchemeLib.CTObject
    | Symbol s -> CottontailSchemeLib.CTSymbol s :> CottontailSchemeLib.CTObject
    | Boolean b -> if b then CottontailSchemeLib.Constants.True else CottontailSchemeLib.Constants.False
    | List l -> List.map toCottontailSchemeType l
                |> List.toArray
                |> CottontailSchemeLib.BuiltIns.List

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
        | _ -> let makeErrorMessage obj = sprintf "Not an identifier: %A" obj
               let message = match ident with
                             | LiteralExpression lit -> toCottontailSchemeType lit |> makeErrorMessage
                             | e -> makeErrorMessage e
               BindingError { message = message;
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

let containsDefinitions exprs =
    exprs
    |> List.filter isDefinition
    |> fun lst -> not lst.IsEmpty

let checkExpressionListForErrors definitionErrorMsg emptyListErrorMsg pos exprs =
    if (containsDefinitions exprs) then
        Some { message = definitionErrorMsg; position = pos }
    elif (exprs.IsEmpty) then
        Some { message = emptyListErrorMsg; position = pos }
    else
        None

let buildLambdaWith pos formals body =
    let definitions = body |> List.takeWhile isDefinition
    let expressions = body |> List.skip definitions.Length
    expressions
    |> checkExpressionListForErrors "Definitions must be in the beginning of the lambda body"
                                    "Lambda body contains no expressions"
                                    pos
    |> function
       | Some err -> ExpressionError err
       | None -> LambdaExpression (formals, definitions, expressions)

let buildBeginBlock pos exprs=
    exprs
    |> checkExpressionListForErrors "A begin block may not introduce new variables"
                                    "Empty begin block"
                                    pos
    |> function
       | Some err -> ExpressionError err
       | None -> BeginExpression exprs

let buildBooleanExpression pos exprType exprs =
    if (containsDefinitions exprs) then
        ExpressionError { message = "Procedure define used in a context where an expression was expected";
                          position = pos }
    else
        BooleanExpression (exprType, exprs)

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
                   | "begin" -> buildBeginBlock pos args.Value
                   | "and"   -> buildBooleanExpression pos AndExpression args.Value
                   | "or"    -> buildBooleanExpression pos OrExpression args.Value
                   | x -> buildFromIdentifier x |> buildProcCall
               match x with
               | CTIdentifierExpression (pos, id) -> buildCallToIdentifier pos id
               | CTLiteralExpression (pos, datum)
                  -> let repr = datum |> toCottontailSchemeType
                     ExpressionError { message = sprintf "Not a procedure: %A" repr;
                                       position = pos }
               | CTListExpression (pos, l) -> buildFromList pos l |> buildProcCall
and buildFromExpression = function
                          | CTIdentifierExpression (_, id) -> buildFromIdentifier id
                          | CTLiteralExpression (_, datum) -> LiteralExpression datum
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
            if List.length lst > argumentNumberLimit then
                ExpressionError { message = sprintf "Too many arguments in procedure definition: number of arguments is limited to %i" argumentNumberLimit
                                  position = formalsPos }
            else
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

    exprs
    |> List.map (function
                 | ExpressionError msg -> [msg]
                 | IdentifierExpression id -> getIdError id
                 | LiteralExpression elist -> []
                 | LambdaExpression (formals, defs, exprs) ->
                    let idErrors = getErrorsFromFormals formals
                    let defErrors = listErrors defs
                    let exprErrors = listErrors exprs
                    List.concat [idErrors; defErrors; exprErrors]
                 | AssignmentExpression binding
                 | Definition binding ->
                      match binding with
                      | BindingError msg -> [msg]
                      | Binding (_, expr) -> listErrors [expr]
                 | ProcedureCallExpression (expr, exprs) ->
                    listErrors <| expr::exprs
                 | ConditionalExpression (cond, thenBranch, elseBranch) ->
                    match elseBranch with
                    | Some expr ->
                        cond::thenBranch::[expr]
                    | None ->
                        cond::[thenBranch]
                    |> listErrors
                 | BeginExpression exprs
                 | BooleanExpression (_, exprs)
                    -> listErrors exprs
                )
    |> List.concat

let buildAST (CTProgram lst) =
    let ast = buildFromExprList lst
    let errors = listErrors ast
    if errors.IsEmpty then
        ASTBuildSuccess ast
    else
        ASTBuildFailure errors
