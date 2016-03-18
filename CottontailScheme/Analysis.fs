module CottontailScheme.Analysis

open CottontailScheme.Parsing

type Identifier = Identifier of string

type Expression =
    | IdentifierExpression of Identifier
    | BooleanLiteral of bool
    | NumberLiteral of float
    | StringLiteral of string
    | SymbolLiteral of string
    | ListLiteral of Expression list
    | LambdaExpression of Identifier * Expression list * Expression list
    | AssignmentExpression of Identifier * Expression
    | ProcedureCallExpression of Expression * Expression list
    | Definition of Identifier * Expression

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

let hasUndefinedReturnValue =
    function
    | Definition _
    | AssignmentExpression _ -> true
    | _ -> false

// TODO: prevent redefining built-in functions?
let buildDefinition =
    function
    | x::y::[] ->
        match x with
        | IdentifierExpression id ->
            let (Identifier name) = id
            if isSpecialFunction name then
                failwithf "Redefining built-in function %s" name
            elif hasUndefinedReturnValue y then
                failwithf "Function define or set! used in unexpected context"
            else
                Definition (id, y)
        | _ -> failwith "Invalid identifier: %A" x
    | _        -> failwith "Invalid number of arguments to define"

// TODO: proper error handling and error positions
// TODO: printing datum objects properly
// TODO: identify tailcalls
// TODO: identify tail recursive calls
let rec buildFromList =
    function
    | []    -> failwith "Empty procedure call expressions are not allowed"
    | x::xs -> let args = buildFromExprList xs
               let buildProcCall proc = ProcedureCallExpression (proc, args)
               let buildCallToIdentifier =
                   function
                   | "define" -> buildDefinition args
                   | "if" -> failwith "Not implemented yet: if"
                   | "lambda" -> failwith "Not implemented yet: lambda"
                   | "set!" -> failwith "Not implemented yet: set!"
                   | "quote" -> failwith "Not implemented yet: quote"
                   | x -> buildFromIdentifier x |> buildProcCall
               match x with
               | CTIdentifierExpression id -> buildCallToIdentifier id
               | CTLiteralExpression datum -> failwithf "Not a procedure: %A" datum
               | CTListExpression l -> buildFromList l |> buildProcCall
and buildFromExpression = function
                          | CTIdentifierExpression id -> buildFromIdentifier id
                          | CTLiteralExpression datum -> buildFromDatum datum
                          | CTListExpression exprs -> buildFromList exprs
and buildFromExprList = List.map buildFromExpression

let buildAST (CTProgram lst) = buildFromExprList lst
