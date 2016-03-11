open FParsec

type CTDatum = CTBool of bool
             | CTNumber of float
             | CTString of string
             | CTSymbol of string
             | CTList of CTDatum list

type CTIdentifier = CTIdentifier of string

type CTLambdaExpressionFormals = CTFormalsSingle of CTIdentifier
                               | CTFormalsList of CTIdentifier list

type CTExpression = CTIdentifierExpression of CTIdentifier
                  | CTLiteralExpression of CTDatum
                  | CTProcedureCallExpression of CTExpression * CTExpression list
                  | CTLambdaExpression of CTLambdaExpressionFormals * CTDefinition list * CTExpression list
                  | CTConditionalExpression of CTExpression * CTExpression * CTExpression option
                  | CTAssignmentExpression of CTIdentifier * CTExpression

and CTDefinition = CTDefinition of CTIdentifier * CTExpression

type CTTopLevelCommand = CTTopLevelDefinition of CTDefinition
                       | CTTopLevelExpression of CTExpression

type CTProgram = CTProgram of CTTopLevelCommand list

let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

let str s = pstring s
