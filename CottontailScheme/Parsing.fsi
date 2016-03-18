module CottontailScheme.Parsing

type CTDatum =
    | CTBool of bool
    | CTNumber of float
    | CTString of string
    | CTSymbol of string
    | CTList of CTDatum list

type CTExpression =
    | CTIdentifierExpression of string
    | CTLiteralExpression of CTDatum
    | CTListExpression of CTExpression list

type CTProgram = | CTProgram of CTExpression list

val parseDatum : FParsec.Primitives.Parser<CTDatum,unit>
val parseExpression : FParsec.Primitives.Parser<CTExpression,unit>
val parseProgram : FParsec.Primitives.Parser<CTProgram,unit>
