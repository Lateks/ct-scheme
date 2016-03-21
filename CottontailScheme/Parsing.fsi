module CottontailScheme.Parsing

type ParsePosition = { line: int64; column: int64 }

type CTDatum =
    | CTBool of bool
    | CTNumber of float
    | CTString of string
    | CTSymbol of string
    | CTList of CTDatum list

type CTExpression =
    | CTIdentifierExpression of ParsePosition * string
    | CTLiteralExpression of ParsePosition * CTDatum
    | CTListExpression of ParsePosition * CTExpression list

type CTProgram = | CTProgram of CTExpression list

val parseDatum : FParsec.Primitives.Parser<CTDatum,unit>
val parseExpression : FParsec.Primitives.Parser<CTExpression,unit>
val parseProgram : FParsec.Primitives.Parser<CTProgram,unit>
