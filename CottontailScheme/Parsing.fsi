module CottontailScheme.Parsing

open Literals

type ParsePosition = { line: int64; column: int64 }

type CTExpression =
    | CTIdentifierExpression of ParsePosition * string
    | CTLiteralExpression of ParsePosition * LiteralValue
    | CTListExpression of ParsePosition * CTExpression list

type CTProgram = | CTProgram of CTExpression list

val parseDatum : FParsec.Primitives.Parser<LiteralValue,unit>
val parseExpression : FParsec.Primitives.Parser<CTExpression,unit>
val parseProgram : FParsec.Primitives.Parser<CTProgram,unit>
