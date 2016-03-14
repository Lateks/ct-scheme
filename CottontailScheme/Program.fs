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

let parseBoolean = skipChar '#' >>. (((pstring "true" <|> pstring "t") >>% CTBool true)
                                <|> ((pstring "false" <|> pstring "f") >>% CTBool false))

test parseBoolean "#true"
test parseBoolean "#f"
test parseBoolean "#a"

// TODO: big numbers?
// TODO: parse integers separately?
let parseNumber =
    let numberFormat =     NumberLiteralOptions.AllowMinusSign
                       ||| NumberLiteralOptions.AllowPlusSign
                       ||| NumberLiteralOptions.AllowFraction
    numberLiteral numberFormat "number"
    |>> fun nl -> CTNumber (float nl.String)

test parseNumber "3.14159"
test parseNumber "+5"
test parseNumber "-42"

let parseStringLiteral =
    let escape = anyOf "\"\\tnr"
                 |>> function
                     | 't' -> "\t"
                     | 'n' -> "\n"
                     | 'r' -> "\r"
                     |  c  -> string c
    let escapedChar = pstring "\\" >>. escape
    let normalCharSeq = manySatisfy (fun c -> c <> '"' && c <> '\\')
    between (pstring "\"") (pstring "\"")
            (stringsSepBy normalCharSeq escapedChar)
    |>> CTString

test parseStringLiteral "\"\""
test parseStringLiteral "\"foobar\""
test parseStringLiteral "\"lol\\\\bal\\n\\r\\t\\\"foo\\\"\""

// This parser currently accepts a subset of the
// identifiers specified by the R7RS grammar.
let parseSymbolOrIdentifier =
    let isSpecialChar = isAnyOf "!$%&*/:<=>?^_~"
    let isExplicitSign = isAnyOf "+-"
    let isInitialChar c = isAsciiLetter c || isSpecialChar c
    let isSubsequentChar c = isInitialChar c || isDigit c || isExplicitSign c
    let isSignSubsequent c = isInitialChar c || isExplicitSign c
    let regularIdentifierFormat = IdentifierOptions(isAsciiIdStart = isInitialChar,
                                                    isAsciiIdContinue = isSubsequentChar)
    let parseRegularIdentifier = identifier regularIdentifierFormat
    let parsePeculiarIdentifier = manyStrings2 (many1Chars2 (satisfy isExplicitSign) (satisfy isSignSubsequent))
                                               parseRegularIdentifier
    parseRegularIdentifier <|> parsePeculiarIdentifier

test parseSymbolOrIdentifier "+"
test parseSymbolOrIdentifier "-"
test parseSymbolOrIdentifier ">"
test parseSymbolOrIdentifier ">10"
test parseSymbolOrIdentifier "fib"
test parseSymbolOrIdentifier "call-with-current-continuation"
test parseSymbolOrIdentifier "+$30"
test parseSymbolOrIdentifier "+fib+"
test parseSymbolOrIdentifier "42" // should return empty string
test parseSymbolOrIdentifier "+1" // should return +

let parseSymbol = parseSymbolOrIdentifier |>> CTSymbol

let parseDatum, parseDatumRef = createParserForwardedToRef<CTDatum, unit>()

let wspace = spaces

let parseList = between (pstring "(") (pstring ")")
                        (wspace >>. sepBy parseDatum spaces1 .>> wspace)
                |>> CTList

do parseDatumRef := choice [parseList
                            parseBoolean
                            parseNumber
                            parseStringLiteral
                            parseSymbol]

test parseList "(1 2 3)"
test parseList "((1 2 3) (a b c))"
test parseDatum "+3.14159"
test parseDatum "\"Hello, world!\""
test parseDatum "#true"
test parseDatum "call-with-current-continuation"
test parseDatum "+"
