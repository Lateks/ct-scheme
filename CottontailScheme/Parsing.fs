namespace CottontailScheme

open FParsec

type CTDatum = CTBool of bool
             | CTNumber of float
             | CTString of string
             | CTSymbol of string
             | CTList of CTDatum list

type CTIdentifier = CTIdentifier of string

type CTExpression = CTIdentifierExpression of CTIdentifier
                  | CTLiteralExpression of CTDatum
                  | CTListExpression of CTExpression list

type CTProgram = CTProgram of CTExpression list

module Parsing =
    let parseBoolean = skipChar '#' >>. (((pstring "true" <|> pstring "t") >>% CTBool true)
                                <|> ((pstring "false" <|> pstring "f") >>% CTBool false))

    // TODO: big numbers?
    // TODO: parse integers separately?
    let parseNumber =
        let numberFormat =     NumberLiteralOptions.AllowMinusSign
                           ||| NumberLiteralOptions.AllowPlusSign
                           ||| NumberLiteralOptions.AllowFraction
        numberLiteral numberFormat "number"
        |>> fun nl -> CTNumber (float nl.String)

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

    // This parser currently accepts a subset of the
    // identifiers specified by the R7RS grammar.
    let private parseSymbolOrIdentifier =
        let isSpecialChar = isAnyOf "!$%&*/:<=>?^_~"
        let isExplicitSign = isAnyOf "+-"
        let isInitialChar c = isAsciiLetter c || isSpecialChar c
        let isSubsequentChar c = isInitialChar c || isDigit c || isExplicitSign c
        let isSignSubsequent c = isInitialChar c || isExplicitSign c
        let regularIdentifierFormat = IdentifierOptions(isAsciiIdStart = isInitialChar,
                                                    isAsciiIdContinue = isSubsequentChar)
        let parseRegularIdentifier = identifier regularIdentifierFormat
        let parsePeculiarIdentifier = many1Strings2 (many1Chars2 (satisfy isExplicitSign) (satisfy isSignSubsequent))
                                                    parseRegularIdentifier
        parseRegularIdentifier <|> parsePeculiarIdentifier

    let parseSymbol = parseSymbolOrIdentifier |>> CTSymbol

    let parseDatum, private parseDatumRef = createParserForwardedToRef<CTDatum, unit>()

    let private singleLineComment = satisfyL (isAnyOf ";") "comment" >>. restOfLine true
    let private whitespace = many1SatisfyL (isAnyOf " \n\r\t") "whitespace"
    let private whitespaceOrComment = singleLineComment <|> whitespace

    let private ws = skipMany whitespaceOrComment
    let private ws1 = skipMany1 whitespaceOrComment

    let private betweenStrings s1 s2 p = between (pstring s1) (pstring s2) p
    let private parseListOf p = (ws >>. sepEndBy p ws1 .>> ws)
    let private parseParenthesisedListOf p = betweenStrings "(" ")" (parseListOf p)

    let parseList = parseParenthesisedListOf parseDatum |>> CTList

    do parseDatumRef := choice [parseList
                                parseBoolean
                                parseNumber
                                parseStringLiteral
                                parseSymbol]

    let parseSugaredQuotation = skipChar '\'' >>. parseDatum

    let parseIdentifier = parseSymbolOrIdentifier |>> CTIdentifier

    let parseExpression, private parseExpressionRef = createParserForwardedToRef<CTExpression, unit>()

    let parseIdentifierExpression = parseIdentifier |>> CTIdentifierExpression

    let parseLiteral = choice [parseSugaredQuotation
                               parseBoolean
                               parseNumber
                               parseStringLiteral]
                       |>> CTLiteralExpression

    let parseListExpression = parseParenthesisedListOf parseExpression
                              |>> CTListExpression

    parseExpressionRef := ws >>. choice [parseListExpression
                                         parseLiteral
                                         parseIdentifierExpression]
