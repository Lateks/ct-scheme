﻿module CottontailScheme.Parsing

open FParsec

type CTDatum = CTBool of bool
             | CTNumber of float
             | CTString of string
             | CTSymbol of string
             | CTList of CTDatum list

type CTExpression = CTIdentifierExpression of string
                  | CTLiteralExpression of CTDatum
                  | CTListExpression of CTExpression list

type CTProgram = CTProgram of CTExpression list

// Parsing whitespace and comments
let singleLineComment = pchar ';' <?> "comment" >>. restOfLine true
let whitespace = many1SatisfyL (isAnyOf " \n\r\t") "whitespace"
let whitespaceOrComment = singleLineComment <|> whitespace
let ws = skipMany whitespaceOrComment
let ws1 = skipMany1 whitespaceOrComment

// Parsing lists
let betweenStrings s1 s2 p = between (pstring s1) (pstring s2) p
let parseListOf p = (ws >>. sepEndBy p ws1 .>> ws)
let parseParenthesisedListOf p = betweenStrings "(" ")" (parseListOf p)

// High level S-expression parsers
let parseDatum, parseDatumRef = createParserForwardedToRef<CTDatum, unit>()
let parseExpression, parseExpressionRef = createParserForwardedToRef<CTExpression, unit>()
let parseProgram = parseListOf parseExpression
                   |>> CTProgram

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
    let parsePeculiarIdentifier = many1Strings2 (many1Chars2 (satisfy isExplicitSign) (satisfy isSignSubsequent))
                                                parseRegularIdentifier
    parseRegularIdentifier <|> parsePeculiarIdentifier

let parseBoolean = skipChar '#' >>. (((pstring "true" <|> pstring "t") >>% CTBool true)
                            <|> ((pstring "false" <|> pstring "f") >>% CTBool false))

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
    betweenStrings "\"" "\"" (stringsSepBy normalCharSeq escapedChar)
    |>> CTString

let parseSymbol = parseSymbolOrIdentifier |>> CTSymbol

let parseList = parseParenthesisedListOf parseDatum |>> CTList

let parseSugaredQuotation = skipChar '\'' >>. parseDatum

let parseParenthesisedQuotation = betweenStrings "(" ")" (pstring "quote" >>. ws1 >>. parseDatum)

let parseIdentifierExpression = parseSymbolOrIdentifier |>> CTIdentifierExpression

let parseLiteral = choice [parseSugaredQuotation
                           parseBoolean
                           parseNumber
                           parseStringLiteral]
                   |>> CTLiteralExpression

let parseListExpression = attempt (parseParenthesisedQuotation |>> CTLiteralExpression)
                          <|> (parseParenthesisedListOf parseExpression |>> CTListExpression)

parseDatumRef := choice [parseList
                         parseBoolean
                         parseNumber
                         parseStringLiteral
                         parseSymbol]

parseExpressionRef := ws >>. choice [parseListExpression
                                     parseLiteral
                                     parseIdentifierExpression]
