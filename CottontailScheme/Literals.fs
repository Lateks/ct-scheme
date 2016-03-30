module CottontailScheme.Literals

type LiteralValue =
    | Boolean of bool
    | Number of float
    | String of string
    | Symbol of string
    | List of LiteralValue list