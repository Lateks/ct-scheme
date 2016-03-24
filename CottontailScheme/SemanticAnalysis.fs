module CottontailScheme.SemanticAnalysis

// TODO in this phase:
// - alpha conversion for variables
// - recognize references to undefined variables
// - closure conversion?
//   -> find and label variables that are referenced from the environment
// - label tail calls

// Notes:
//
// Lambda expressions compile into closure objects that are parameterised with
// their environment.
//
// Directly tail recursive functions compile into a loop in the function body.
// -> Can use a loop element as an intermediate language construct.
// -> What should the loop element's parameters be?
//
// Other tail calls just need to be labeled so we can use the .tail annotation.
//
// "Values" in tail position need to be identified to insert return statements.
// Do we need to label expressions or is it redundant?

type SymbolGenerator () =
    let counters = new System.Collections.Generic.Dictionary<string, int>()

    // TODO: use a prefix?
    member this.generateSymbol name =
        if not (counters.ContainsKey name) then
            counters.Add (name, 1)
        let counter = counters.[name]
        counters.[name] <- counter + 1
        sprintf "%s$%d" name counter

type Identifier = { name: string; unique_name: string; }
type VariableReferenceDetails = { id: Identifier; fromExternalScope: bool }
type LiteralValue =
    | Boolean of bool
    | Number of float
    | String of string
    | Symbol of string
    | List of LiteralValue list

type Expression =
     | VariableReference of VariableReferenceDetails
     | Closure of ClosureDefinition
     | ProcedureCall of Expression * Expression list
     | LiteralExpression of LiteralValue
     | Assignment of VariableReferenceDetails * Expression
     | Conditional of Expression * Expression * Expression option
     | Loop of LoopDefinition
and NewIdentifier = NewIdentifier of Identifier * Expression
and ClosureFormals = SingleArgFormals of NewIdentifier
                   | MultiArgFormals of NewIdentifier list
and ClosureDefinition = { formals: ClosureFormals;
                          definitions: NewIdentifier list;
                          body: Expression list;
                          environment: Identifier list }
and LoopDefinition = { test: Expression
                       loopVars: Identifier list
                       returnVar: Identifier
                       loopBody: Expression list }

let placeholder name = failwithf "Not implemented yet: %s" name

let rec analyse exprs =
    exprs
    |> List.map (function
                 | ASTBuilder.IdentifierExpression id -> placeholder "identifier expressions"
                 | ASTBuilder.LambdaExpression (formals, defs, exprs) -> placeholder "lambda expressions"
                 | ASTBuilder.AssignmentExpression binding -> placeholder "assignments"
                 | ASTBuilder.Definition binding -> placeholder "definitions"
                 | ASTBuilder.ProcedureCallExpression (expr, exprs) -> placeholder "procedure calls"
                 | ASTBuilder.ConditionalExpression (cond, thenBranch, elseBranch) -> placeholder "conditionals"
                 | ASTBuilder.LiteralExpression lit -> placeholder "literals"
                 | ASTBuilder.ExpressionError err -> failwithf "Error, faulty AST given as input to analysis. Contains error \"%s\"." err.message
                )
