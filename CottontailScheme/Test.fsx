#r "..\\packages\\FParsec.1.0.2\\lib\\net40-client\\FParsecCS.dll"
#r "..\\packages\\FParsec.1.0.2\\lib\\net40-client\\FParsec.dll"
#load "Parsing.fs"
#load "Literals.fs"
#load "ASTBuilder.fs"
#load "Scope.fs"
#load "SymbolGenerator.fs"
#load "SemanticAnalysis.fs"

open FParsec
open CottontailScheme.Parsing
open CottontailScheme.ASTBuilder
open CottontailScheme.SemanticAnalysis
open CottontailScheme.Literals

let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

let makeAST str = match run parseProgram str with
                  | Success(result, _, _) -> printfn "Success: %A" (buildAST result)
                  | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

let makeASTandAnalyse str = match run parseProgram str with
                            | Success(result, _, _) -> match buildAST result with
                                                       | ASTBuildSuccess exprs -> printfn "Success %A" (analyse exprs)
                                                       | ASTBuildFailure msgs -> printfn "Failure %A" msgs
                            | Failure(errorMsg, _, _) -> failwith errorMsg

analyse [Definition (Binding (Identifier "pi", LiteralExpression (Number 3.14159)))
         IdentifierExpression (Identifier "pi")
         IdentifierExpression (Identifier "+")]
analyse [Definition (Binding (Identifier "pi", LiteralExpression (Number 3.14159)))
         Definition (Binding (Identifier "pi", LiteralExpression (Number 3.0)))]
analyse [ConditionalExpression (LiteralExpression (Boolean true), LiteralExpression (String "true"), Some (LiteralExpression (String "false")))]
analyse [ConditionalExpression (LiteralExpression (Boolean true), LiteralExpression (String "true"), None)]
analyse [Definition (Binding (Identifier "pi", LiteralExpression (Number 3.14159)))
         ConditionalExpression (ProcedureCallExpression (IdentifierExpression (Identifier "<"), [IdentifierExpression (Identifier "pi"); LiteralExpression (Number 3.0)]),
                                ProcedureCallExpression (IdentifierExpression (Identifier "display"), [LiteralExpression (String "smaller than")]),
                                Some (ProcedureCallExpression (IdentifierExpression (Identifier "display"), [LiteralExpression (String "greater than")])))]
analyse [AssignmentExpression (Binding (Identifier "pi", LiteralExpression (Number 3.14159)))]
analyse [Definition (Binding (Identifier "pi", LiteralExpression (Number -1.0)))
         AssignmentExpression (Binding (Identifier "pi", LiteralExpression (Number 3.14159)))]
analyse [Definition (Binding (Identifier "say-hello", LambdaExpression (CottontailScheme.ASTBuilder.MultiArgFormals [Identifier "name"],
                                                                        [],
                                                                        [ProcedureCallExpression (IdentifierExpression (Identifier "display"), [LiteralExpression (String "Hello, ")])
                                                                         ProcedureCallExpression (IdentifierExpression (Identifier "display"), [IdentifierExpression (Identifier "name")])
                                                                         ProcedureCallExpression (IdentifierExpression (Identifier "display"), [LiteralExpression (String "!\n")])])))
         ProcedureCallExpression (IdentifierExpression (Identifier "say-hello"), [LiteralExpression (String "Laura")])]
analyse [Definition (Binding (Identifier "a", ProcedureCallExpression (IdentifierExpression (Identifier "+"), [IdentifierExpression (Identifier "b"); LiteralExpression (Number 1.0)])))
         Definition (Binding (Identifier "b", LiteralExpression (Number 5.0)))]
analyse [AssignmentExpression (Binding (Identifier "pi", LiteralExpression (Number 3.14159)))
         Definition (Binding (Identifier "pi", LiteralExpression (Number -1.0)))]
analyse [Definition (Binding (Identifier "zero?",
                              LambdaExpression (CottontailScheme.ASTBuilder.MultiArgFormals [Identifier "n"],
                                                [],
                                                [ProcedureCallExpression (IdentifierExpression (Identifier "eq?"), [IdentifierExpression (Identifier "n")
                                                                                                                    LiteralExpression (Number 0.0)])])))]
analyse [Definition (Binding (Identifier "pi", LiteralExpression (Number 3.14159)))
         Definition (Binding (Identifier "circle-circumference", LambdaExpression (CottontailScheme.ASTBuilder.MultiArgFormals [Identifier "r"],
                                                                                    [],
                                                                                    [ProcedureCallExpression (IdentifierExpression (Identifier "*"),
                                                                                                              [IdentifierExpression (Identifier "pi")
                                                                                                               IdentifierExpression (Identifier "r")])])))
         Definition (Binding (Identifier "test-fun", LambdaExpression (CottontailScheme.ASTBuilder.MultiArgFormals [Identifier "x"],
                                                                       [],
                                                                       [LambdaExpression (CottontailScheme.ASTBuilder.MultiArgFormals [Identifier "y"],
                                                                                          [],
                                                                                          [ProcedureCallExpression (IdentifierExpression (Identifier "+"),
                                                                                                                    [IdentifierExpression (Identifier "y")
                                                                                                                     IdentifierExpression (Identifier "x")])])])))]

makeASTandAnalyse "(define sum\
                      (lambda (l acc)\
                         (if (null? l)\
                             acc\
                             (sum (cdr l) (+ (car l) acc)))))"

makeASTandAnalyse "(define print-list\
                      (lambda (l)\
                         (if (null? l)\
                             (display \"\\n\")\
                             (begin\
                                (display (car l))\
                                (print-list (cdr l))))))"

makeASTandAnalyse "(lambda ()\
                      (and (< 1 2) (or (> 3 4) (eq? 3 3))))"

makeASTandAnalyse "(lambda () (and #t))"

makeASTandAnalyse "(define x (+ x 1))"

makeASTandAnalyse "(define x 1)(define x (+ x 1))"

makeASTandAnalyse "(define y (+ x 1)) (define x 1)"

makeASTandAnalyse "(set! x 1)"

makeASTandAnalyse "(lambda () (define x 1) (define x (+ x 1)) x)"

makeASTandAnalyse "(define foo\
                     (lambda ()\
                        (define helper\
                          (lambda ()\
                             (display \"Hello\")))\
                        (helper)))"

makeASTandAnalyse "(define x 1)\
                   (define foo\
                     (lambda ()\
                       (define y (+ x 1))\
                       (define x (+ y 1))\
                       (+ x y)))"

makeASTandAnalyse "(define foo (lambda (x y) (+ x y)))\
                   (foo 1 2)"

makeASTandAnalyse "(define foo (lambda l l))\
                   (foo)"

makeASTandAnalyse "(+ 1 2 3)"

makeASTandAnalyse "(< 1 2 3)"

makeASTandAnalyse "(< 1)"

makeASTandAnalyse "(zero? 1)"

makeASTandAnalyse "(zero? 1 2)"
