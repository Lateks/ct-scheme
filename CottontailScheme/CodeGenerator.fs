module CottontailScheme.CodeGenerator

open Scope
open SemanticAnalysis
open Literals

open CottontailSchemeLib

open System
open System.Reflection

type Procedure = { methodBuilder : Emit.MethodBuilder;
                   closure : ClosureDefinition }

type Variable = Field of Emit.FieldBuilder
              | LocalVar of Emit.LocalBuilder

type Scope = { variables : Map<string, Variable>;
               procedures : Map<string, Procedure> }

let builtIns = ref []

let maxArgsToUserDefinedProc = 3

let setupAssembly name =
    let assemblyName = AssemblyName()
    assemblyName.Name <- name
    let appDomain = AppDomain.CurrentDomain
    appDomain.DefineDynamicAssembly(assemblyName, Emit.AssemblyBuilderAccess.Save)

let copyLibs () =
    let exePath = Assembly.GetExecutingAssembly().Location |> IO.Path.GetDirectoryName
    let csLib = "CottontailSchemeLib.dll"
    let target = IO.Directory.GetCurrentDirectory() + "/" + csLib
    if IO.File.Exists(target) then
        IO.File.Delete(target)
    IO.File.Copy(exePath + "/" + csLib, target)

let loadStringObject (gen : Emit.ILGenerator) (s : string) =
    gen.Emit(Emit.OpCodes.Ldstr, s)
    gen.Emit(Emit.OpCodes.Newobj, typeof<CTString>.GetConstructor([| typeof<string> |]))

let loadBooleanObject (gen : Emit.ILGenerator) (b : bool) =
    let constantInfo = typeof<Constants>.GetField(if b then "True" else "False")
    gen.Emit(Emit.OpCodes.Ldsfld, constantInfo)

let loadNumberObject (gen : Emit.ILGenerator) (f : float) =
    gen.Emit(Emit.OpCodes.Ldc_R8, f)
    gen.Emit(Emit.OpCodes.Newobj, typeof<CTNumber>.GetConstructor([| typeof<float> |]))

let loadSymbolObject (gen : Emit.ILGenerator) (s : string) =
    gen.Emit(Emit.OpCodes.Ldstr, s)
    gen.Emit(Emit.OpCodes.Newobj, typeof<CTSymbol>.GetConstructor([| typeof<string> |]))

let loadUndefined (gen : Emit.ILGenerator) =
    gen.Emit(Emit.OpCodes.Ldsfld, typeof<Constants>.GetField("Undefined"))

let builtInFunctionsTakingArrayParams = ["list"; "+"; "-"; "/"; "*"; "<"; ">"]

let popStack (gen : Emit.ILGenerator) =
    gen.Emit(Emit.OpCodes.Pop)

let emitArray (gen : Emit.ILGenerator) members emitMember =
    let memberCount = List.length members
    gen.Emit(Emit.OpCodes.Ldc_I4, memberCount)
    gen.Emit(Emit.OpCodes.Newarr, typeof<CTObject>)
    let indexedMembers = seq{0..memberCount-1}
                         |> Seq.toList
                         |> List.zip members
    for (m, i) in indexedMembers do
        gen.Emit(Emit.OpCodes.Dup)
        gen.Emit(Emit.OpCodes.Ldc_I4, i)
        emitMember gen m
        gen.Emit(Emit.OpCodes.Stelem_Ref)

let rec emitLiteral (gen : Emit.ILGenerator) (lit : Literals.LiteralValue) =
    match lit with
    | String s -> loadStringObject gen s
    | Boolean b -> loadBooleanObject gen b
    | Number f -> loadNumberObject gen f
    | Symbol s -> loadSymbolObject gen s
    | List lits -> emitArray gen lits emitLiteral
                   gen.Emit(Emit.OpCodes.Call, typeof<ListOperations>.GetMethod("List", [| typeof<CTObject array> |]))

let emitBooleanConversion (gen : Emit.ILGenerator) =
    gen.Emit(Emit.OpCodes.Callvirt, typeof<CTObject>.GetMethod("ToBool", [||]))

let rec emitBuiltInFunctionCall (gen : Emit.ILGenerator) (id : Identifier) =
    let listOps = typeof<ListOperations>
    let numberOps = typeof<NumberOperations>
    let commonOps = typeof<CommonOperations>
    let ctObject = typeof<CTObject>
    let arrayType = typeof<CTObject array>

    match id.uniqueName with
    | "display" -> gen.Emit(Emit.OpCodes.Call, commonOps.GetMethod("Display", [| ctObject |]))
    | "zero?" -> gen.Emit(Emit.OpCodes.Call, numberOps.GetMethod("IsZero", [| ctObject |]))
    | "list" -> gen.Emit(Emit.OpCodes.Call, listOps.GetMethod("List", [| arrayType |]))
    | "null?" -> gen.Emit(Emit.OpCodes.Call, listOps.GetMethod("IsNull", [| ctObject |]))
    | "car" -> gen.Emit(Emit.OpCodes.Call, listOps.GetMethod("Car", [| ctObject |]))
    | "cdr" -> gen.Emit(Emit.OpCodes.Call, listOps.GetMethod("Cdr", [| ctObject |]))
    | "cons" -> gen.Emit(Emit.OpCodes.Call, listOps.GetMethod("Cons", [| ctObject; ctObject |]))
    | "+" -> gen.Emit(Emit.OpCodes.Call, numberOps.GetMethod("Plus", [| arrayType |]))
    | "-" -> gen.Emit(Emit.OpCodes.Call, numberOps.GetMethod("Minus", [| arrayType |]))
    | "*" -> gen.Emit(Emit.OpCodes.Call, numberOps.GetMethod("Mult", [| arrayType |]))
    | "/" -> gen.Emit(Emit.OpCodes.Call, numberOps.GetMethod("Div", [| arrayType |]))
    | "<" -> gen.Emit(Emit.OpCodes.Call, numberOps.GetMethod("LessThan", [| arrayType |]))
    | ">" -> gen.Emit(Emit.OpCodes.Call, numberOps.GetMethod("GreaterThan", [| arrayType |]))
    | "eq?" -> gen.Emit(Emit.OpCodes.Call, commonOps.GetMethod("AreEq", [| ctObject; ctObject |]))
    | "not" -> gen.Emit(Emit.OpCodes.Call, commonOps.GetMethod("Not", [| ctObject |]))
    | "newline" -> gen.Emit(Emit.OpCodes.Call, commonOps.GetMethod("Newline", [||]))
    | e -> failwithf "Built-in function %s is not implemented!" e
and generateSubExpression (gen : Emit.ILGenerator) (scope: Scope) (pushOnStack : bool) (expr : Expression) =
    let pushExprResultToStack = generateSubExpression gen scope true

    let emitFunctionCall id args isTailCall =
        let pushArgsAsArray () = emitArray gen args (fun g -> generateSubExpression g scope true)
        let pushIndividualArgs () = for arg in args do pushExprResultToStack arg

        // Tail annotations are not emitted for built-in procedures because
        // the built-in procedures currently defined in the library never
        // result in recursion or other tail call continuations.
        if List.contains id !builtIns then
            if List.contains id.uniqueName builtInFunctionsTakingArrayParams then
                pushArgsAsArray ()
            else
                pushIndividualArgs ()

            emitBuiltInFunctionCall gen id
        elif scope.procedures.ContainsKey id.uniqueName then
            let procedure = scope.procedures.Item(id.uniqueName)
            match procedure.closure.formals with
            | SingleArgFormals _ -> pushArgsAsArray ()
            | MultiArgFormals _ -> if args.Length <= maxArgsToUserDefinedProc then
                                       pushIndividualArgs ()
                                   else
                                       pushArgsAsArray ()

            if isTailCall then gen.Emit(Emit.OpCodes.Tailcall)
            gen.Emit(Emit.OpCodes.Call, procedure.methodBuilder)
        else
            failwithf "Not implemented yet! (generic procedure calls)"
    let emitConditional condition thenExpression elseExpression =
        let elseLabel = gen.DefineLabel()
        let exitLabel = gen.DefineLabel()

        pushExprResultToStack condition
        emitBooleanConversion gen
        gen.Emit(Emit.OpCodes.Brfalse, elseLabel)

        pushExprResultToStack thenExpression
        gen.Emit(Emit.OpCodes.Br_S, exitLabel)

        gen.MarkLabel(elseLabel)
        pushExprResultToStack elseExpression

        gen.MarkLabel(exitLabel)
    let emitBeginSequence =
        function
        | [] -> if pushOnStack then loadUndefined gen
        | exprs -> let tailExpr = List.last exprs
                   let nonTailExprs = List.take (exprs.Length - 1) exprs
                   for expr in nonTailExprs do
                       generateSubExpression gen scope false expr
                   generateSubExpression gen scope pushOnStack tailExpr
    let emitBooleanCombinationExpression breakValue =
        function
        | [] -> if pushOnStack then loadBooleanObject gen (not breakValue)
        | exprs ->
            let exitLabel = gen.DefineLabel()
            let tempVar = if pushOnStack then Some <| gen.DeclareLocal(typeof<CTObject>) else None
            let storeTempValue () = tempVar |> Option.map (fun (v : Emit.LocalBuilder) -> gen.Emit(Emit.OpCodes.Stloc, v.LocalIndex))
                                            |> ignore
            let loadTempValue () = tempVar |> Option.map (fun (v : Emit.LocalBuilder) -> gen.Emit(Emit.OpCodes.Ldloc, v.LocalIndex))
                                           |> ignore
            let comparison = if breakValue then
                                Emit.OpCodes.Brtrue
                             else
                                Emit.OpCodes.Brfalse
            let tailExpr = List.last exprs
            let nonTailExprs = List.take (exprs.Length - 1) exprs

            for expr in nonTailExprs do
                pushExprResultToStack expr
                storeTempValue ()
                loadTempValue ()

                emitBooleanConversion gen
                gen.Emit(comparison, exitLabel)

            pushExprResultToStack tailExpr
            if pushOnStack then
                storeTempValue ()
            else
                popStack gen

            gen.MarkLabel(exitLabel)
            loadTempValue ()
    let emitAssignment (id : Identifier) expr =
        assert scope.variables.ContainsKey(id.uniqueName)

        pushExprResultToStack expr

        match scope.variables.Item(id.uniqueName) with
        | LocalVar v -> gen.Emit(Emit.OpCodes.Stloc, v)
        | Field v -> gen.Emit(Emit.OpCodes.Stsfld, v)

        if pushOnStack then loadUndefined gen
    let emitVariableLoad (id : Identifier) =
        match id.argIndex with
        | Some n ->
            let opcode = match n with // TODO: limit number of arguments to 5-7?
                         | 0 -> Emit.OpCodes.Ldarg_0
                         | 1 -> Emit.OpCodes.Ldarg_1
                         | 2 -> Emit.OpCodes.Ldarg_2
                         | 3 -> Emit.OpCodes.Ldarg_3
                         | _ -> failwith "Not implemented yet: functions with more than 3 arguments"
            gen.Emit(opcode)
        | None ->
            assert scope.variables.ContainsKey(id.uniqueName)

            match scope.variables.Item(id.uniqueName) with
            | LocalVar v -> gen.Emit(Emit.OpCodes.Ldloc, v)
            | Field v -> gen.Emit(Emit.OpCodes.Ldsfld, v)

    match expr with
    | ProcedureCall (proc, args, isTailCall)
        -> match proc with
           | VariableReference id -> emitFunctionCall id args isTailCall
           | e -> failwithf "Not implemented yet! %A" e
           if not pushOnStack then popStack gen
    | ValueExpression lit
        -> emitLiteral gen lit
           if not pushOnStack then popStack gen
    | Conditional (cond, thenBranch, elseBranch)
        -> emitConditional cond thenBranch elseBranch
           if not pushOnStack then popStack gen
    | SequenceExpression (seqType, exprs)
        -> match seqType with
           | BeginSequence -> emitBeginSequence exprs
           | AndSequence -> emitBooleanCombinationExpression false exprs
           | OrSequence -> emitBooleanCombinationExpression true exprs
    | Assignment (id, expr)
        -> emitAssignment id expr
    | VariableReference id
        -> emitVariableLoad id
           if not pushOnStack then popStack gen
    | UndefinedValue
        -> if pushOnStack then loadUndefined gen
    | e -> failwithf "Not implemented yet! %A" e

let generateExpression (gen : Emit.ILGenerator) (expr : Expression) (scope : Scope) =
    generateSubExpression gen scope false expr

let generateProcedureBody (gen : Emit.ILGenerator) (c : ClosureDefinition) scope =
    let locals = c.variableDeclarations
                 |> List.map (fun (VariableDeclaration id) ->
                                  (id.uniqueName, LocalVar <| gen.DeclareLocal(typeof<CTObject>)))
    let extendedScope = { scope with variables = Map.toSeq scope.variables
                                                 |> Seq.append locals
                                                 |> Map.ofSeq }

    let body = List.take (c.body.Length - 1) c.body
    let tailExpr = List.last c.body

    for expr in body do
        generateExpression gen expr extendedScope

    // TODO: tailcalls
    generateSubExpression gen extendedScope true tailExpr
    gen.Emit(Emit.OpCodes.Ret)

let defineVariables (c : Emit.TypeBuilder) =
    List.map (fun (VariableDeclaration id) ->
                    (id.uniqueName, Field <| c.DefineField(id.uniqueName,
                                                           typeof<CTObject>,
                                                           FieldAttributes.Static ||| FieldAttributes.Private)))
    >> Map.ofList

let defineProcedures (c : Emit.TypeBuilder) =
    List.map (fun (ProcedureDefinition (id, clos)) ->
                  let parameterTypes = match clos.formals with
                                       | SingleArgFormals id -> [| typeof<CTObject array> |]
                                       | MultiArgFormals ids -> if ids.Length <= maxArgsToUserDefinedProc then
                                                                    ids |> List.map (fun _ -> typeof<CTObject> )
                                                                        |> Array.ofList
                                                                else
                                                                    [| typeof<CTObject array> |]
                  let procedure = c.DefineMethod(id.uniqueName,
                                                 MethodAttributes.Static ||| MethodAttributes.Private,
                                                 typeof<CTObject>,
                                                 parameterTypes)
                  (id.uniqueName, { methodBuilder = procedure; closure = clos }))
    >> Map.ofList

let generateTopLevelProcedureBodies (scope : Scope) =
    for (_, proc) in Map.toSeq scope.procedures do
        generateProcedureBody (proc.methodBuilder.GetILGenerator()) proc.closure scope

let generateMainModule (mainClass : Emit.TypeBuilder) (mainMethod : Emit.MethodBuilder) (program : ProgramStructure) =
    let ilGen = mainMethod.GetILGenerator()

    let scope = { variables = defineVariables mainClass program.variableDeclarations;
                  procedures = defineProcedures mainClass program.procedureDefinitions }

    generateTopLevelProcedureBodies scope

    ilGen.BeginExceptionBlock() |> ignore

    for expr in program.expressions do
        generateExpression ilGen expr scope

    ilGen.BeginCatchBlock(typeof<CottontailSchemeException>)

    ilGen.Emit(Emit.OpCodes.Ldstr, "\nError:")
    ilGen.Emit(Emit.OpCodes.Call, typeof<Console>.GetMethod("WriteLine", [| typeof<string> |]))
    ilGen.Emit(Emit.OpCodes.Callvirt, typeof<Exception>.GetProperty("Message").GetGetMethod())
    ilGen.Emit(Emit.OpCodes.Call, typeof<Console>.GetMethod("WriteLine", [| typeof<string> |]))

    ilGen.EndExceptionBlock()

    ilGen.Emit(Emit.OpCodes.Ldc_I4_0)
    ilGen.Emit(Emit.OpCodes.Ret)

    mainClass.CreateType()

let generateCodeFor (program : ProgramStructure) (name : string) =
    let capitalizedName = SymbolGenerator.capitalizeWord name
    let outputFileName = sprintf "%s.exe" capitalizedName
    let assemblyBuilder = setupAssembly capitalizedName
    let moduleBuilder = assemblyBuilder.DefineDynamicModule(capitalizedName, outputFileName);

    // Create class for the "module body"
    let mainClass = moduleBuilder.DefineType(capitalizedName, TypeAttributes.Public ||| TypeAttributes.Class)
    let mainMethod = mainClass.DefineMethod("Main", MethodAttributes.Public ||| MethodAttributes.Static,
                                            typeof<int>, [| typeof<string array> |])

    builtIns := getBuiltIns program.scope
    generateMainModule mainClass mainMethod program |> ignore

    // Save assembly and mark it as a console application
    assemblyBuilder.SetEntryPoint(mainMethod, Emit.PEFileKinds.ConsoleApplication)
    assemblyBuilder.Save(outputFileName)

    copyLibs()
