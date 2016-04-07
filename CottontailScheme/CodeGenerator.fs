module CottontailScheme.CodeGenerator

open Scope
open SemanticAnalysis
open Literals

open CottontailSchemeLib

open System
open System.Reflection

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

    let emitFunctionCall id args =
        if List.contains id (getBuiltIns scope) then
            if List.contains id.uniqueName builtInFunctionsTakingArrayParams then
                emitArray gen args (fun g -> generateSubExpression g scope true)
            else
                for arg in args do pushExprResultToStack arg

            emitBuiltInFunctionCall gen id
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
    let emitAndExpression =
        function
        | [] -> if pushOnStack then loadBooleanObject gen true
        | exprs ->
            let exitLabel = gen.DefineLabel()
            let tempVar = if pushOnStack then Some <| gen.DeclareLocal(typeof<CTObject>) else None
            for expr in exprs do
                pushExprResultToStack expr

                tempVar
                |> Option.map (fun (v : Emit.LocalBuilder) ->
                                   gen.Emit(Emit.OpCodes.Stloc, v.LocalIndex)
                                   gen.Emit(Emit.OpCodes.Ldloc, v.LocalIndex))
                |> ignore

                emitBooleanConversion gen
                gen.Emit(Emit.OpCodes.Brfalse, exitLabel)

            gen.MarkLabel(exitLabel)

            tempVar
            |> Option.map (fun (v : Emit.LocalBuilder) -> gen.Emit(Emit.OpCodes.Ldloc, v.LocalIndex))
            |> ignore

    match expr with
    | ProcedureCall (proc, args)
        -> match proc with
           | VariableReference id -> emitFunctionCall id args
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
           | AndSequence -> emitAndExpression exprs
           | _ -> failwith "Not implemented yet: sequence other than begin"
    | e -> failwithf "Not implemented yet! %A" e

let generateExpression (gen : Emit.ILGenerator) (expr : Expression) (scope : Scope) =
    generateSubExpression gen scope false expr

let generateMainModule (mainClass : Emit.TypeBuilder) (mainMethod : Emit.MethodBuilder) (program : ProgramStructure) =
    let ilGen = mainMethod.GetILGenerator()

    ilGen.BeginExceptionBlock() |> ignore

    for expr in program.expressions do
        generateExpression ilGen expr program.scope

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

    generateMainModule mainClass mainMethod program |> ignore

    // Save assembly and mark it as a console application
    assemblyBuilder.SetEntryPoint(mainMethod, Emit.PEFileKinds.ConsoleApplication)
    assemblyBuilder.Save(outputFileName)

    copyLibs()
