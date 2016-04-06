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

let builtInFunctionsTakingArrayParams = ["list"; "+"; "-"; "/"; "*"]

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

let rec emitBuiltInFunctionCall (gen : Emit.ILGenerator) (id : Identifier) =
    let listOps = typeof<ListOperations>
    let numberOps = typeof<NumberOperations>
    let ctObject = typeof<CTObject>
    match id.uniqueName with
    | "display" -> gen.Emit(Emit.OpCodes.Call, typeof<Console>.GetMethod("Write", [| typeof<Object> |]))
    | "zero?" -> gen.Emit(Emit.OpCodes.Call, numberOps.GetMethod("IsZero", [| ctObject |]))
    | "list" -> gen.Emit(Emit.OpCodes.Call, listOps.GetMethod("List", [| typeof<CTObject array> |]))
    | "null?" -> gen.Emit(Emit.OpCodes.Call, listOps.GetMethod("IsNull", [| ctObject |]))
    | "car" -> gen.Emit(Emit.OpCodes.Call, listOps.GetMethod("Car", [| ctObject |]))
    | "cdr" -> gen.Emit(Emit.OpCodes.Call, listOps.GetMethod("Cdr", [| ctObject |]))
    | "cons" -> gen.Emit(Emit.OpCodes.Call, listOps.GetMethod("Cons", [| ctObject; ctObject |]))
    | "+" -> gen.Emit(Emit.OpCodes.Call, numberOps.GetMethod("Plus", [| typeof<CTObject array> |]))
    | "-" -> gen.Emit(Emit.OpCodes.Call, numberOps.GetMethod("Minus", [| typeof<CTObject array> |]))
    | "*" -> gen.Emit(Emit.OpCodes.Call, numberOps.GetMethod("Mult", [| typeof<CTObject array> |]))
    | "/" -> gen.Emit(Emit.OpCodes.Call, numberOps.GetMethod("Div", [| typeof<CTObject array> |]))
    | e -> failwithf "Not implemented yet! (built-in function %s)" e
and generateSubExpression (gen : Emit.ILGenerator) (scope: Scope) (pushOnStack : bool) (expr : Expression) =
    let emitFunctionCall id args =
        if List.contains id (getBuiltIns scope) then
           if List.contains id.uniqueName builtInFunctionsTakingArrayParams then
               emitArray gen args (fun g -> generateSubExpression g scope true)
           else
               for arg in args do generateSubExpression gen scope true arg
           emitBuiltInFunctionCall gen id 
        else
            failwithf "Not implemented yet! (generic procedure calls)"

    match expr with
    | ProcedureCall (proc, args)
        ->  match proc with
            | VariableReference id -> emitFunctionCall id args
            | e -> failwithf "Not implemented yet! %A" e
    | ValueExpression lit
        -> emitLiteral gen lit
           if not pushOnStack then
              popStack gen
    | e -> failwithf "Not implemented yet! %A" e

let generateExpression (gen : Emit.ILGenerator) (expr : Expression) (scope : Scope) =
    generateSubExpression gen scope false expr

let generateMainModule (mainClass : Emit.TypeBuilder) (mainMethod : Emit.MethodBuilder) (program : ProgramStructure) =
    let ilGen = mainMethod.GetILGenerator()

    ilGen.BeginExceptionBlock() |> ignore

    for expr in program.expressions do
        generateExpression ilGen expr program.scope

    ilGen.BeginCatchBlock(typeof<CottontailSchemeException>)

    ilGen.Emit(Emit.OpCodes.Ldstr, "")
    ilGen.Emit(Emit.OpCodes.Call, typeof<Console>.GetMethod("WriteLine", [| typeof<string> |]))
    ilGen.Emit(Emit.OpCodes.Call, typeof<Exception>.GetMethod("get_Message"))
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
