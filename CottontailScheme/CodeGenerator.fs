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

let emitStringObjectCreation (gen : Emit.ILGenerator) (s : string) =
    gen.Emit(Emit.OpCodes.Ldstr, s)
    gen.Emit(Emit.OpCodes.Newobj, typeof<CTString>.GetConstructor([| typeof<string> |]))

let emitBuiltInFunctionCall (gen : Emit.ILGenerator) (id : Identifier) =
    match id.uniqueName with
    | "display" -> gen.Emit(Emit.OpCodes.Call, typeof<Console>.GetMethod("Write", [| typeof<Object> |]))
    | e -> failwithf "Not implemented yet! (built-in function %s)" e

let emitFunctionCall (gen : Emit.ILGenerator) (id : Identifier) (scope : Scope) =
    if List.contains id (getBuiltIns scope) then
        emitBuiltInFunctionCall gen id
    else // TODO: what if identifier identifies a variable?
        failwithf "Not implemented yet ! (generic procedure calls)"

let popStack (gen : Emit.ILGenerator) =
    gen.Emit(Emit.OpCodes.Pop)

let generateExpression (gen : Emit.ILGenerator) (expr : Expression) (scope : Scope) =
    let rec generate expr pushOnStack =
        match expr with
        | ProcedureCall (proc, args)
            -> for arg in args do generate arg true
               match proc with
               | VariableReference id -> emitFunctionCall gen id scope
               | e -> failwithf "Not implemented yet! %A" e
        | ValueExpression lit
            -> match lit with
               | String s -> emitStringObjectCreation gen s
               | e -> failwithf "Not implemented yet! %A" e
               if not pushOnStack then
                  popStack gen
        | e -> failwithf "Not implemented yet! %A" e

    generate expr false

let generateMainModule (mainClass : Emit.TypeBuilder) (mainMethod : Emit.MethodBuilder) (program : ProgramStructure) =
    let ilGen = mainMethod.GetILGenerator()
    for expr in program.expressions do
        generateExpression ilGen expr program.scope

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
