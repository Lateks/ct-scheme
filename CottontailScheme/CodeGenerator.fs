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
    IO.File.Copy(exePath + "/" + csLib, IO.Directory.GetCurrentDirectory() + "/" + csLib)

let emitStringObjectCreation (gen : Emit.ILGenerator) (s : string) =
    gen.Emit(Emit.OpCodes.Ldstr, s)
    gen.Emit(Emit.OpCodes.Newobj, typeof<CTString>.GetConstructor([| typeof<string> |]))

let emitBuiltInFunctionCall (gen : Emit.ILGenerator) (id : Identifier) =
    match id.uniqueName with
    | "display" -> gen.Emit(Emit.OpCodes.Call, typeof<GenericOperations>.GetMethod("GetDisplayValue"))
                   gen.Emit(Emit.OpCodes.Call, typeof<Console>.GetMethod("Write", [| typeof<string> |]))
    | e -> failwithf "Not implemented yet! (built-in function %s)" e

// TODO: handling args differently? (when to pop results off the stack?)
let rec generateExpression (gen : Emit.ILGenerator) (expr : Expression) (scope : Scope) =
    match expr with
    | ProcedureCall (proc, args)
        -> for arg in args do
               generateExpression gen arg scope
           match proc with
           | VariableReference id -> if List.contains id (getBuiltIns scope) then
                                        emitBuiltInFunctionCall gen id
                                     else
                                        failwithf "Not implemented yet ! (generic procedure calls)"
           | e -> failwithf "Not implemented yet! %A" e
    | ValueExpression lit
        -> match lit with
           | String s -> emitStringObjectCreation gen s
           | e -> failwithf "Not implemented yet! %A" e
    | e -> failwithf "Not implemented yet! %A" e

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
