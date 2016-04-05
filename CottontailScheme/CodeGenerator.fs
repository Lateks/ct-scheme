module CottontailScheme.CodeGenerator

open Scope
open SemanticAnalysis

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

let generateCodeFor (exprs : SemanticAnalysis.Expression list) (scope : Scope.Scope) (name : string) =
    let capitalizedName = SymbolGenerator.capitalizeWord name
    let outputFileName = sprintf "%s.exe" capitalizedName
    let assemblyBuilder = setupAssembly capitalizedName
    let moduleBuilder = assemblyBuilder.DefineDynamicModule(capitalizedName, outputFileName);

    // Create class
    let typeBuilder = moduleBuilder.DefineType(capitalizedName, TypeAttributes.Public ||| TypeAttributes.Class)

    // Create main method
    let methodBuilder = typeBuilder.DefineMethod("Main", MethodAttributes.Public ||| MethodAttributes.Static,
                                                    typeof<int>, [| typeof<string array> |])
    let ilGen = methodBuilder.GetILGenerator()
    // Emit instructions for the main method
    ilGen.Emit(Emit.OpCodes.Ldsfld, typeof<Constants>.GetField("True"))
    ilGen.Emit(Emit.OpCodes.Call, typeof<Console>.GetMethod("WriteLine", [| typeof<Object> |]))
    ilGen.Emit(Emit.OpCodes.Ldstr, "Hello, world!")
    ilGen.Emit(Emit.OpCodes.Call, typeof<Console>.GetMethod("WriteLine", [| typeof<string> |]))
    ilGen.Emit(Emit.OpCodes.Ldc_I4_0)
    ilGen.Emit(Emit.OpCodes.Ret)

    // Finalize generated class
    let t = typeBuilder.CreateType()

    // Save assembly and mark it as a console application
    assemblyBuilder.SetEntryPoint(methodBuilder, Emit.PEFileKinds.ConsoleApplication)
    assemblyBuilder.Save(outputFileName)

    copyLibs()
