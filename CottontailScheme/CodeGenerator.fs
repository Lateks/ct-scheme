module CottontailScheme.CodeGenerator

open Scope
open SemanticAnalysis

open System
open System.Reflection

let generateCodeFor (program : Program) (name : string) =
    let capitalizedName = SymbolGenerator.capitalizeWord name
    let outputFileName = sprintf "%s.exe" capitalizedName
    let assemblyName = AssemblyName()
    assemblyName.Name <- capitalizedName
    let appDomain = AppDomain.CurrentDomain
    // Create assembly
    let assemblyBuilder = appDomain.DefineDynamicAssembly(assemblyName, Emit.AssemblyBuilderAccess.Save)
    // Create module
    let moduleBuilder = assemblyBuilder.DefineDynamicModule(assemblyName.Name, outputFileName);
    // Create class
    let typeBuilder = moduleBuilder.DefineType(capitalizedName, TypeAttributes.Public ||| TypeAttributes.Class)
    // Create main method
    let methodBuilder = typeBuilder.DefineMethod("Main", MethodAttributes.Public ||| MethodAttributes.Static,
                                                 typeof<int>, [| typeof<string array> |])
    let ilGen = methodBuilder.GetILGenerator()
    // Emit instructions for the main method
    ilGen.Emit(Emit.OpCodes.Ldstr, "Hello, world!")
    ilGen.Emit(Emit.OpCodes.Call, typeof<Console>.GetMethod("WriteLine", [| typeof<string> |]))
    ilGen.Emit(Emit.OpCodes.Ldc_I4_0)
    ilGen.Emit(Emit.OpCodes.Ret)
    // Finalize generated class
    let t = typeBuilder.CreateType()
    // Save assembly and mark it as a console application
    assemblyBuilder.SetEntryPoint(methodBuilder, Emit.PEFileKinds.ConsoleApplication)
    assemblyBuilder.Save(outputFileName)
