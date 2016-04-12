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
                   gen.Emit(Emit.OpCodes.Call, typeof<BuiltIns>.GetMethod("List", [| typeof<CTObject array> |]))

let emitBooleanConversion (gen : Emit.ILGenerator) =
    gen.Emit(Emit.OpCodes.Callvirt, typeof<CTObject>.GetMethod("ToBool", [||]))

let convertBuiltInName =
    function
    | "zero?" -> "IsZero"
    | "null?" -> "IsNull"
    | "+" -> "Plus"
    | "-" -> "Minus"
    | "*" -> "Mult"
    | "/" -> "Div"
    | "<" -> "LessThan"
    | ">" -> "GreaterThan"
    | "eq?" -> "AreEq"
    | "list"
    | "display"
    | "car"
    | "cdr"
    | "cons"
    | "not"
    | "newline" as n
        -> SymbolGenerator.capitalizeWord n
    | e -> failwithf "Built-in function %s is not implemented!" e

let loadBuiltInProcedure (gen : Emit.ILGenerator) (id : Identifier) =
    let builtInName = convertBuiltInName id.uniqueName
    gen.Emit(Emit.OpCodes.Ldsfld, typeof<BuiltIns>.GetField("Obj" + builtInName));

let emitBuiltInFunctionCall (gen : Emit.ILGenerator) (id : Identifier) =
    let builtInName = convertBuiltInName id.uniqueName
    let methodInfo = typeof<BuiltIns>.GetMethod(builtInName)
    gen.Emit(Emit.OpCodes.Call, methodInfo)

let rec generateSubExpression (gen : Emit.ILGenerator) (scope: Scope) (pushOnStack : bool) (expr : Expression) =
    let pushExprResultToStack = generateSubExpression gen scope true

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
        pushExprResultToStack expr

        match id.argIndex with
        | Some n ->
            gen.Emit(Emit.OpCodes.Starg_S, (byte) n)
        | None ->
            assert scope.variables.ContainsKey(id.uniqueName)
            match scope.variables.Item(id.uniqueName) with
            | LocalVar v -> gen.Emit(Emit.OpCodes.Stloc, v)
            | Field v -> gen.Emit(Emit.OpCodes.Stsfld, v)

        if pushOnStack then loadUndefined gen
    let emitVariableLoad (id : Identifier) =
        let loadVariableOrField name = match scope.variables.Item(name) with
                                       | LocalVar v -> gen.Emit(Emit.OpCodes.Ldloc, v)
                                       | Field v -> gen.Emit(Emit.OpCodes.Ldsfld, v)
        match id.argIndex with
        | Some n ->
            if n < 4 then
                let opcode = match n with
                             | 0 -> Emit.OpCodes.Ldarg_0
                             | 1 -> Emit.OpCodes.Ldarg_1
                             | 2 -> Emit.OpCodes.Ldarg_2
                             | _ -> Emit.OpCodes.Ldarg_3
                gen.Emit(opcode)
            else
                gen.Emit(Emit.OpCodes.Ldarg_S, (byte) n)
        | None ->
            if scope.variables.ContainsKey(id.uniqueName) then
                loadVariableOrField id.uniqueName
            else
                assert List.contains id !builtIns
                loadBuiltInProcedure gen id
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
            | MultiArgFormals _ -> pushIndividualArgs ()

            if isTailCall then gen.Emit(Emit.OpCodes.Tailcall)
            gen.Emit(Emit.OpCodes.Call, procedure.methodBuilder)
        else
            let proc = gen.DeclareLocal(typeof<CTProcedure>)

            gen.BeginExceptionBlock() |> ignore

            emitVariableLoad id
            gen.Emit(Emit.OpCodes.Castclass, typeof<CTProcedure>)
            gen.Emit(Emit.OpCodes.Stloc, proc)

            gen.BeginCatchBlock(typeof<System.InvalidCastException>)

            gen.Emit(Emit.OpCodes.Ldstr, id.name)
            emitVariableLoad id
            gen.Emit(Emit.OpCodes.Newobj, typeof<NotAProcedureError>.GetConstructor([| typeof<string>; typeof<CTObject> |]))
            gen.Emit(Emit.OpCodes.Throw)

            gen.EndExceptionBlock()

            gen.Emit(Emit.OpCodes.Ldloc, proc)
            let methodInfo = match args.Length with
                             | 0 | 1 | 2 | 3 | 4 | 5 as n
                                -> pushIndividualArgs ()
                                   typeof<CTProcedure>.GetMethod(sprintf "apply%i" n)
                             | _
                                -> pushArgsAsArray ()
                                   typeof<CTProcedure>.GetMethod("applyN")

            if isTailCall then gen.Emit(Emit.OpCodes.Tailcall)
            gen.Emit(Emit.OpCodes.Callvirt, methodInfo)

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

    generateSubExpression gen extendedScope true tailExpr
    gen.Emit(Emit.OpCodes.Ret)

let defineVariables (c : Emit.TypeBuilder)=
    List.map (fun (VariableDeclaration id) ->
                  (id.uniqueName, Field <| c.DefineField(id.uniqueName,
                                                         typeof<CTObject>,
                                                         FieldAttributes.Static ||| FieldAttributes.Private)))
    >> Map.ofList

let defineProcedures (c : Emit.TypeBuilder) =
    List.map (fun (ProcedureDefinition (id, clos)) ->
                  let parameterTypes = match clos.formals with
                                       | SingleArgFormals id -> [| typeof<CTObject array> |]
                                       | MultiArgFormals ids -> ids |> List.map (fun _ -> typeof<CTObject> )
                                                                    |> Array.ofList
                  let procedure = c.DefineMethod(id.uniqueName,
                                                 MethodAttributes.Static ||| MethodAttributes.Private,
                                                 typeof<CTObject>,
                                                 parameterTypes)
                  (id.uniqueName, { methodBuilder = procedure; closure = clos }))
    >> Map.ofList

let generateTopLevelProcedureBodies (scope : Scope) =
    for (_, proc) in Map.toSeq scope.procedures do
        generateProcedureBody (proc.methodBuilder.GetILGenerator()) proc.closure scope

let generateClassInitializer (mainClass : Emit.TypeBuilder) (procs : ProcedureDefinition list) (scope : Scope) =
    let classInitializer = mainClass.DefineConstructor(MethodAttributes.Static ||| MethodAttributes.Public, CallingConventions.Standard, [||])
    let gen = classInitializer.GetILGenerator()
    for (ProcedureDefinition (id, c)) in procs do
        if c.usedAsFirstClassValue then
            let procedure = scope.procedures.Item(id.uniqueName)
            let procType = match c.formals with
                           | SingleArgFormals id -> typeof<Func<CTObject[], CTObject>>
                           | MultiArgFormals ids -> match List.length ids with
                                                    | 0 -> typeof<Func<CTObject>>
                                                    | 1 -> typeof<Func<CTObject, CTObject>>
                                                    | 2 -> typeof<Func<CTObject, CTObject, CTObject>>
                                                    | 3 -> typeof<Func<CTObject, CTObject, CTObject, CTObject>>
                                                    | 4 -> typeof<Func<CTObject, CTObject, CTObject, CTObject, CTObject>>
                                                    | _ -> typeof<Func<CTObject, CTObject, CTObject, CTObject, CTObject, CTObject>>
            let ctObjectType = match c.formals with
                               | SingleArgFormals id -> typeof<CTDelegateProcedureVarargs>
                               | MultiArgFormals ids -> match List.length ids with
                                                       | 0 -> typeof<CTDelegateProcedure0>
                                                       | 1 -> typeof<CTDelegateProcedure1>
                                                       | 2 -> typeof<CTDelegateProcedure2>
                                                       | 3 -> typeof<CTDelegateProcedure3>
                                                       | 4 -> typeof<CTDelegateProcedure4>
                                                       | _ -> typeof<CTDelegateProcedure5>
            let (Field var) = scope.variables.Item(SymbolGenerator.toProcedureObjectName id.uniqueName)
            gen.Emit(Emit.OpCodes.Ldstr, id.name)
            gen.Emit(Emit.OpCodes.Ldnull)
            gen.Emit(Emit.OpCodes.Ldftn, procedure.methodBuilder)
            gen.Emit(Emit.OpCodes.Newobj, procType.GetConstructor([| typeof<Object>; typeof<nativeint> |]))
            gen.Emit(Emit.OpCodes.Newobj, ctObjectType.GetConstructor([| typeof<string>; procType |]))
            gen.Emit(Emit.OpCodes.Stsfld, var)
    gen.Emit(Emit.OpCodes.Ret)

let generateMainModule (mainClass : Emit.TypeBuilder) (mainMethod : Emit.MethodBuilder) (program : ProgramStructure) =
    let ilGen = mainMethod.GetILGenerator()
    let procedures = defineProcedures mainClass program.procedureDefinitions
    let variables = defineVariables mainClass program.variableDeclarations

    let scope = { variables = variables;
                  procedures = procedures }

    generateTopLevelProcedureBodies scope
    generateClassInitializer mainClass program.procedureDefinitions scope

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
