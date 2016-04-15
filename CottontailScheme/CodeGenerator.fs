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
              | InstanceField of Emit.FieldBuilder

type ClosureLoadInfo = { lambdaBuilder : Emit.MethodBuilder;
                         localFrameField : Emit.LocalBuilder option }

type Scope = { variables : Map<string, Variable>;
               procedures : Map<string, Procedure>;
               closureInfo : Map<string, ClosureLoadInfo> }

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
        emitMember m
        gen.Emit(Emit.OpCodes.Stelem_Ref)

let convertArrayOnStackToList (gen : Emit.ILGenerator) =
    gen.Emit(Emit.OpCodes.Call, typeof<BuiltIns>.GetMethod("List", [| typeof<CTObject array> |]))

let rec emitLiteral (gen : Emit.ILGenerator) (lit : Literals.LiteralValue) =
    match lit with
    | String s -> loadStringObject gen s
    | Boolean b -> loadBooleanObject gen b
    | Number f -> loadNumberObject gen f
    | Symbol s -> loadSymbolObject gen s
    | List lits -> emitArray gen lits (emitLiteral gen)
                   convertArrayOnStackToList gen

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

let capturedLocals (clos : ClosureDefinition) (scope : Scope) =
    let isGlobalVariable id = List.contains id !builtIns ||
                              scope.procedures.ContainsKey id.uniqueName ||
                              scope.variables.ContainsKey id.uniqueName &&
                              match scope.variables.Item(id.uniqueName) with
                              | Field _ -> true
                              | LocalVar _ -> false
                              | InstanceField _ -> false
    clos.environment
    |> List.filter (isGlobalVariable >> not)

let defineProcedure (parentClass : Emit.TypeBuilder) (c : ClosureDefinition) (isInClosureFrameClass : bool) =
    let parameterTypes = match c.formals with
                         | SingleArgFormals id -> [| typeof<CTObject> |]
                         | MultiArgFormals ids -> ids |> List.map (fun _ -> typeof<CTObject> )
                                                      |> Array.ofList

    let methodAttributes =
        if isInClosureFrameClass then
            MethodAttributes.Public
        else
            MethodAttributes.Static ||| MethodAttributes.Private

    parentClass.DefineMethod(c.functionName.uniqueName,
                             methodAttributes,
                             typeof<CTObject>,
                             parameterTypes)

let createProcedureObjectOnStack (gen : Emit.ILGenerator) (c : ClosureDefinition) (isAnonymous : bool) =
    let procType = match c.formals with
                   | SingleArgFormals id -> typeof<Func<CTObject, CTObject>>
                   | MultiArgFormals ids -> match List.length ids with
                                            | 0 -> typeof<Func<CTObject>>
                                            | 1 -> typeof<Func<CTObject, CTObject>>
                                            | 2 -> typeof<Func<CTObject, CTObject, CTObject>>
                                            | 3 -> typeof<Func<CTObject, CTObject, CTObject, CTObject>>
                                            | 4 -> typeof<Func<CTObject, CTObject, CTObject, CTObject, CTObject>>
                                            | _ -> typeof<Func<CTObject, CTObject, CTObject, CTObject, CTObject, CTObject>>
    let ctObjectType = match c.formals with
                       | SingleArgFormals id -> typeof<CTDelegateProcedureVarargsList>
                       | MultiArgFormals ids -> match List.length ids with
                                                | 0 -> typeof<CTDelegateProcedure0>
                                                | 1 -> typeof<CTDelegateProcedure1>
                                                | 2 -> typeof<CTDelegateProcedure2>
                                                | 3 -> typeof<CTDelegateProcedure3>
                                                | 4 -> typeof<CTDelegateProcedure4>
                                                | _ -> typeof<CTDelegateProcedure5>

    gen.Emit(Emit.OpCodes.Newobj, procType.GetConstructor([| typeof<Object>; typeof<nativeint> |]))

    if isAnonymous then
        gen.Emit(Emit.OpCodes.Newobj, ctObjectType.GetConstructor([| procType |]))
    else
        gen.Emit(Emit.OpCodes.Ldstr, c.functionName.name)
        gen.Emit(Emit.OpCodes.Newobj, ctObjectType.GetConstructor([| procType; typeof<string> |]))

let findClosuresInScope exprs =
    let rec findClosures =
        function
        | ProcedureCall (proc, args, isTailCall)
            -> let procClosures = findClosures proc
               let closureArgs = args |> List.map findClosures |> List.concat
               List.append procClosures closureArgs
        | Conditional (cond, thenBranch, elseBranch)
            -> findClosures cond
               |> List.append (findClosures thenBranch)
               |> List.append (findClosures elseBranch)
        | SequenceExpression (seqType, exprs)
            -> exprs |> List.map findClosures |> List.concat
        | Assignment (id, expr) -> findClosures expr
        | Closure c -> [c]
        | ValueExpression _ | VariableReference _ | UndefinedValue -> []

    exprs |> List.map findClosures |> List.concat

let mapKeys map = Map.toList map |> List.map (fun (name, _) -> name)

let emitVariableLoad (gen : Emit.ILGenerator) scope (id : Identifier) isStaticContext =
    let loadVariableOrField name =
        match scope.variables.Item(name) with
        | LocalVar v -> gen.Emit(Emit.OpCodes.Ldloc, v)
        | Field v -> gen.Emit(Emit.OpCodes.Ldsfld, v)
        | InstanceField v -> gen.Emit(Emit.OpCodes.Ldarg_0)
                             gen.Emit(Emit.OpCodes.Ldfld, v)

    if scope.variables.ContainsKey(id.uniqueName) then
        loadVariableOrField id.uniqueName
    elif List.contains id !builtIns then
        loadBuiltInProcedure gen id
    else
        if id.argIndex.IsNone then
            failwithf "Attempting to load unknown variable %A, current scope contains the following variables %A"
                      id.uniqueName (mapKeys scope.variables)

        let n = id.argIndex.Value
        let index = if isStaticContext then n else n + 1
        if index < 4 then
            let opcode = match index with
                            | 0 -> Emit.OpCodes.Ldarg_0
                            | 1 -> Emit.OpCodes.Ldarg_1
                            | 2 -> Emit.OpCodes.Ldarg_2
                            | _ -> Emit.OpCodes.Ldarg_3
            gen.Emit(opcode)
        else
            gen.Emit(Emit.OpCodes.Ldarg_S, (byte) index)

let rec generateSubExpression (methodBuilder : Emit.MethodBuilder) (scope: Scope) (pushOnStack : bool) (expr : Expression) =
    let recur = generateSubExpression methodBuilder scope
    let pushExprResultToStack = recur true
    let gen = methodBuilder.GetILGenerator()

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
                       recur false expr
                   recur pushOnStack tailExpr

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
            let index = if methodBuilder.IsStatic then n else n + 1
            gen.Emit(Emit.OpCodes.Starg_S, (byte) index)
        | None ->
            assert scope.variables.ContainsKey(id.uniqueName)
            match scope.variables.Item(id.uniqueName) with
            | LocalVar v -> gen.Emit(Emit.OpCodes.Stloc, v)
            | Field v -> gen.Emit(Emit.OpCodes.Stsfld, v)
            | InstanceField v -> gen.Emit(Emit.OpCodes.Ldarg_0)
                                 gen.Emit(Emit.OpCodes.Stfld, v)

        if pushOnStack then loadUndefined gen
    let emitVariableLoad id =
        emitVariableLoad gen scope id methodBuilder.IsStatic

    let pushArgsAsArray args = emitArray gen args (generateSubExpression methodBuilder scope true)
    let pushIndividualArgs args = for arg in args do pushExprResultToStack arg
    let emitCallToFirstClassProcedureOnStack args isTailCall =
        let methodInfo = match List.length args with
                         | 0 | 1 | 2 | 3 | 4 | 5 as n
                             -> pushIndividualArgs args
                                typeof<CTObject>.GetMethod(sprintf "apply%i" n)
                         | _
                             -> pushArgsAsArray args
                                typeof<CTObject>.GetMethod("applyN")

        if isTailCall then gen.Emit(Emit.OpCodes.Tailcall)
        gen.Emit(Emit.OpCodes.Callvirt, methodInfo)
    let emitNamedProcedureCall id args isTailCall =
        // Tail annotations are not emitted for built-in procedures because
        // the built-in procedures currently defined in the library never
        // result in recursion or other tail call continuations.
        if List.contains id !builtIns then
            if List.contains id.uniqueName builtInFunctionsTakingArrayParams then
                pushArgsAsArray args
            else
                pushIndividualArgs args

            emitBuiltInFunctionCall gen id
        elif scope.procedures.ContainsKey id.uniqueName then
            let procedure = scope.procedures.Item(id.uniqueName)
            match procedure.closure.formals with
            | SingleArgFormals _ -> pushArgsAsArray args
                                    convertArrayOnStackToList gen
            | MultiArgFormals _ -> pushIndividualArgs args

            if isTailCall then gen.Emit(Emit.OpCodes.Tailcall)
            gen.Emit(Emit.OpCodes.Call, procedure.methodBuilder)
        else
            emitVariableLoad id
            emitCallToFirstClassProcedureOnStack args isTailCall

    match expr with
    | ProcedureCall (proc, args, isTailCall)
        -> match proc with
           | VariableReference id -> emitNamedProcedureCall id args isTailCall
           | e -> pushExprResultToStack e
                  emitCallToFirstClassProcedureOnStack args isTailCall
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
    | Closure c
        -> printfn "Looking for closure info for %s, scope contains %A" c.functionName.uniqueName (mapKeys scope.closureInfo)
           let closureInfo = scope.closureInfo.Item(c.functionName.uniqueName)
           match closureInfo.localFrameField with
           | Some f -> gen.Emit(Emit.OpCodes.Ldloc, f)
           | None -> gen.Emit(Emit.OpCodes.Ldnull)

           gen.Emit(Emit.OpCodes.Ldftn, closureInfo.lambdaBuilder)
           createProcedureObjectOnStack gen c true

let defineFrame (parentClass : Emit.TypeBuilder) (captures : Identifier list) parentProcedureName =
    let frameClassName = sprintf "%s$frame" parentProcedureName
    let frameClass = parentClass.DefineNestedType(frameClassName, TypeAttributes.NestedAssembly)
    // Define fields for captured variables
    let fields = captures
                 |> List.map (fun id ->
                                     let field = frameClass.DefineField(id.uniqueName,
                                                                        typeof<CTObject>,
                                                                        FieldAttributes.Assembly)
                                     (id.uniqueName, InstanceField field))

    let defaultConstructor = frameClass.DefineDefaultConstructor(MethodAttributes.Public)
    (frameClass, defaultConstructor, fields)

let createLambdaFrameScope fields scope =
    // TODO: add local procedures to scope?
    let extendedVars =
        fields
        |> List.fold (fun map (fieldName, f) ->
                            map |> Map.remove fieldName
                                |> Map.add fieldName f)
                     scope.variables
    { scope with variables = extendedVars }

let matchCaptures capturedVars captureFields =
    let fields = Map.ofList captureFields
    capturedVars
    |> List.map (fun id -> (id, fields.Item(id.uniqueName)))

let instantiateClosureFrameOnStack (gen : Emit.ILGenerator) (constructorHandle : Emit.ConstructorBuilder) captures scope isStaticContext =
    gen.Emit(Emit.OpCodes.Newobj, constructorHandle)

    for (id, InstanceField f) in captures do
        gen.Emit(Emit.OpCodes.Dup)
        emitVariableLoad gen scope id isStaticContext
        gen.Emit(Emit.OpCodes.Stfld, f)

let rec generateProcedureBody (mainClass : Emit.TypeBuilder) (parentClass : Emit.TypeBuilder) (mb : Emit.MethodBuilder) (c : ClosureDefinition) scope =
    let gen = mb.GetILGenerator()
    let closures = findClosuresInScope c.body
                   |> List.append (List.map (fun (ProcedureDefinition (_, def)) -> def) c.procedureDefinitions)
    printfn "Closures in procedure body of %A: %A" c.functionName.uniqueName (List.map (fun c -> c.functionName.uniqueName) closures)

    let vars = c.variableDeclarations
               |> List.map (fun (VariableDeclaration id) ->
                                (id.uniqueName, LocalVar <| gen.DeclareLocal(typeof<CTObject>)))
    let procs = c.procedureDefinitions
                |> List.map (fun (ProcedureDefinition (id, proc)) ->
                                 (id.uniqueName, LocalVar <| gen.DeclareLocal(typeof<CTObject>)))
    let locals = List.append vars procs

    let extendedScope = { scope with variables = Map.toSeq scope.variables
                                                 |> Seq.append locals
                                                 |> Map.ofSeq }

    // TODO: frame variable should be added to locals and references to captured variables should be bound
    //       to pass through the frame object
    // TODO: actually the scope can only be extended with locals after lambdas and frames have been created
    let closureInfo = generateClosures mainClass parentClass mb extendedScope c.functionName.uniqueName closures

    let extendedScopeWithClosures = { extendedScope with closureInfo = closureInfo }

    for ProcedureDefinition (id, proc) in c.procedureDefinitions do
        generateSubExpression mb extendedScopeWithClosures false (Assignment (id, Closure proc))

    let body = List.take (c.body.Length - 1) c.body
    let tailExpr = List.last c.body

    for expr in body do
        generateSubExpression mb extendedScopeWithClosures false expr

    generateSubExpression mb extendedScopeWithClosures true tailExpr
    gen.Emit(Emit.OpCodes.Ret)

// mainClass: Top level class in module, containing the main method.
//            Used for defined nested frame classes.
// parentClass: Parent class of current method (mb).
//              Used to house non-capturing lambda bodies as methods.
and generateClosures (mainClass : Emit.TypeBuilder) (parentClass : Emit.TypeBuilder) (mb : Emit.MethodBuilder) (scope : Scope) parentProcedureName closures =
    let makeBuilders lambdaParentClass isFrameClass =
        List.map (fun nestedClosure -> (nestedClosure.functionName.uniqueName,
                                        defineProcedure lambdaParentClass nestedClosure isFrameClass,
                                        nestedClosure))
    let generateBodies builders scope =
        for (name, lb, nestedClosure) in builders do
            generateProcedureBody mainClass parentClass lb nestedClosure scope
    let buildClosureInfoMap localFrameVar =
        List.map (fun (name, lb, _) -> (name, { lambdaBuilder = lb; localFrameField = localFrameVar }))
        >> Map.ofList

    let gen = mb.GetILGenerator()
    if List.isEmpty closures then
        Map.empty
    else
        let captures = closures
                       |> List.map (fun clos -> clos.environment)
                       |> List.concat
                       |> List.distinct
        if captures.IsEmpty then
            let isInsideFrameClass = mainClass <> parentClass
            let builders = makeBuilders parentClass isInsideFrameClass closures

            // TODO: add locally visible procedures to lambda scope before generating bodies
            //       to allow procedures to call each other
            // TODO: scope passed in should be the "lambda scope" of the previous level
            generateBodies builders scope

            buildClosureInfoMap None builders
        else
            let frameClass, cons, fields = defineFrame mainClass captures parentProcedureName
            let lambdaScope = createLambdaFrameScope fields scope

            let frameVar = gen.DeclareLocal(frameClass)
            let matchedCaptures = matchCaptures captures fields
            instantiateClosureFrameOnStack gen cons matchedCaptures scope mb.IsStatic
            gen.Emit(Emit.OpCodes.Stloc, frameVar)

            let builders = makeBuilders frameClass true closures

            // TODO: add locally visible procedures to lambda scope before generating bodies
            generateBodies builders lambdaScope

            frameClass.CreateType() |> ignore

            buildClosureInfoMap (Some frameVar) builders

let generateExpression (mb : Emit.MethodBuilder) (expr : Expression) (scope : Scope) =
    generateSubExpression mb scope false expr

let defineVariables (c : Emit.TypeBuilder)=
    List.map (fun (VariableDeclaration id) ->
                  (id.uniqueName, Field <| c.DefineField(id.uniqueName,
                                                         typeof<CTObject>,
                                                         FieldAttributes.Static ||| FieldAttributes.Private)))
    >> Map.ofList

let defineProcedures (mainClass : Emit.TypeBuilder) =
    List.map (fun (ProcedureDefinition (id, clos)) ->
                  let procedure = defineProcedure mainClass clos false
                  (id.uniqueName, { methodBuilder = procedure; closure = clos }))
    >> Map.ofList

let generateTopLevelProcedureBodies (mainClass : Emit.TypeBuilder) (scope : Scope) =
    for (_, proc) in Map.toSeq scope.procedures do
        generateProcedureBody mainClass mainClass proc.methodBuilder proc.closure scope

let generateClassInitializer (mainClass : Emit.TypeBuilder) (procs : ProcedureDefinition list) (scope : Scope) =
    let classInitializer = mainClass.DefineConstructor(MethodAttributes.Static ||| MethodAttributes.Public, CallingConventions.Standard, [||])
    let gen = classInitializer.GetILGenerator()
    for (ProcedureDefinition (id, c)) in procs do
        if c.isUsedAsFirstClassValue || c.isReassigned then
            let (Field var) = scope.variables.Item(SymbolGenerator.toProcedureObjectName id.uniqueName)
            let procedure = scope.procedures.Item(id.uniqueName)
            gen.Emit(Emit.OpCodes.Ldnull)
            gen.Emit(Emit.OpCodes.Ldftn, procedure.methodBuilder)
            createProcedureObjectOnStack gen c false
            gen.Emit(Emit.OpCodes.Stsfld, var)
    gen.Emit(Emit.OpCodes.Ret)

let generateMainMethod (mainClass : Emit.TypeBuilder) (mainMethod : Emit.MethodBuilder) (program : ProgramStructure) scope =
    let closures = findClosuresInScope program.expressions
    let closureInfo = generateClosures mainClass mainClass mainMethod scope "Main" closures
    let extendedScope = { scope with closureInfo = closureInfo }

    for expr in program.expressions do
        generateExpression mainMethod expr extendedScope

let generateMainModule (mainClass : Emit.TypeBuilder) (mainMethod : Emit.MethodBuilder) (program : ProgramStructure) =
    let ilGen = mainMethod.GetILGenerator()
    let procedures = defineProcedures mainClass program.procedureDefinitions
    let variables = defineVariables mainClass program.variableDeclarations

    let scope = { variables = variables;
                  procedures = procedures;
                  closureInfo = Map.empty }

    generateTopLevelProcedureBodies mainClass scope
    generateClassInitializer mainClass program.procedureDefinitions scope

    ilGen.BeginExceptionBlock() |> ignore

    generateMainMethod mainClass mainMethod program scope

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
