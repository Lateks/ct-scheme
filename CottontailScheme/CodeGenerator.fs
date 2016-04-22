﻿module CottontailScheme.CodeGenerator

open Scope
open SemanticAnalysis
open Literals

open CottontailSchemeLib

open System
open System.Reflection

exception CodeGenException of string

type CodeGenResult = CodeGenInternalError of string
                   | CodeGenSuccess of string

type Procedure = { methodBuilder : Emit.MethodBuilder;
                   closure : ClosureDefinition }

type Variable = Field of Emit.FieldBuilder
              | LocalVar of Emit.LocalBuilder
              | InstanceField of Emit.FieldBuilder
              | ObjectField of Emit.FieldBuilder * Variable

type FrameVariable = StaticInstance of Emit.FieldBuilder
                   | LocalInstance of Emit.LocalBuilder

type ClosureLoadInfo = { lambdaBuilder : Emit.MethodBuilder;
                         localFrameField : FrameVariable;
                         index : int }

type Scope = { variables : Map<string, Variable>;
               procedures : Map<string, Procedure>;
               closureInfo : Map<string, ClosureLoadInfo> }

type ArgCount = NumArgs of int
              | VarArgs

type ClosureMapping = (ArgCount * (int * Procedure) list) list

type Frame = { frameClass : Emit.TypeBuilder
               frameFields: (string * Emit.FieldBuilder) list
               staticLink: Emit.FieldBuilder option
               parent: Frame option
               frameVariable : FrameVariable
               closureMapping : ClosureMapping }

type FrameInfo = { closureInfo : Map<string, ClosureLoadInfo>
                   frameVariable : FrameVariable
                   matchedCapturesFromCurrentScope : (Identifier * Variable) list
                   closureMappingForParentFrame : ClosureMapping }

type ClosureHandleTypeInformation = { tb : Emit.TypeBuilder
                                      anonymousClosureConstructor : Emit.ConstructorBuilder
                                      namedClosureConstructor : Emit.ConstructorBuilder
                                      anonymousVarargsClosureConstructor : Emit.ConstructorBuilder
                                      namedVarargsClosureConstructor : Emit.ConstructorBuilder
                                      fields : Map<string, Emit.FieldBuilder>
                                      methods : Map<string, Emit.MethodBuilder> }

type TopLevelTypes = { mainClass : Emit.TypeBuilder
                       procedureHandleClass : ClosureHandleTypeInformation }

let argCount c =
    match c.formals with
    | MultiArgFormals ids -> List.length ids |> NumArgs
    | SingleArgFormals _ -> VarArgs

let indexList startIndex procs =
    let count = List.length procs
    let endIndex = startIndex + count - 1
    List.zip (Seq.toList <| seq{startIndex..endIndex}) procs

let mainMethodName = "Main"
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
    | "list" | "display" | "car" | "cdr" | "cons" | "not" | "newline" as n
        -> SymbolGenerator.capitalizeWord n
    | e -> sprintf "Built-in function %s is not implemented!" e |> CodeGenException |> raise

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
                              | ObjectField _ -> false
    clos.environment
    |> List.filter (isGlobalVariable >> not)

let defineProcedure (parentClass : Emit.TypeBuilder) (c : ClosureDefinition) (isInstanceMethod : bool) =
    let parameterTypes = match c.formals with
                         | SingleArgFormals id -> [| typeof<CTObject> |]
                         | MultiArgFormals ids -> ids |> List.map (fun _ -> typeof<CTObject> )
                                                      |> Array.ofList

    let methodAttributes =
        if isInstanceMethod then
            MethodAttributes.Public
        else
            MethodAttributes.Static ||| MethodAttributes.Private

    parentClass.DefineMethod(c.functionName.uniqueName,
                             methodAttributes,
                             typeof<CTObject>,
                             parameterTypes)

let createProcedureObjectOnStack (topLevelTypes : TopLevelTypes) (gen : Emit.ILGenerator) (c : ClosureDefinition) (closureLoadInfo : ClosureLoadInfo) =
    // Load frame instance reference
    match closureLoadInfo.localFrameField with
    | StaticInstance f -> gen.Emit(Emit.OpCodes.Ldsfld, f)
    | LocalInstance f -> gen.Emit(Emit.OpCodes.Ldloc, f)

    // Load closure index
    gen.Emit(Emit.OpCodes.Ldc_I4, closureLoadInfo.index)

    match c.formals with
    | SingleArgFormals id
        -> gen.Emit(Emit.OpCodes.Newobj, topLevelTypes.procedureHandleClass.anonymousVarargsClosureConstructor)
    | MultiArgFormals ids
        -> let argCount = List.length ids
           // Load argument count
           gen.Emit(Emit.OpCodes.Ldc_I4_S, (byte) argCount)
           gen.Emit(Emit.OpCodes.Newobj, topLevelTypes.procedureHandleClass.anonymousClosureConstructor)

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

let rec loadVariableOrField (gen : Emit.ILGenerator) var =
    match var with
    | LocalVar v -> gen.Emit(Emit.OpCodes.Ldloc, v)
    | Field v -> gen.Emit(Emit.OpCodes.Ldsfld, v)
    | InstanceField v -> gen.Emit(Emit.OpCodes.Ldarg_0)
                         gen.Emit(Emit.OpCodes.Ldfld, v)
    | ObjectField (v, obj) -> loadVariableOrField gen obj
                              gen.Emit(Emit.OpCodes.Ldfld, v)

let loadVariableOrFieldWithName (gen : Emit.ILGenerator) scope name =
    scope.variables.Item(name) |> loadVariableOrField gen

let emitVariableLoad (gen : Emit.ILGenerator) scope (id : Identifier) isStaticContext =
    if scope.variables.ContainsKey(id.uniqueName) then
        loadVariableOrFieldWithName gen scope id.uniqueName
    elif List.contains id !builtIns then
        loadBuiltInProcedure gen id
    else
        if id.argIndex.IsNone then
            sprintf "Attempting to load unknown variable %s, current scope contains the following variables %A" id.uniqueName (mapKeys scope.variables)
            |> CodeGenException
            |> raise

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

let rec generateSubExpression (topLevelTypes : TopLevelTypes) (methodBuilder : Emit.MethodBuilder) (scope: Scope) (pushOnStack : bool) (emitReturn : bool) (expr : Expression) =
    let recurInScope = generateSubExpression topLevelTypes methodBuilder scope
    let recurInNonTailContext push expr = recurInScope push false expr
    let pushExprResultToStack = recurInNonTailContext true
    let gen = methodBuilder.GetILGenerator()

    let emitConditional condition thenExpression elseExpression =
        let elseLabel = gen.DefineLabel()
        let exitLabel = gen.DefineLabel()

        pushExprResultToStack condition
        emitBooleanConversion gen
        gen.Emit(Emit.OpCodes.Brfalse, elseLabel)

        recurInScope true emitReturn thenExpression

        if not emitReturn then
            gen.Emit(Emit.OpCodes.Br, exitLabel)

        gen.MarkLabel(elseLabel)
        recurInScope true emitReturn elseExpression

        if not emitReturn then
            gen.MarkLabel(exitLabel)
            if not pushOnStack then popStack gen

    let emitBeginSequence =
        function
        | [] -> if pushOnStack || emitReturn then loadUndefined gen
                if emitReturn then gen.Emit(Emit.OpCodes.Ret)
        | exprs -> let tailExpr = List.last exprs
                   let nonTailExprs = List.take (exprs.Length - 1) exprs
                   for expr in nonTailExprs do
                       recurInNonTailContext false expr
                   generateSubExpression topLevelTypes methodBuilder scope pushOnStack emitReturn tailExpr

    let emitBooleanCombinationExpression breakValue =
        function
        | [] -> if pushOnStack || emitReturn then loadBooleanObject gen (not breakValue)
                if emitReturn then gen.Emit(Emit.OpCodes.Ret)
        | exprs ->
            let exitLabel = gen.DefineLabel()
            let tempVar = if pushOnStack || emitReturn then
                             Some <| gen.DeclareLocal(typeof<CTObject>)
                          else
                             None
            let storeTempValue () = tempVar |> Option.map (fun (v : Emit.LocalBuilder) -> gen.Emit(Emit.OpCodes.Stloc, v))
                                            |> ignore
            let loadTempValue () = tempVar |> Option.map (fun (v : Emit.LocalBuilder) -> gen.Emit(Emit.OpCodes.Ldloc, v))
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

            if emitReturn then
                recurInScope false true tailExpr
            else
                pushExprResultToStack tailExpr
                if pushOnStack then
                    storeTempValue ()
                else
                    popStack gen

            gen.MarkLabel(exitLabel)
            loadTempValue ()
            if emitReturn then
                gen.Emit(Emit.OpCodes.Ret)

    let emitVariableLoad id =
        emitVariableLoad gen scope id methodBuilder.IsStatic

    let emitAssignment (id : Identifier) expr =
        let push () = pushExprResultToStack expr

        if scope.variables.ContainsKey(id.uniqueName) then
            match scope.variables.Item(id.uniqueName) with
            | LocalVar v -> push ()
                            gen.Emit(Emit.OpCodes.Stloc, v)
            | Field v -> push ()
                         gen.Emit(Emit.OpCodes.Stsfld, v)
            | InstanceField v -> gen.Emit(Emit.OpCodes.Ldarg_0)
                                 push ()
                                 gen.Emit(Emit.OpCodes.Stfld, v)
            | ObjectField (v, obj) -> loadVariableOrField gen obj
                                      push ()
                                      gen.Emit(Emit.OpCodes.Stfld, v)
        else
            match id.argIndex with
            | Some n ->
                push ()
                let index = if methodBuilder.IsStatic then n else n + 1
                gen.Emit(Emit.OpCodes.Starg_S, (byte) index)
            | None -> sprintf "Attempting to set! unknown variable %A" id |> CodeGenException |> raise

    let pushArgsAsArray args = emitArray gen args (generateSubExpression topLevelTypes methodBuilder scope true false)
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
    | ValueExpression lit
        -> emitLiteral gen lit
    | Conditional (cond, thenBranch, elseBranch)
        -> emitConditional cond thenBranch elseBranch
    | SequenceExpression (seqType, exprs)
        -> match seqType with
           | BeginSequence -> emitBeginSequence exprs
           | AndSequence -> emitBooleanCombinationExpression false exprs
           | OrSequence -> emitBooleanCombinationExpression true exprs
    | Assignment (id, expr)
        -> emitAssignment id expr
    | VariableReference id
        -> emitVariableLoad id
    | Closure c
        -> let closureInfo = scope.closureInfo.Item(c.functionName.uniqueName)
           createProcedureObjectOnStack topLevelTypes gen c closureInfo
    | UndefinedValue -> ()

    match expr with
    | Conditional _ | SequenceExpression _ -> ()
    | Assignment _
    | UndefinedValue _
        -> if emitReturn then
              loadUndefined gen
              gen.Emit(Emit.OpCodes.Ret)
           elif pushOnStack then
              loadUndefined gen
    | _ -> if emitReturn then
              gen.Emit(Emit.OpCodes.Ret)
           elif not pushOnStack then
              popStack gen

let generateExpression (topLevelTypes : TopLevelTypes) (mb : Emit.MethodBuilder) (expr : Expression) (scope : Scope) emitReturn =
    generateSubExpression topLevelTypes mb scope false emitReturn expr

let defineFrame (mainClass : Emit.TypeBuilder) (parentClass : Emit.TypeBuilder) (capturesFromCurrentScope : Identifier list) (captureParentFrame : bool) parentProcedureName =
    let frameClassName = sprintf "%s$frame" parentProcedureName
    let frameClass = mainClass.DefineNestedType(frameClassName, TypeAttributes.NestedAssembly, typeof<ProcedureFrame>)

    let fields = capturesFromCurrentScope
                 |> List.map (fun id ->
                                  let field = frameClass.DefineField(id.uniqueName, typeof<CTObject>, FieldAttributes.Assembly)
                                  (id.uniqueName, InstanceField field))

    let frameField = if captureParentFrame then
                        let field = frameClass.DefineField("staticLink", parentClass, FieldAttributes.Assembly)
                        Some field
                     else
                        None

    let defaultConstructor = frameClass.DefineDefaultConstructor(MethodAttributes.Public)
    (frameClass, defaultConstructor, fields, frameField)

let matchCaptures capturedVars captureFields =
    let fields = Map.ofList captureFields
    capturedVars
    |> List.map (fun id -> (id, fields.Item(id.uniqueName)))

let instantiateClosureFrameOnStack (gen : Emit.ILGenerator) (constructorHandle : Emit.ConstructorBuilder) captures scope isStaticContext =
    gen.Emit(Emit.OpCodes.Newobj, constructorHandle)

    for (id, var) in captures do
        match var with
        | InstanceField f ->
            gen.Emit(Emit.OpCodes.Dup)
            emitVariableLoad gen scope id isStaticContext
            gen.Emit(Emit.OpCodes.Stfld, f)
        | f -> sprintf "Closure frame instantiation failed, expected %s to be an instance field but was %A" id.uniqueName f
               |> CodeGenException
               |> raise

let rebindFrameFields frameVar scope captures =
        let frameVariable = match frameVar with
                            | StaticInstance f -> Field f
                            | LocalInstance f -> LocalVar f

        // Binds captured local variable names and argument names
        // to fields on the frame variable
        let rec rebindName name scope fb =
            if scope.variables.ContainsKey name then
                match scope.variables.Item(name) with
                | InstanceField _ | ObjectField _ -> scope.variables
                | Field _ | LocalVar _ as f -> sprintf "Unexpected field found in captures (%s): %A" name f |> CodeGenException |> raise
            else
                let frameFieldReference = ObjectField (fb, frameVariable)
                Map.add name frameFieldReference scope.variables
        let rec rebind newScope =
            function
            | [] -> newScope
            | (id, (InstanceField f)) :: xs
                -> let newVars = rebindName id.uniqueName newScope f
                   let scope' = { newScope with variables = newVars }
                   rebind scope' xs
            | (_, f)::_ -> sprintf "Error, expected an instance field but got %A" f |> CodeGenException |> raise

        rebind scope captures

let generateFuncallMethods (frameClass : Emit.TypeBuilder) (mapping : ClosureMapping) =
    let parentClass = typeof<ProcedureFrame>

    let loadArgs (gen : Emit.ILGenerator) argc =
        match argc with
        | NumArgs n ->
            for i in seq{2..n+1} do
                gen.Emit(Emit.OpCodes.Ldarg_S, (byte) i)
        | VarArgs ->
            gen.Emit(Emit.OpCodes.Ldarg_2)

    let overrideFuncall argc closures =
        let sortedClosures = closures |> List.sortBy (fun (i, _) -> i)
        if List.isEmpty sortedClosures then
            ()
        else
            let methodName = match argc with
                             | NumArgs n -> sprintf "funcall%i" n
                             | VarArgs -> "funcallVarargs"

            let paramTypes = match argc with
                             | NumArgs n -> seq{1..n} |> List.ofSeq |> List.map (fun _ -> typeof<CTObject>)
                             | VarArgs -> [typeof<CTObject>]
                             |> fun l -> typeof<int> :: l
                             |> List.toArray

            let mb = frameClass.DefineMethod(methodName, MethodAttributes.Public ||| MethodAttributes.Virtual, typeof<CTObject>, paramTypes)
            let gen = mb.GetILGenerator()
            let labels = sortedClosures |> List.map (fun _ -> gen.DefineLabel())
            let failureLabel = gen.DefineLabel()

            gen.Emit(Emit.OpCodes.Ldarg_1)
            gen.Emit(Emit.OpCodes.Switch, List.toArray labels)
            gen.Emit(Emit.OpCodes.Br, failureLabel)

            for (label, (_, proc)) in List.zip labels sortedClosures do
                gen.MarkLabel(label)

                if not proc.methodBuilder.IsStatic then
                    gen.Emit(Emit.OpCodes.Ldarg_0)

                loadArgs gen argc

                gen.Emit(Emit.OpCodes.Tailcall)
                gen.Emit(Emit.OpCodes.Call, proc.methodBuilder)
                gen.Emit(Emit.OpCodes.Ret)

            gen.MarkLabel(failureLabel)
            gen.Emit(Emit.OpCodes.Ldarg_0)
            gen.Emit(Emit.OpCodes.Ldarg_1)
            loadArgs gen argc
            gen.Emit(Emit.OpCodes.Call, parentClass.GetMethod(methodName))
            gen.Emit(Emit.OpCodes.Ret)

            frameClass.DefineMethodOverride(mb, parentClass.GetMethod(methodName))

    for (argc, procs) in mapping do
        overrideFuncall argc procs

let mergeClosureMappings (mappings : ClosureMapping list) =
    let merge map (argc, procs) =
        match Map.tryFind argc map with
        | Some p ->
            let combinedProcs = List.append procs p
            map
            |> Map.remove argc
            |> Map.add argc combinedProcs
        | None ->
            Map.add argc procs map

    let rec mergeMappings mappings mapAcc =
        match mappings with
        | [] -> mapAcc
        | x::xs
            -> let newAcc = List.fold merge mapAcc x
               mergeMappings xs newAcc

    if List.isEmpty mappings then
        []
    else
        let firstMap = List.head mappings |> Map.ofList
        mergeMappings (List.tail mappings) firstMap
        |> Map.toList

// Helper functions for handling closure definitions.
let getFormals (c : ClosureDefinition) =
    match c.formals with
    | SingleArgFormals id -> [id]
    | MultiArgFormals ids -> ids

let getLocalVariableIds (c : ClosureDefinition) =
    c.variableDeclarations |> List.map (fun (VariableDeclaration id) -> id)

let getLocalProcedureIds (c : ClosureDefinition) =
    c.procedureDefinitions |> List.map (fun (ProcedureDefinition (id, _)) -> id)

let getVariableIdsInScope (c : ClosureDefinition) =
    List.append (getLocalVariableIds c) (getLocalProcedureIds c)

let rec generateProcedureBody (topLevelTypes : TopLevelTypes) (parentFrame : Frame) (mb : Emit.MethodBuilder) (c : ClosureDefinition) scope =
    let gen = mb.GetILGenerator()
    let closures = let namedProcedures = c.procedureDefinitions
                                         |> List.map (fun (ProcedureDefinition (_, def)) -> def)
                   let anonymousProcedures = findClosuresInScope c.body
                   List.append namedProcedures anonymousProcedures

    // Create frame classes and closure body methods.
    let frameInfo = generateClosures topLevelTypes mb scope (Some c) parentFrame closures
    let scopeWithFrameReferences = rebindFrameFields frameInfo.frameVariable scope frameInfo.matchedCapturesFromCurrentScope

    // Find local variables that were not captured in the frame and
    // add them to the local scope.
    let capturedVarIds = frameInfo.matchedCapturesFromCurrentScope |> List.map (fun (id, _) -> id)
    let newLocals = getVariableIdsInScope c
                    |> List.filter (fun id -> not <| List.contains id capturedVarIds)
                    |> List.map (fun id -> (id.uniqueName, LocalVar <| gen.DeclareLocal(typeof<CTObject>)))

    let scopeWithLocalFieldsAndClosures =
        { scopeWithFrameReferences with
            variables = Map.toSeq scopeWithFrameReferences.variables
                        |> Seq.append newLocals
                        |> Map.ofSeq
            closureInfo = frameInfo.closureInfo }

    // Populate (assign) the local variables that hold closures.
    for ProcedureDefinition (id, proc) in c.procedureDefinitions do
        generateExpression topLevelTypes mb (Assignment (id, Closure proc)) scopeWithLocalFieldsAndClosures false

    // Generate procedure body.
    let body = List.take (c.body.Length - 1) c.body
    let tailExpr = List.last c.body

    for expr in body do
        generateExpression topLevelTypes mb expr scopeWithLocalFieldsAndClosures false

    generateExpression topLevelTypes mb tailExpr scopeWithLocalFieldsAndClosures true

    frameInfo.closureMappingForParentFrame

and generateClosures (topLevelTypes : TopLevelTypes) (mb : Emit.MethodBuilder) (scope : Scope) (parentProcedure : ClosureDefinition option) (currentFrame : Frame) closures =
    let gen = mb.GetILGenerator()
    let localVars, args, parentProcedureName =
        match parentProcedure with
        | Some p -> getVariableIdsInScope p, getFormals p, p.functionName.uniqueName
        | None -> [], [], "Main"
    let parentClass = currentFrame.frameClass

    let makeBuilders lambdaParentClass asInstanceMethods =
        List.map (fun nestedClosure -> { methodBuilder = defineProcedure lambdaParentClass nestedClosure asInstanceMethods
                                         closure = nestedClosure })

    let generateBodies builders frame scope =
        builders
        |> List.map (fun proc ->
                         generateProcedureBody topLevelTypes frame proc.methodBuilder proc.closure scope)

    let buildClosureInfoMap localFrameVar closureMapping =
        closureMapping
        |> List.map (fun (_, indexedProcs) -> indexedProcs)
        |> List.concat
        |> List.map (fun (i, proc) -> (proc.closure.functionName.uniqueName,
                                       { lambdaBuilder = proc.methodBuilder; localFrameField = localFrameVar; index = i }))
        |> Map.ofList

    let isArgument id = List.contains id args

    let capturesFromCurrentScope captures =
        let isLocal id = List.contains id localVars
        captures |> List.filter (fun id -> isArgument id || isLocal id)

    let createFrame captures =
        let currentScopeCaptures = capturesFromCurrentScope captures
        let captureParentFrame = List.length captures <> List.length currentScopeCaptures
        let frameClass, cons, fields, capturedFrameField = defineFrame topLevelTypes.mainClass parentClass currentScopeCaptures captureParentFrame parentProcedureName

        let frameVar = gen.DeclareLocal(frameClass)
        let matchedCapturesInCurrentScope = matchCaptures currentScopeCaptures fields

        // Local variables are not instantiated until the parent method body assigns them
        let capturedArguments = matchedCapturesInCurrentScope |> List.filter (fun (id, _) -> isArgument id)
        instantiateClosureFrameOnStack gen cons capturedArguments scope mb.IsStatic

        // Capture parent frame as a field.
        match capturedFrameField with
        | None -> ()
        | Some f -> gen.Emit(Emit.OpCodes.Dup)
                    gen.Emit(Emit.OpCodes.Ldarg_0)
                    gen.Emit(Emit.OpCodes.Stfld, f)
        gen.Emit(Emit.OpCodes.Stloc, frameVar)

        let frameFields = fields |> List.map (fun (n, var) ->
                                                  match var with
                                                  | InstanceField f -> (n, f)
                                                  | f -> sprintf "Frame generation failed: expected %s to be an instance field but was %A" n f |> CodeGenException |> raise)

        let newFrame = { parent = Some currentFrame;
                         frameFields = frameFields;
                         staticLink = capturedFrameField;
                         frameClass = frameClass;
                         frameVariable = LocalInstance frameVar;
                         closureMapping = [] }

        newFrame, matchedCapturesInCurrentScope

    let extendScopeWithFrameFields fields scope =
        let extendedVars =
            fields
            |> List.fold (fun map (fieldName, f) ->
                              map |> Map.remove fieldName
                                  |> Map.add fieldName (InstanceField f))
                         scope.variables
        { scope with variables = extendedVars }

    let createIndirectReferenceTo staticLink name =
        let fail () = sprintf "Cannot build an indirect reference to field %s" name |> CodeGenException |> raise
        let rec buildIndirectReferenceChain frameOpt =
            if Option.isNone frameOpt then fail ()

            let frame = Option.get frameOpt
            let fieldMap = Map.ofList frame.frameFields
            if fieldMap.ContainsKey name then
                [fieldMap.Item(name)]
            else
               if Option.isNone frame.staticLink then fail ()
               let link = Option.get frame.staticLink
               link :: (buildIndirectReferenceChain frame.parent)
        let rec buildVarChain fbs =
            match fbs with
            | []    -> fail ()
            | x::[] -> InstanceField x
            | x::xs -> ObjectField (x, buildVarChain xs)

        let referenceChain = staticLink :: (buildIndirectReferenceChain (Some currentFrame))
                             |> List.rev
        buildVarChain referenceChain

    let extendScopeWithIndirectlyAccessibleFields nonLocalCaptures staticLinkField scope =
        match staticLinkField with
        | None -> scope
        | Some f ->
            let newVars =
                nonLocalCaptures
                |> List.fold (fun map id ->
                                  let variable = createIndirectReferenceTo f id.uniqueName
                                  map |> Map.remove id.uniqueName
                                      |> Map.add id.uniqueName variable)
                             scope.variables
            { scope with variables = newVars }

    let generateIdentifiersForClosures builders frame =
        let currentMapping = Map.ofList frame.closureMapping
        builders
        |> List.groupBy (fun proc -> argCount proc.closure)
        |> List.map (fun (argc, procs) ->
                         let startingIndex = match Map.tryFind argc currentMapping with
                                             | None -> 0
                                             | Some indexedProcs ->
                                                 indexedProcs
                                                 |> List.map (fun (i, p) -> i)
                                                 |> List.max
                                                 |> fun x -> x + 1
                         (argc, indexList startingIndex procs))

    if List.isEmpty closures then
        { closureInfo = Map.empty;
          frameVariable = currentFrame.frameVariable;
          matchedCapturesFromCurrentScope = []
          closureMappingForParentFrame = [] }
    else
        let captures = closures
                       |> List.map (fun clos -> clos.environment)
                       |> List.concat
                       |> List.distinct
        if captures.IsEmpty then
            let isInsideMainClass = parentClass = topLevelTypes.mainClass
            let builders = makeBuilders parentClass (not isInsideMainClass) closures

            let closuresWithIdentifiers = generateIdentifiersForClosures builders currentFrame
            let frameWithClosureMapping = { currentFrame with closureMapping = mergeClosureMappings [closuresWithIdentifiers; currentFrame.closureMapping] }

            let closuresFromNestedFrames = generateBodies builders frameWithClosureMapping scope
            let closureMapping = closuresWithIdentifiers :: closuresFromNestedFrames |> mergeClosureMappings

            { closureInfo = buildClosureInfoMap currentFrame.frameVariable closureMapping;
              frameVariable = currentFrame.frameVariable;
              matchedCapturesFromCurrentScope = [];
              closureMappingForParentFrame = closureMapping }
        else
            let newFrame, matchedCapturesInCurrentScope = createFrame captures

            let isNonLocalCapture = let captureIds = matchedCapturesInCurrentScope |> List.map (fun (id, _) -> id)
                                    fun id -> captureIds |> List.contains id |> not
            let nonLocalCaptures = captures |> List.filter isNonLocalCapture
            let lambdaScope = scope |> extendScopeWithFrameFields newFrame.frameFields
                                    |> extendScopeWithIndirectlyAccessibleFields nonLocalCaptures newFrame.staticLink

            let builders = makeBuilders newFrame.frameClass true closures
            let closuresWithIdentifiers = generateIdentifiersForClosures builders newFrame
            let frameWithClosureMapping = { newFrame with closureMapping = closuresWithIdentifiers }

            let closuresFromNestedFrames = generateBodies builders frameWithClosureMapping lambdaScope
            let closureMapping = closuresWithIdentifiers :: closuresFromNestedFrames |> mergeClosureMappings

            generateFuncallMethods newFrame.frameClass closureMapping

            frameWithClosureMapping.frameClass.CreateType() |> ignore

            { closureInfo = buildClosureInfoMap frameWithClosureMapping.frameVariable closureMapping;
              frameVariable = frameWithClosureMapping.frameVariable;
              matchedCapturesFromCurrentScope = matchedCapturesInCurrentScope;
              closureMappingForParentFrame = [] }

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

let generateTopLevelProcedureBodies (topLevelTypes : TopLevelTypes) (topLevelFrame : Frame) (scope : Scope) =
    Map.toList scope.procedures
    |> List.map (fun (_, proc) ->
                     generateProcedureBody topLevelTypes topLevelFrame proc.methodBuilder proc.closure scope)

let createTopLevelClosureMapping (procs : ProcedureDefinition list) (scope : Scope) =
    let usedAsFirstClassValueOrReassigned (ProcedureDefinition (_, c)) = c.isUsedAsFirstClassValue || c.isReassigned
    let argCount (ProcedureDefinition (_, c)) = argCount c
    let numberProcedureDefinitions (argc, procDefs) = (argc, indexList 0 procDefs)
    let resolveProcedures (argc, numberedDefinitions) =
        let closureInfo = numberedDefinitions
                          |> List.map (fun (i, proc) -> let (ProcedureDefinition (id, _)) = proc
                                                        let p = scope.procedures.Item(id.uniqueName)
                                                        (i, p))
        (argc, closureInfo)

    procs
    |> List.filter usedAsFirstClassValueOrReassigned
    |> List.groupBy argCount
    |> List.map numberProcedureDefinitions
    |> List.map resolveProcedures

let generateClassInitializer (topLevelTypes : TopLevelTypes) (closureMapping : ClosureMapping) (instanceField : Emit.FieldBuilder) (scope : Scope) =
    let classInitializer = topLevelTypes.mainClass.DefineConstructor(MethodAttributes.Static ||| MethodAttributes.Public,
                                                                        CallingConventions.Standard,
                                                                        [||])
    let defaultConstructor = topLevelTypes.mainClass.DefineDefaultConstructor(MethodAttributes.Public)

    let gen = classInitializer.GetILGenerator()
    gen.Emit(Emit.OpCodes.Newobj, defaultConstructor)
    gen.Emit(Emit.OpCodes.Stsfld, instanceField)

    let assignField (argCount : ArgCount) (index : int) (procedure : Procedure) =
        let fieldName = SymbolGenerator.toProcedureObjectName procedure.closure.functionName.uniqueName
        let var = match scope.variables.Item(fieldName) with
                    | Field v -> v
                    | v -> sprintf "Expected %s to be a field but was %A" fieldName v |> CodeGenException |> raise

        gen.Emit(Emit.OpCodes.Ldsfld, instanceField)
        gen.Emit(Emit.OpCodes.Ldc_I4, index)

        let procedureName = procedure.closure.functionName.name
        match argCount with
        | VarArgs -> gen.Emit(Emit.OpCodes.Ldstr, procedureName)
                     gen.Emit(Emit.OpCodes.Newobj, topLevelTypes.procedureHandleClass.namedVarargsClosureConstructor)
        | NumArgs n -> gen.Emit(Emit.OpCodes.Ldc_I4, n)
                       gen.Emit(Emit.OpCodes.Ldstr, procedureName)
                       gen.Emit(Emit.OpCodes.Newobj, topLevelTypes.procedureHandleClass.namedClosureConstructor)

        gen.Emit(Emit.OpCodes.Stsfld, var)

    for (argCount, procs) in closureMapping do
        for (index, proc) in procs do
            assignField argCount index proc

    gen.Emit(Emit.OpCodes.Ret)

let generateMainMethod (topLevelTypes : TopLevelTypes) (mainMethod : Emit.MethodBuilder) (program : ProgramStructure) (topLevelFrame : Frame) scope =
    let closures = findClosuresInScope program.expressions
    let frameInfo = generateClosures topLevelTypes mainMethod scope None topLevelFrame closures
    let scopeWithClosures = { scope with closureInfo = frameInfo.closureInfo }

    for expr in program.expressions do
        generateExpression topLevelTypes mainMethod expr scopeWithClosures false

    frameInfo.closureMappingForParentFrame

let generateMainModule (topLevelTypes : TopLevelTypes) (mainMethod : Emit.MethodBuilder) (program : ProgramStructure) =
    let mainClass = topLevelTypes.mainClass
    let gen = mainMethod.GetILGenerator()

    let procedures = defineProcedures mainClass program.procedureDefinitions
    let variables = defineVariables mainClass program.variableDeclarations

    let scope = { variables = variables;
                  procedures = procedures;
                  closureInfo = Map.empty }

    let instanceField = mainClass.DefineField("_instance", mainClass, FieldAttributes.Assembly ||| FieldAttributes.Static)
    let closureMapping = createTopLevelClosureMapping program.procedureDefinitions scope

    generateClassInitializer topLevelTypes closureMapping instanceField scope

    let frame = { frameClass = topLevelTypes.mainClass
                  frameFields = []
                  staticLink = None
                  parent = None
                  frameVariable = StaticInstance instanceField
                  closureMapping = closureMapping }

    let closuresFromProcedureBodies = generateTopLevelProcedureBodies topLevelTypes frame scope

    gen.BeginExceptionBlock() |> ignore

    let updatedFrame = { frame with closureMapping = (closureMapping :: closuresFromProcedureBodies) |> mergeClosureMappings}
    let closuresFromMainMethodBody = generateMainMethod topLevelTypes mainMethod program updatedFrame scope

    gen.BeginCatchBlock(typeof<CottontailSchemeException>)

    gen.Emit(Emit.OpCodes.Ldstr, "\nError:")
    gen.Emit(Emit.OpCodes.Call, typeof<Console>.GetMethod("WriteLine", [| typeof<string> |]))
    gen.Emit(Emit.OpCodes.Callvirt, typeof<Exception>.GetProperty("Message").GetGetMethod())
    gen.Emit(Emit.OpCodes.Call, typeof<Console>.GetMethod("WriteLine", [| typeof<string> |]))

    gen.EndExceptionBlock()

    gen.Emit(Emit.OpCodes.Ldc_I4_0)
    gen.Emit(Emit.OpCodes.Ret)

    let finalClosureMapping = [updatedFrame.closureMapping; closuresFromMainMethodBody] |> mergeClosureMappings
    generateFuncallMethods topLevelTypes.mainClass finalClosureMapping

let generateProcedureParentClass (moduleBuilder : Emit.ModuleBuilder) =
    let parentType = typeof<CTProcedure>

    // Type
    let tcProcType = moduleBuilder.DefineType("CTTailCallProcedureHandle", TypeAttributes.Public ||| TypeAttributes.Class, parentType)

    // Fields
    let indexField = tcProcType.DefineField("index", typeof<int>, FieldAttributes.Assembly)
    let frameField = tcProcType.DefineField("frame", typeof<ProcedureFrame>, FieldAttributes.Assembly)

    // Constructors
    let namedConstructor = tcProcType.DefineConstructor(MethodAttributes.Public, CallingConventions.Standard, [| typeof<ProcedureFrame>; typeof<int>; typeof<int>; typeof<string> |])
    let namedVarargsConstructor = tcProcType.DefineConstructor(MethodAttributes.Public, CallingConventions.Standard, [| typeof<ProcedureFrame>; typeof<int>; typeof<string>|])
    let anonymousConstructor = tcProcType.DefineConstructor(MethodAttributes.Public, CallingConventions.Standard, [| typeof<ProcedureFrame>; typeof<int>; typeof<int>; |])
    let anonymousVarargsConstructor = tcProcType.DefineConstructor(MethodAttributes.Public, CallingConventions.Standard, [| typeof<ProcedureFrame>; typeof<int>; |])

    // Methods
    let apply0 = tcProcType.DefineMethod("apply0", MethodAttributes.Public ||| MethodAttributes.Virtual, typeof<CTObject>, [||])
    let apply1 = tcProcType.DefineMethod("apply1", MethodAttributes.Public ||| MethodAttributes.Virtual, typeof<CTObject>, [| typeof<CTObject> |])
    let apply2 = tcProcType.DefineMethod("apply2", MethodAttributes.Public ||| MethodAttributes.Virtual, typeof<CTObject>, [| typeof<CTObject>; typeof<CTObject> |])
    let apply3 = tcProcType.DefineMethod("apply3", MethodAttributes.Public ||| MethodAttributes.Virtual, typeof<CTObject>, [| typeof<CTObject>; typeof<CTObject>; typeof<CTObject> |])
    let apply4 = tcProcType.DefineMethod("apply4", MethodAttributes.Public ||| MethodAttributes.Virtual, typeof<CTObject>, [| typeof<CTObject>; typeof<CTObject>; typeof<CTObject>; typeof<CTObject> |])
    let apply5 = tcProcType.DefineMethod("apply5", MethodAttributes.Public ||| MethodAttributes.Virtual, typeof<CTObject>, [| typeof<CTObject>; typeof<CTObject>; typeof<CTObject>; typeof<CTObject>; typeof<CTObject> |])
    let applyN = tcProcType.DefineMethod("applyN", MethodAttributes.Public ||| MethodAttributes.Virtual, typeof<CTObject>, [| typeof<CTObject array> |])

    tcProcType.DefineMethodOverride(apply0, parentType.GetMethod("apply0"))
    tcProcType.DefineMethodOverride(apply1, parentType.GetMethod("apply1"))
    tcProcType.DefineMethodOverride(apply2, parentType.GetMethod("apply2"))
    tcProcType.DefineMethodOverride(apply3, parentType.GetMethod("apply3"))
    tcProcType.DefineMethodOverride(apply4, parentType.GetMethod("apply4"))
    tcProcType.DefineMethodOverride(apply5, parentType.GetMethod("apply5"))
    tcProcType.DefineMethodOverride(applyN, parentType.GetMethod("applyN"))

    let storeIndexAndFrameAndReturn (gen : Emit.ILGenerator) =
        gen.Emit(Emit.OpCodes.Ldarg_0)
        gen.Emit(Emit.OpCodes.Ldarg_1)
        gen.Emit(Emit.OpCodes.Stfld, frameField)
        gen.Emit(Emit.OpCodes.Ldarg_0)
        gen.Emit(Emit.OpCodes.Ldarg_2)
        gen.Emit(Emit.OpCodes.Stfld, indexField)
        gen.Emit(Emit.OpCodes.Ret)

    let overrideApplyMethod (mb : Emit.MethodBuilder) numArgs =
        let gen = mb.GetILGenerator()
        let procName = sprintf "funcall%i" numArgs
        let varargsCallLabel = gen.DefineLabel()

        gen.Emit(Emit.OpCodes.Ldarg_0)
        gen.Emit(Emit.OpCodes.Callvirt, parentType.GetProperty("isVarargs").GetGetMethod())
        gen.Emit(Emit.OpCodes.Brtrue, varargsCallLabel)

        gen.Emit(Emit.OpCodes.Ldarg_0)
        gen.Emit(Emit.OpCodes.Ldc_I4_S, (byte) numArgs)
        gen.Emit(Emit.OpCodes.Call, parentType.GetMethod("matchNumArgs"))

        gen.Emit(Emit.OpCodes.Ldarg_0)
        gen.Emit(Emit.OpCodes.Ldfld, frameField)
        gen.Emit(Emit.OpCodes.Ldarg_0)
        gen.Emit(Emit.OpCodes.Ldfld, indexField)

        for i in seq{1..numArgs} do
            gen.Emit(Emit.OpCodes.Ldarg_S, (byte) i)

        gen.Emit(Emit.OpCodes.Tailcall)
        gen.Emit(Emit.OpCodes.Callvirt, typeof<ProcedureFrame>.GetMethod(procName))
        gen.Emit(Emit.OpCodes.Ret)

        gen.MarkLabel(varargsCallLabel)
        gen.Emit(Emit.OpCodes.Ldarg_0)
        gen.Emit(Emit.OpCodes.Ldfld, frameField)
        gen.Emit(Emit.OpCodes.Ldarg_0)
        gen.Emit(Emit.OpCodes.Ldfld, indexField)
        gen.Emit(Emit.OpCodes.Ldc_I4, numArgs)
        gen.Emit(Emit.OpCodes.Newarr, typeof<CTObject>)

        for i in seq{1..numArgs} do
            gen.Emit(Emit.OpCodes.Dup)
            gen.Emit(Emit.OpCodes.Ldc_I4, (i - 1))
            gen.Emit(Emit.OpCodes.Ldarg_S, (byte) i)
            gen.Emit(Emit.OpCodes.Stelem_Ref)

        convertArrayOnStackToList gen

        gen.Emit(Emit.OpCodes.Tailcall)
        gen.Emit(Emit.OpCodes.Callvirt, typeof<ProcedureFrame>.GetMethod("funcallVarargs"))
        gen.Emit(Emit.OpCodes.Ret)

    let namedConstructorGen = namedConstructor.GetILGenerator()
    namedConstructorGen.Emit(Emit.OpCodes.Ldarg_0)
    namedConstructorGen.Emit(Emit.OpCodes.Ldarg_3)
    namedConstructorGen.Emit(Emit.OpCodes.Ldarg_S, (byte) 4)
    namedConstructorGen.Emit(Emit.OpCodes.Call, parentType.GetConstructor([| typeof<int>; typeof<string> |]))
    storeIndexAndFrameAndReturn namedConstructorGen

    let namedVarargsConstructorGen = namedVarargsConstructor.GetILGenerator()
    namedVarargsConstructorGen.Emit(Emit.OpCodes.Ldarg_0)
    namedVarargsConstructorGen.Emit(Emit.OpCodes.Ldarg_3)
    namedVarargsConstructorGen.Emit(Emit.OpCodes.Call, parentType.GetConstructor([| typeof<string> |]))
    storeIndexAndFrameAndReturn namedVarargsConstructorGen

    let anonymousConstructorGen = anonymousConstructor.GetILGenerator()
    anonymousConstructorGen.Emit(Emit.OpCodes.Ldarg_0)
    anonymousConstructorGen.Emit(Emit.OpCodes.Ldarg_3)
    anonymousConstructorGen.Emit(Emit.OpCodes.Call, parentType.GetConstructor([| typeof<int> |]))
    storeIndexAndFrameAndReturn anonymousConstructorGen

    let anonymousVarargsConstructorGen = anonymousVarargsConstructor.GetILGenerator()
    anonymousVarargsConstructorGen.Emit(Emit.OpCodes.Ldarg_0)
    anonymousVarargsConstructorGen.Emit(Emit.OpCodes.Call, parentType.GetConstructor([||]))
    storeIndexAndFrameAndReturn anonymousVarargsConstructorGen

    overrideApplyMethod apply0 0
    overrideApplyMethod apply1 1
    overrideApplyMethod apply2 2
    overrideApplyMethod apply3 3
    overrideApplyMethod apply4 4
    overrideApplyMethod apply5 5

    let applyNGen = applyN.GetILGenerator()
    applyNGen.Emit(Emit.OpCodes.Ldarg_0)
    applyNGen.Emit(Emit.OpCodes.Ldfld, frameField)
    applyNGen.Emit(Emit.OpCodes.Ldarg_0)
    applyNGen.Emit(Emit.OpCodes.Ldfld, indexField)
    applyNGen.Emit(Emit.OpCodes.Ldarg_1)
    convertArrayOnStackToList applyNGen
    applyNGen.Emit(Emit.OpCodes.Tailcall)
    applyNGen.Emit(Emit.OpCodes.Callvirt, typeof<ProcedureFrame>.GetMethod("funcallVarargs"))
    applyNGen.Emit(Emit.OpCodes.Ret)

    let fieldMap = Map.empty |> Map.add indexField.Name indexField
    let methodMap = [apply0; apply1; apply2; apply3; apply4; apply5; applyN]
                    |> List.map (fun f -> (f.Name, f))
                    |> Map.ofList

    { tb = tcProcType
      anonymousClosureConstructor = anonymousConstructor
      namedClosureConstructor = namedConstructor
      anonymousVarargsClosureConstructor = anonymousVarargsConstructor
      namedVarargsClosureConstructor = namedVarargsConstructor
      fields = fieldMap
      methods = methodMap }

let generateCodeFor (program : ProgramStructure) (name : string) =
    try
        let capitalizedName = SymbolGenerator.capitalizeWord name
        let outputFileName = sprintf "%s.exe" capitalizedName
        let assemblyBuilder = setupAssembly capitalizedName
        let moduleBuilder = assemblyBuilder.DefineDynamicModule(capitalizedName, outputFileName);

        let handleClassType = generateProcedureParentClass moduleBuilder
        handleClassType.tb.CreateType() |> ignore

        let mainClass = moduleBuilder.DefineType(capitalizedName, TypeAttributes.Public ||| TypeAttributes.Class, typeof<ProcedureFrame>)
        let mainMethod = mainClass.DefineMethod(mainMethodName,
                                                MethodAttributes.Public ||| MethodAttributes.Static,
                                                typeof<int>,
                                                [| typeof<string array> |])
        let topLevelTypes = { mainClass = mainClass
                              procedureHandleClass = handleClassType }

        builtIns := getBuiltIns program.scope
        generateMainModule topLevelTypes mainMethod program

        mainClass.CreateType() |> ignore

        // Save assembly and mark it as a console application
        assemblyBuilder.SetEntryPoint(mainMethod, Emit.PEFileKinds.ConsoleApplication)
        assemblyBuilder.Save(outputFileName)

        copyLibs()

        sprintf "Output written to file %s" outputFileName |> CodeGenSuccess
     with
        | CodeGenException msg -> CodeGenInternalError msg