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
              | ObjectField of Emit.FieldBuilder * Variable

type ClosureLoadInfo = { lambdaBuilder : Emit.MethodBuilder;
                         localFrameField : Emit.LocalBuilder option }

type Scope = { variables : Map<string, Variable>;
               procedures : Map<string, Procedure>;
               closureInfo : Map<string, ClosureLoadInfo> }

// TODO: procedures?
type Frame = { frameClass : Emit.TypeBuilder;
               frameFields: (string * Emit.FieldBuilder) list;
               staticLink: Emit.FieldBuilder option;
               parent: Frame option }

type FrameInfo = { closureInfo : Map<string, ClosureLoadInfo>;
                   frameVariable : Emit.LocalBuilder option;
                   matchedCapturesFromCurrentScope : (Identifier * Variable) list }

type ClosureTypeInformation = { tb : Emit.TypeBuilder
                                anonymousClosureConstructor : Emit.ConstructorBuilder
                                namedClosureConstructor : Emit.ConstructorBuilder
                                anonymousVarargsClosureConstructor : Emit.ConstructorBuilder
                                namedVarargsClosureConstructor : Emit.ConstructorBuilder
                                fields : Map<string, Emit.FieldBuilder>
                                methods : Map<string, Emit.MethodBuilder> }

type TopLevelTypes = { mainClass : ClosureTypeInformation
                       frameClass : ClosureTypeInformation }

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
                              | ObjectField _ -> false
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

let rec generateSubExpression (methodBuilder : Emit.MethodBuilder) (scope: Scope) (pushOnStack : bool) (emitReturn : bool) (expr : Expression) =
    let recur push expr = generateSubExpression methodBuilder scope push false expr
    let pushExprResultToStack = recur true
    let gen = methodBuilder.GetILGenerator()

    let emitConditional condition thenExpression elseExpression =
        let elseLabel = gen.DefineLabel()
        let exitLabel = gen.DefineLabel()

        pushExprResultToStack condition
        emitBooleanConversion gen
        gen.Emit(Emit.OpCodes.Brfalse, elseLabel)

        pushExprResultToStack thenExpression

        if emitReturn then
            gen.Emit(Emit.OpCodes.Ret)
        else
            gen.Emit(Emit.OpCodes.Br_S, exitLabel)

        gen.MarkLabel(elseLabel)
        pushExprResultToStack elseExpression

        if emitReturn then
            gen.Emit(Emit.OpCodes.Ret)
        else
            gen.MarkLabel(exitLabel)
            if not pushOnStack then popStack gen

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

            pushExprResultToStack tailExpr
            if emitReturn then // return with tail expression value
                gen.Emit(Emit.OpCodes.Ret)
            elif pushOnStack then
                storeTempValue ()
            else
                popStack gen

            gen.MarkLabel(exitLabel)
            loadTempValue ()
            if emitReturn then // return with stored value
                gen.Emit(Emit.OpCodes.Ret)

    let emitVariableLoad id =
        emitVariableLoad gen scope id methodBuilder.IsStatic

    let emitAssignment (id : Identifier) expr =
        let push () = pushExprResultToStack expr

        printfn "Assigning to %A, scope contains variables %A" id scope.variables
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
            printfn "Falling back to argIndex"
            match id.argIndex with
            | Some n ->
                push ()
                let index = if methodBuilder.IsStatic then n else n + 1
                gen.Emit(Emit.OpCodes.Starg_S, (byte) index)
            | None -> failwith "Attempting to set! unknown variable %A" id

    let pushArgsAsArray args = emitArray gen args (generateSubExpression methodBuilder scope true false)
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
        -> printfn "Looking for closure info for %s, scope contains %A" c.functionName.uniqueName (mapKeys scope.closureInfo)
           let closureInfo = scope.closureInfo.Item(c.functionName.uniqueName)
           match closureInfo.localFrameField with
           | Some f -> gen.Emit(Emit.OpCodes.Ldloc, f)
           | None -> gen.Emit(Emit.OpCodes.Ldnull)

           gen.Emit(Emit.OpCodes.Ldftn, closureInfo.lambdaBuilder)
           createProcedureObjectOnStack gen c true
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

let generateExpression (mb : Emit.MethodBuilder) (expr : Expression) (scope : Scope) emitReturn =
    generateSubExpression mb scope false emitReturn expr

let defineFrame (mainClass : Emit.TypeBuilder) (parentClass : Emit.TypeBuilder) (capturesFromCurrentScope : Identifier list) (captureParentFrame : bool) parentProcedureName =
    let frameClassName = sprintf "%s$frame" parentProcedureName
    let frameClass = mainClass.DefineNestedType(frameClassName, TypeAttributes.NestedAssembly)

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

    for (id, InstanceField f) in captures do
        gen.Emit(Emit.OpCodes.Dup)
        emitVariableLoad gen scope id isStaticContext
        gen.Emit(Emit.OpCodes.Stfld, f)

let rebindFrameFields frameVar scope captures =
    match frameVar with
    | None -> scope
    | Some v ->
        let frameVariable = LocalVar v

        // Binds captured local variable names and argument names
        // to fields on the frame variable
        let rec rebindName name scope fb =
            if scope.variables.ContainsKey name then
                match scope.variables.Item(name) with
                | InstanceField _ | ObjectField _ -> scope.variables
                | Field _ | LocalVar _ as f -> failwithf "Unexpected field found in captures (%s): %A" name f
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
            | (_, f)::_ -> failwithf "Error, expected an instance field but got %A" f

        rebind scope captures

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

let rec generateProcedureBody (mainClass : Emit.TypeBuilder) (parentFrame : Frame option) (mb : Emit.MethodBuilder) (c : ClosureDefinition) scope =
    let gen = mb.GetILGenerator()
    let closures = let namedProcedures = c.procedureDefinitions
                                         |> List.map (fun (ProcedureDefinition (_, def)) -> def)
                   let anonymousProcedures = findClosuresInScope c.body
                   List.append namedProcedures anonymousProcedures

    // Debug printing, TODO: remove
    printfn "Closures in procedure body of %A: %A" c.functionName.uniqueName (List.map (fun c -> c.functionName.uniqueName) closures)

    // Create frame classes and closure body methods.
    let frameInfo = generateClosures mainClass mb scope (Some c) parentFrame closures
    let scopeWithFrameReferences = rebindFrameFields frameInfo.frameVariable scope frameInfo.matchedCapturesFromCurrentScope

    // Find local variables that were not captured in the frame and
    // add them to the local scope.
    let capturedVarIds = frameInfo.matchedCapturesFromCurrentScope |> List.map (fun (id, _) -> id)
    let newLocals = getVariableIdsInScope c
                    |> List.filter (fun id -> not <| List.contains id capturedVarIds)
                    |> List.map (fun id -> (id.uniqueName, LocalVar <| gen.DeclareLocal(typeof<CTObject>)))

    // TODO: do some local names need to be removed from scope later?
    let scopeWithLocalFieldsAndClosures =
        { scopeWithFrameReferences with
            variables = Map.toSeq scopeWithFrameReferences.variables
                        |> Seq.append newLocals
                        |> Map.ofSeq
            closureInfo = frameInfo.closureInfo }

    // Debug printing, TODO: remove
    printfn "Extended scope for %s with closures contains variables %A" c.functionName.uniqueName scopeWithLocalFieldsAndClosures.variables
    printfn "Generating body for closure %A" c

    // Populate (assign) the local variables that hold closures.
    for ProcedureDefinition (id, proc) in c.procedureDefinitions do
        generateExpression mb (Assignment (id, Closure proc)) scopeWithLocalFieldsAndClosures false

    // Generate procedure body.
    let body = List.take (c.body.Length - 1) c.body
    let tailExpr = List.last c.body

    for expr in body do
        generateExpression mb expr scopeWithLocalFieldsAndClosures false

    generateExpression mb tailExpr scopeWithLocalFieldsAndClosures true

// mainClass: Top level class in module, containing the main method.
//            Used for defining nested frame classes.
// parentClass: Parent class of current method (mb).
//              Used to house non-capturing lambda bodies as methods.
and generateClosures (mainClass : Emit.TypeBuilder) (mb : Emit.MethodBuilder) (scope : Scope) (parentProcedure : ClosureDefinition option) (currentFrame : Frame option) closures =
    let gen = mb.GetILGenerator()
    let localVars, args, parentProcedureName =
        match parentProcedure with
        | Some p -> getVariableIdsInScope p, getFormals p, p.functionName.uniqueName
        | None -> [], [], "Main"
    let parentClass = match currentFrame with
                      | None -> mainClass
                      | Some f -> f.frameClass
    let makeBuilders lambdaParentClass isFrameClass =
        List.map (fun nestedClosure -> (nestedClosure.functionName.uniqueName,
                                        defineProcedure lambdaParentClass nestedClosure isFrameClass,
                                        nestedClosure))

    let generateBodies builders frame scope =
        for (name, lb, nestedClosure) in builders do
            generateProcedureBody mainClass frame lb nestedClosure scope

    let buildClosureInfoMap localFrameVar =
        List.map (fun (name, lb, _) -> (name, { lambdaBuilder = lb; localFrameField = localFrameVar }))
        >> Map.ofList

    let isArgument id = List.contains id args

    let capturesFromCurrentScope captures =
        let isLocal id = List.contains id localVars
        captures |> List.filter (fun id -> isArgument id || isLocal id)

    let createFrame captures =
        let currentScopeCaptures = capturesFromCurrentScope captures
        let captureParentFrame = List.length captures <> List.length currentScopeCaptures
        let frameClass, cons, fields, capturedFrameField = defineFrame mainClass parentClass currentScopeCaptures captureParentFrame parentProcedureName

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

        // TODO: add locally visible procedures to lambda scope before generating bodies
        let newFrame = { parent = currentFrame;
                         frameFields = fields |> List.map (fun (n, InstanceField f) -> (n, f));
                         staticLink = capturedFrameField;
                         frameClass = frameClass; }

        newFrame, matchedCapturesInCurrentScope, frameVar

    let extendScopeWithFrameFields fields scope =
        // TODO: add local procedures to scope?
        let extendedVars =
            fields
            |> List.fold (fun map (fieldName, f) ->
                              map |> Map.remove fieldName
                                  |> Map.add fieldName (InstanceField f))
                         scope.variables
        { scope with variables = extendedVars }

    let createIndirectReferenceTo staticLink name =
        let fail () = failwithf "Cannot build an indirect reference to field %s" name
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

        let referenceChain = staticLink :: (buildIndirectReferenceChain currentFrame)
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

    if List.isEmpty closures then
        { closureInfo = Map.empty;
          frameVariable = None;
          matchedCapturesFromCurrentScope = [] }
    else
        let captures = closures
                       |> List.map (fun clos -> clos.environment)
                       |> List.concat
                       |> List.distinct
        if captures.IsEmpty then
            let isInsideFrameClass = currentFrame.IsSome
            let builders = makeBuilders parentClass isInsideFrameClass closures

            // TODO: add locally visible procedures to lambda scope before generating bodies
            //       to allow procedures to call each other?
            // TODO: scope passed in should be the "lambda scope" of the previous level
            generateBodies builders currentFrame scope

            { closureInfo = buildClosureInfoMap None builders;
              frameVariable = None;
              matchedCapturesFromCurrentScope = [] }
        else
            // Debug printing, TODO: remove
            printfn "Generating closures inside %s, capture list is %A" parentProcedureName (List.map (fun id -> id.uniqueName) captures)

            let newFrame, matchedCapturesInCurrentScope, frameVar = createFrame captures

            let isNonLocalCapture = let captureIds = matchedCapturesInCurrentScope |> List.map (fun (id, _) -> id)
                                    fun id -> captureIds |> List.contains id |> not
            let nonLocalCaptures = captures |> List.filter isNonLocalCapture
            let lambdaScope = scope |> extendScopeWithFrameFields newFrame.frameFields
                                    |> extendScopeWithIndirectlyAccessibleFields nonLocalCaptures newFrame.staticLink

            let builders = makeBuilders newFrame.frameClass true closures
            generateBodies builders (Some newFrame) lambdaScope

            newFrame.frameClass.CreateType() |> ignore

            { closureInfo = buildClosureInfoMap (Some frameVar) builders;
              frameVariable = Some frameVar;
              matchedCapturesFromCurrentScope = matchedCapturesInCurrentScope }

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
        generateProcedureBody mainClass None proc.methodBuilder proc.closure scope

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
    let frameInfo = generateClosures mainClass mainMethod scope None None closures
    let scopeWithClosures = { scope with closureInfo = frameInfo.closureInfo }

    for expr in program.expressions do
        generateExpression mainMethod expr scopeWithClosures false

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

let generateProcedureParentClass (moduleBuilder : Emit.ModuleBuilder) =
    let parentType = typeof<CTProcedure>

    // Type
    let tcProcType = moduleBuilder.DefineType("CTTailCallProcedure", TypeAttributes.Public ||| TypeAttributes.Class, parentType)

    // Fields
    let indexField = tcProcType.DefineField("index", typeof<int>, FieldAttributes.Assembly)

    // Constructors
    let namedConstructor = tcProcType.DefineConstructor(MethodAttributes.Public, CallingConventions.Standard, [| typeof<int>; typeof<int>; typeof<string> |])
    let namedVarargsConstructor = tcProcType.DefineConstructor(MethodAttributes.Public, CallingConventions.Standard, [| typeof<int>; typeof<string>|])
    let anonymousConstructor = tcProcType.DefineConstructor(MethodAttributes.Public, CallingConventions.Standard, [| typeof<int>; typeof<int>; |])
    let anonymousVarargsConstructor = tcProcType.DefineConstructor(MethodAttributes.Public, CallingConventions.Standard, [| typeof<int>; |])

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

    let storeIndexAndReturn (gen : Emit.ILGenerator) =
        gen.Emit(Emit.OpCodes.Ldarg_0)
        gen.Emit(Emit.OpCodes.Ldarg_1)
        gen.Emit(Emit.OpCodes.Stfld, indexField)
        gen.Emit(Emit.OpCodes.Ret)

    let overrideApplyMethod (mb : Emit.MethodBuilder) numArgs =
        let gen = mb.GetILGenerator()
        let procName = sprintf "funcall%i" numArgs
        let elseLabel = gen.DefineLabel()
        gen.Emit(Emit.OpCodes.Ldarg_0)
        gen.Emit(Emit.OpCodes.Callvirt, parentType.GetProperty("isVarargs").GetGetMethod())
        gen.Emit(Emit.OpCodes.Brtrue, elseLabel)
        gen.Emit(Emit.OpCodes.Ldarg_0)

        for i in seq{1..numArgs} do
            gen.Emit(Emit.OpCodes.Ldarg_S, (byte) i)

        gen.Emit(Emit.OpCodes.Tailcall)
        gen.Emit(Emit.OpCodes.Callvirt, parentType.GetMethod(procName))
        gen.Emit(Emit.OpCodes.Ret)

        gen.MarkLabel(elseLabel)
        gen.Emit(Emit.OpCodes.Ldarg_0)
        gen.Emit(Emit.OpCodes.Ldc_I4, numArgs)
        gen.Emit(Emit.OpCodes.Newarr, typeof<CTObject>)

        for i in seq{1..numArgs} do
            gen.Emit(Emit.OpCodes.Dup)
            gen.Emit(Emit.OpCodes.Ldc_I4, (i - 1))
            gen.Emit(Emit.OpCodes.Ldarg_S, (byte) i)
            gen.Emit(Emit.OpCodes.Stelem_Ref)

        gen.Emit(Emit.OpCodes.Tailcall)
        gen.Emit(Emit.OpCodes.Callvirt, parentType.GetMethod("funcallVarargs"))
        gen.Emit(Emit.OpCodes.Ret)

    let namedConstructorGen = namedConstructor.GetILGenerator()
    namedConstructorGen.Emit(Emit.OpCodes.Ldarg_0)
    namedConstructorGen.Emit(Emit.OpCodes.Ldarg_2)
    namedConstructorGen.Emit(Emit.OpCodes.Ldarg_3)
    namedConstructorGen.Emit(Emit.OpCodes.Call, parentType.GetConstructor([| typeof<int>; typeof<string> |]))
    storeIndexAndReturn namedConstructorGen

    let namedVarargsConstructorGen = namedVarargsConstructor.GetILGenerator()
    namedVarargsConstructorGen.Emit(Emit.OpCodes.Ldarg_0)
    namedVarargsConstructorGen.Emit(Emit.OpCodes.Ldarg_2)
    namedVarargsConstructorGen.Emit(Emit.OpCodes.Call, parentType.GetConstructor([| typeof<string> |]))
    storeIndexAndReturn namedVarargsConstructorGen

    let anonymousConstructorGen = anonymousConstructor.GetILGenerator()
    anonymousConstructorGen.Emit(Emit.OpCodes.Ldarg_0)
    anonymousConstructorGen.Emit(Emit.OpCodes.Ldarg_2)
    anonymousConstructorGen.Emit(Emit.OpCodes.Call, parentType.GetConstructor([| typeof<int> |]))
    storeIndexAndReturn anonymousConstructorGen

    let anonymousVarargsConstructorGen = anonymousVarargsConstructor.GetILGenerator()
    anonymousVarargsConstructorGen.Emit(Emit.OpCodes.Ldarg_0)
    anonymousVarargsConstructorGen.Emit(Emit.OpCodes.Call, parentType.GetConstructor([||]))
    storeIndexAndReturn anonymousVarargsConstructorGen

    overrideApplyMethod apply0 0
    overrideApplyMethod apply1 1
    overrideApplyMethod apply2 2
    overrideApplyMethod apply3 3
    overrideApplyMethod apply4 4
    overrideApplyMethod apply5 5

    let applyNGen = applyN.GetILGenerator()
    applyNGen.Emit(Emit.OpCodes.Ldarg_0)
    applyNGen.Emit(Emit.OpCodes.Ldarg_1)
    applyNGen.Emit(Emit.OpCodes.Tailcall)
    applyNGen.Emit(Emit.OpCodes.Callvirt, parentType.GetMethod("funcallVarargs"))
    applyNGen.Emit(Emit.OpCodes.Ret)

    tcProcType.CreateType() |> ignore

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
    let capitalizedName = SymbolGenerator.capitalizeWord name
    let outputFileName = sprintf "%s.exe" capitalizedName
    let assemblyBuilder = setupAssembly capitalizedName
    let moduleBuilder = assemblyBuilder.DefineDynamicModule(capitalizedName, outputFileName);

    let procParentType = generateProcedureParentClass moduleBuilder

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
