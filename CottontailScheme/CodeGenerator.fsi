﻿module CottontailScheme.CodeGenerator

type CodeGenResult = CodeGenInternalError of string
                   | CodeGenSuccess of string

val generateCodeFor : CottontailScheme.SemanticAnalysis.ProgramStructure -> CodeGenResult