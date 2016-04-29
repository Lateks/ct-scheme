package backend

import java.io.{PrintStream, PrintWriter}

import backend.ast._
import backend.codegen.{DebugClassWriter, SimpleMethodVisitor}
import org.objectweb.asm.Type
import org.objectweb.asm.Opcodes._

object CodeGenerator {

  def getInternalName(c : Class [_]): String = {
    Type.getType(c).getInternalName
  }

  def getDescriptor(c : Class [_]): String = {
    Type.getType(c).getDescriptor
  }

  def generateCodeFor(program : ProgramSyntaxTree) : Unit = {
    val pw = new PrintWriter(System.out)
    val mainClass = new DebugClassWriter(true, pw)
    mainClass.declareClass(program.programName, getInternalName(classOf[Object]))

    val mainMethodDescriptor = "([" + getDescriptor(classOf[String]) + ")" + Type.VOID_TYPE.getDescriptor
    val mainMethod = new SimpleMethodVisitor(mainClass, ACC_PUBLIC + ACC_STATIC, "main", mainMethodDescriptor)

    mainMethod.visitCode()
    mainMethod.emitGetStatic(getInternalName(classOf[System]), "out", getDescriptor(classOf[PrintStream]))
    mainMethod.visitLdcInsn("Hello, world!")
    mainMethod.emitInvokeVirtual(getInternalName(classOf[PrintStream]), "println", "(Ljava/lang/String;)V", false)
    mainMethod.emitReturn()
    mainMethod.visitEnd()

    mainMethod.setMaxs()
    mainClass.visitEnd()

    mainClass.writeToDisk()
  }

}
