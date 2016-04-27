package backend

import java.io.FileOutputStream

import backend.ast._
import backend.codegen.SimpleMethodVisitor
import org.objectweb.asm.ClassWriter
import org.objectweb.asm.Opcodes._

object CodeGenerator {

  def generateCodeFor(program : ProgramSyntaxTree) : Unit = {
    val mainClass = new ClassWriter(0)
    mainClass.visit(V1_8, ACC_PUBLIC, program.programName, null, "java/lang/Object", new Array[String](0))
    val mainMethod = new SimpleMethodVisitor(mainClass, ACC_PUBLIC + ACC_STATIC, "main", "([Ljava/lang/String;)V")
    mainMethod.emitGetStatic("java/lang/System", "out", "Ljava/io/PrintStream;")
    mainMethod.visitLdcInsn("Hello, world!")
    mainMethod.emitInvokeVirtual("java/io/PrintStream", "println", "(Ljava/lang/String;)V", false)
    mainMethod.emitReturn()
    mainMethod.visitEnd()

    val maxStack = 2
    val maxLocals = 1
    mainMethod.visitMaxs(maxStack, maxLocals)
    mainClass.visitEnd()

    val bytes = mainClass.toByteArray
    new FileOutputStream(program.programName + ".class").write(bytes)
  }

}
