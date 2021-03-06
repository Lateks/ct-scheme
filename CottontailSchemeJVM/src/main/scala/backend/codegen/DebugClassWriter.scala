package backend.codegen

import java.io.{FileOutputStream, PrintWriter}

import org.objectweb.asm.util.{CheckClassAdapter, TraceClassVisitor}
import org.objectweb.asm.{AnnotationVisitor, MethodVisitor, _}
import org.objectweb.asm.Opcodes._

class DebugClassWriter(debug : Boolean, pw : PrintWriter) extends ClassVisitor(ASM5) {
  val cw = new ClassWriter(ClassWriter.COMPUTE_FRAMES)
  val classVisitor = if (debug) {
    val checkDataFlow = false
    new CheckClassAdapter(new TraceClassVisitor(cw, pw), checkDataFlow)
  } else {
    cw
  }

  private var className: Option[String] = None

  def getName: String = {
    className match {
      case Some(x) => x
      case None => null
    }
  }

  def declareClass(className : String, parentClass : String): Unit = {
    this.className = Some(className)
    classVisitor.visit(V1_8, ACC_PUBLIC, className, null, parentClass, new Array[String](0))
  }

  def toByteArray: Array[Byte] = {
    cw.toByteArray
  }

  def writeToDisk(): Unit = {
    className match {
      case None =>
        throw new CodeGenException("Trying to write class to disk before class has been declared.")
      case Some(n) =>
        val bytes = toByteArray
        new FileOutputStream(n + ".class").write(bytes)
    }
  }

  override def visitAttribute(attribute: Attribute): Unit = classVisitor.visitAttribute(attribute)

  override def visitTypeAnnotation(i: Int, typePath: TypePath, s: String, b: Boolean): AnnotationVisitor = classVisitor.visitTypeAnnotation(i, typePath, s, b)

  override def visitField(i: Int, s: String, s1: String, s2: String, o: scala.Any): FieldVisitor = classVisitor.visitField(i, s, s1, s2, o)

  override def visit(i: Int, i1: Int, s: String, s1: String, s2: String, strings: Array[String]): Unit = classVisitor.visit(i, i1, s, s1, s2, strings)

  override def visitEnd(): Unit = classVisitor.visitEnd()

  override def visitSource(s: String, s1: String): Unit = classVisitor.visitSource(s, s1)

  override def visitMethod(i: Int, s: String, s1: String, s2: String, strings: Array[String]): MethodVisitor = classVisitor.visitMethod(i, s, s1, s2, strings)

  override def visitAnnotation(s: String, b: Boolean): AnnotationVisitor = classVisitor.visitAnnotation(s, b)

  override def visitOuterClass(s: String, s1: String, s2: String): Unit = classVisitor.visitOuterClass(s, s1, s2)

  override def visitInnerClass(s: String, s1: String, s2: String, i: Int): Unit = classVisitor.visitInnerClass(s, s1, s2, i)
}
