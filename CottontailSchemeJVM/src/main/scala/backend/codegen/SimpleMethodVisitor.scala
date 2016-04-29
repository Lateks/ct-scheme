package backend.codegen

import org.objectweb.asm._
import org.objectweb.asm.Opcodes._

class SimpleMethodVisitor(cv : ClassVisitor, visibilityFlags : Int, name : String, signature : String) extends MethodVisitor(ASM5) {
  val methodVisitor = cv.visitMethod(visibilityFlags, name, signature, null, new Array[String](0))

  def emitGetStatic(className : String, fieldName : String, fieldType : String): Unit = {
    methodVisitor.visitFieldInsn(GETSTATIC, className, fieldName, fieldType)
  }

  def emitInvokeVirtual(ownerType : String, methodName : String, methodType : String, onInterface : Boolean): Unit = {
    methodVisitor.visitMethodInsn(INVOKEVIRTUAL, ownerType, methodName, methodType, onInterface)
  }

  def emitReturn(): Unit = {
    methodVisitor.visitInsn(RETURN)
  }

  def createAndDupObject(objectType : String): Unit = {
    methodVisitor.visitTypeInsn(NEW, objectType)
    methodVisitor.visitInsn(DUP)
  }

  def invokeConstructor(objectType : String, paramTypeDescriptor : String): Unit = {
    methodVisitor.visitMethodInsn(INVOKESPECIAL, objectType, "<init>", "(" + paramTypeDescriptor + ")V", false)
  }

  def loadConstant(obj : AnyRef): Unit = {
    methodVisitor.visitLdcInsn(obj)
  }

  def setMaxs(): Unit = {
    methodVisitor.visitMaxs(0, 0)
  }

  override def visitMaxs(maxStack : Int, maxLocals : Int): Unit = {
    methodVisitor.visitMaxs(maxStack, maxLocals)
  }

  override def visitInsn(opCode : Int): Unit = {
    methodVisitor.visitInsn(opCode)
  }

  override def visitIntInsn(opCode : Int, arg : Int): Unit = {
    methodVisitor.visitIntInsn(opCode, arg)
  }

  override def visitVarInsn(opCode : Int, arg : Int): Unit = {
    methodVisitor.visitVarInsn(opCode, arg)
  }

  override def visitTypeInsn(opCode : Int, arg : String): Unit = {
    methodVisitor.visitTypeInsn(opCode, arg)
  }

  override def visitLdcInsn(obj : AnyRef): Unit = {
    methodVisitor.visitLdcInsn(obj)
  }

  override def visitEnd(): Unit = {
    methodVisitor.visitEnd()
  }

  override def visitMethodInsn(opCode : Int, owner : String, methodName : String, methodDescriptor : String, onInterface : Boolean): Unit = {
    methodVisitor.visitMethodInsn(opCode, owner, methodName, methodDescriptor, onInterface)
  }

  override def visitFieldInsn(opCode : Int, owner : String, fieldName : String, fieldDescriptor : String): Unit = {
    methodVisitor.visitFieldInsn(opCode, owner, fieldName, fieldDescriptor)
  }

  override def visitAttribute(attr : Attribute): Unit = {
    methodVisitor.visitAttribute(attr)
  }

  override def visitCode() : Unit = {
    methodVisitor.visitCode()
  }

  override def visitInvokeDynamicInsn(name : String, desc : String, bsm : Handle, bsmArgs : Object*): Unit = {
    methodVisitor.visitInvokeDynamicInsn(name, desc, bsm, bsmArgs:_*)
  }

  override def visitJumpInsn(opCode : Int, label : Label): Unit = {
    methodVisitor.visitJumpInsn(opCode, label)
  }

  override def visitLabel(l : Label): Unit = {
    methodVisitor.visitLabel(l)
  }

  override def visitIincInsn(opCode : Int, increment : Int): Unit = {
    methodVisitor.visitIincInsn(opCode, increment)
  }

  override def visitTableSwitchInsn(min : Int, max : Int, default : Label, labels : Label*): Unit = {
    methodVisitor.visitTableSwitchInsn(min, max, default, labels:_*)
  }

  override def visitLookupSwitchInsn(default : Label, keys : Array[Int], labels : Array[Label]): Unit = {
    methodVisitor.visitLookupSwitchInsn(default, keys, labels)
  }

  override def visitMultiANewArrayInsn(desc : String, dims : Int): Unit = {
    methodVisitor.visitMultiANewArrayInsn(desc, dims)
  }

  override def visitInsnAnnotation(typeRef : Int, typePath : TypePath, desc : String, visible : Boolean): AnnotationVisitor = {
    methodVisitor.visitInsnAnnotation(typeRef, typePath, desc, visible)
  }

  override def visitTryCatchBlock(start : Label, end : Label, handler : Label, exceptionType : String): Unit = {
    methodVisitor.visitTryCatchBlock(start, end, handler, exceptionType)
  }

  override def visitTryCatchAnnotation(typeRef : Int, typePath : TypePath, desc : String, visible : Boolean): AnnotationVisitor = {
    methodVisitor.visitTryCatchAnnotation(typeRef, typePath, desc, visible)
  }

  override def visitLocalVariable(name : String, desc : String, signature : String, start : Label, end : Label, index : Int) = {
    methodVisitor.visitLocalVariable(name, desc, signature, start, end, index)
  }

  override def visitLocalVariableAnnotation(typeRef : Int, typePath : TypePath, start : Array[Label], end : Array[Label], index : Array[Int], desc : String, visible : Boolean): AnnotationVisitor = {
    methodVisitor.visitLocalVariableAnnotation(typeRef, typePath, start, end, index, desc, visible)
  }

  override def visitLineNumber(line : Int, start : Label): Unit = {
    methodVisitor.visitLineNumber(line, start)
  }

  override def visitFrame(typeN : Int, nLocal : Int, local : Array[Object], nStack : Int, stack : Array[Object]): Unit = {
    methodVisitor.visitFrame(typeN, nLocal, local, nStack, stack)
  }
}
