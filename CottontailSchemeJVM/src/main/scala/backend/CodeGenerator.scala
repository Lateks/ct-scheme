package backend

import java.io.{DataOutputStream, FileOutputStream}
import javassist.bytecode.{AccessFlag, Bytecode, ClassFile, MethodInfo}

import backend.ast._

object CodeGenerator {

  def generateCodeFor(program : ProgramSyntaxTree) : Unit = {
    // Using the higher level API:
    //val classPool = ClassPool.getDefault
    //val mainClass = classPool.makeClass(program.programName)
    //val mainMethod = CtNewMethod.make(
    //  "public static void main(String[] args) { System.out.println(\"Hello, world!\"); }",
    //  mainClass)
    //mainClass.addMethod(mainMethod)
    //mainClass.writeFile(".")

    // Defining an abstract method and giving it a body later:
    // CtMethod m = new CtMethod(CtClass.intType, "move",
    //  new CtClass[] { CtClass.intType }, cc);
    // cc.addMethod(m);
    // m.setBody("{ x += $1; }");
    // cc.setModifiers(cc.getModifiers() & ~Modifier.ABSTRACT);

    val mainClass = new ClassFile(false, program.programName, "java/lang/Object")
    mainClass.setAccessFlags(AccessFlag.PUBLIC | AccessFlag.SUPER)

    // Add main method
    val mainMethod = new MethodInfo(mainClass.getConstPool, "main", "([Ljava/lang/String;)V")
    mainMethod.setAccessFlags(AccessFlag.PUBLIC | AccessFlag.STATIC)

    val mainMethodBody = new Bytecode(mainClass.getConstPool)
    mainMethodBody.addGetstatic("java/lang/System", "out", "Ljava/io/PrintStream;")
    mainMethodBody.addLdc("Hello, world!")
    mainMethodBody.addInvokevirtual("java/io/PrintStream", "println", "(Ljava/lang/Object;)V")
    mainMethodBody.addReturn(null)
    mainMethodBody.setMaxLocals(1)

    mainMethod.setCodeAttribute(mainMethodBody.toCodeAttribute)

    mainClass.addMethod(mainMethod)

    // Add constructor
    val initializer = new MethodInfo(mainClass.getConstPool, MethodInfo.nameInit, "()V")
    initializer.setAccessFlags(AccessFlag.PUBLIC)

    val initializerBody = new Bytecode(mainClass.getConstPool)
    initializerBody.addAload(0)
    initializerBody.addInvokespecial("java/lang/Object", MethodInfo.nameInit, "()V")
    initializerBody.addReturn(null)
    initializerBody.setMaxLocals(1)

    initializer.setCodeAttribute(initializerBody.toCodeAttribute)

    mainClass.addMethod(initializer)

    mainClass.write(new DataOutputStream(new FileOutputStream(program.programName + ".class")))
  }

}
