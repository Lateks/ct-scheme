package backend

import javassist.{ClassPool, CtNewMethod}

import backend.ast._

object CodeGenerator {

  def generateCodeFor(program : ProgramSyntaxTree) : Unit = {
    val classPool = ClassPool.getDefault
    val mainClass = classPool.makeClass(program.programName)
    val mainMethod = CtNewMethod.make(
      "public static void main(String[] args) { System.out.println(\"Hello, world!\"); }",
      mainClass)
    mainClass.addMethod(mainMethod)
    mainClass.writeFile(".")
    // Defining an abstract method and giving it a body later:
    // CtMethod m = new CtMethod(CtClass.intType, "move",
    //  new CtClass[] { CtClass.intType }, cc);
    // cc.addMethod(m);
    // m.setBody("{ x += $1; }");
    // cc.setModifiers(cc.getModifiers() & ~Modifier.ABSTRACT);
  }

}
