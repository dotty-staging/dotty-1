package dotty.tools.backend.jvm

import scala.language.unsafeNulls

import org.junit.Assert._
import org.junit.Test

import scala.tools.asm
import asm._
import asm.tree._

import scala.tools.asm.Opcodes
import scala.jdk.CollectionConverters._
import Opcodes._

class LabelBytecodeTests extends DottyBytecodeTest {
  import ASMConverters._

   @Test def labelBreak = {
    testLabelBytecode(
      """val local = boundary.Label[Long]()
        |try throw boundary.Break[Long](local, 5L)
        |catch case ex: boundary.Break[Long] @unchecked =>
        |  if ex.label eq local then ex.value
        |  else throw ex
      """.stripMargin,
      "Long",
      Ldc(LDC, 5),
      Op(LRETURN)
    )
  }

  @Test def simpleBoundaryBreak = {
    testLabelBytecode(
      """boundary: l ?=>
        |  l.break(2)
      """.stripMargin,
      "Int",
      Op(ICONST_2),
      Op(IRETURN)
    )

    testLabelBytecode(
      """boundary:
        |  break(3)
      """.stripMargin,
      "Int",
      Op(ICONST_3),
      Op(IRETURN)
    )

    testLabelBytecode(
      """boundary:
        |  break()
      """.stripMargin,
      "Unit",
      Field(GETSTATIC, "scala/runtime/BoxedUnit", "UNIT", "Lscala/runtime/BoxedUnit;"),
      VarOp(ASTORE, 1),
      Op(RETURN)
    )
  }

  @Test def labelExtraction = {
    // Test extra Inlined around the label
    testLabelBytecode(
      """boundary:
        |  summon[boundary.Label[Int]].break(2)
      """.stripMargin,
      "Int",
      Op(ICONST_2),
      Op(IRETURN)
    )

    // Test extra Block around the label
    testLabelBytecode(
      """boundary: l ?=>
        |  { l }.break(2)
      """.stripMargin,
      "Int",
      Op(ICONST_2),
      Op(IRETURN)
    )
  }

  @Test def boundaryLocalBreak = {
    testLabelBytecode(
      """val x: Boolean = true
        |boundary[Unit, Unit]:
        |  var i = 0
        |  while true do
        |    i += 1
        |    if i > 10 then break()
      """.stripMargin,
      "Unit",
      Op(ICONST_1),
      VarOp(ISTORE, 1),
      Op(ICONST_0),
      VarOp(ISTORE, 2),
      Label(4),
      FrameEntry(1, List(1, 1), List()),
      Op(ICONST_1),
      Jump(IFEQ, Label(18)),
      Incr(IINC, 2, 1),
      VarOp(ILOAD, 2),
      IntOp(BIPUSH, 10),
      Jump(IF_ICMPLE, Label(15)),
      Field(GETSTATIC, "scala/runtime/BoxedUnit", "UNIT", "Lscala/runtime/BoxedUnit;"),
      VarOp(ASTORE, 3),
      Op(RETURN), // break()
      Label(15),
      FrameEntry(3, List(), List()),
      Jump(GOTO, Label(4)),
      Label(18),
      FrameEntry(3, List(), List()),
      Op(RETURN)
    )
  }

  @Test def boundaryNonLocalBreak = {
    testLabelBytecode(
      """boundary[Unit, Unit]:
        |  nonLocalBreak()
      """.stripMargin,
      "Unit",
      TypeOp(NEW, "scala/util/boundary$Label"),
      Op(DUP),
      Invoke(INVOKESPECIAL, "scala/util/boundary$Label", "<init>", "()V", false),
      VarOp(ASTORE, 1),
      Label(4),
      VarOp(ALOAD, 0),
      VarOp(ALOAD, 1),
      Invoke(INVOKEVIRTUAL, "Test", "nonLocalBreak", "(Lscala/util/boundary$Label;)Lscala/runtime/Nothing$;", false),
      Op(ATHROW),
      Label(9),
      FrameEntry(0, List(), List("java/lang/Throwable")),
      Op(NOP),
      Op(NOP),
      Op(ATHROW),
      Label(14),
      FrameEntry(0, List("Test", "scala/util/boundary$Label"), List("scala/util/boundary$Break")),
      VarOp(ASTORE, 2),
      VarOp(ALOAD, 2),
      Invoke(INVOKEVIRTUAL, "scala/util/boundary$Break", "label", "()Lscala/util/boundary$Label;", false),
      VarOp(ALOAD, 1),
      Jump(IF_ACMPNE, Label(22)),
      Jump(GOTO, Label(26)),
      Label(22),
      FrameEntry(1, List("scala/util/boundary$Break"), List()),
      VarOp(ALOAD, 2),
      Op(ATHROW),
      Label(26),
      FrameEntry(3, List(), List()),
      Jump(GOTO, Label(29)),
      Label(29),
      FrameEntry(3, List(), List()),
      Op(RETURN),
    )

    testLabelBytecode(
      """boundary[Unit, Unit]:
        |  def f() = break()
        |  f()
      """.stripMargin,
      "Unit",
      TypeOp(NEW, "scala/util/boundary$Label"),
      Op(DUP),
      Invoke(INVOKESPECIAL, "scala/util/boundary$Label", "<init>", "()V", false),
      VarOp(ASTORE, 1),
      Label(4),
      VarOp(ALOAD, 1),
      Invoke(INVOKESTATIC, "Test", "f$1", "(Lscala/util/boundary$Label;)Lscala/runtime/Nothing$;", false),
      Op(ATHROW),
      Label(8),
      FrameEntry(0, List(), List("java/lang/Throwable")),
      Op(NOP),
      Op(NOP),
      Op(ATHROW),
      Label(13),
      FrameEntry(0, List("Test", "scala/util/boundary$Label"), List("scala/util/boundary$Break")),
      VarOp(ASTORE, 2),
      VarOp(ALOAD, 2),
      Invoke(INVOKEVIRTUAL, "scala/util/boundary$Break", "label", "()Lscala/util/boundary$Label;", false),
      VarOp(ALOAD, 1),
      Jump(IF_ACMPNE, Label(21)),
      Jump(GOTO, Label(25)),
      Label(21),
      FrameEntry(1, List("scala/util/boundary$Break"), List()),
      VarOp(ALOAD, 2),
      Op(ATHROW),
      Label(25),
      FrameEntry(3, List(), List()),
      Jump(GOTO, Label(28)),
      Label(28),
      FrameEntry(3, List(), List()),
      Op(RETURN),
    )
  }

  @Test def boundaryLocalAndNonLocalBreak = {
    testLabelBytecode(
      """boundary[Unit, Unit]: l ?=>
        |  break()
        |  nonLocalBreak()
      """.stripMargin,
      "Unit",
      TypeOp(NEW, "scala/util/boundary$Label"),
      Op(DUP),
      Invoke(INVOKESPECIAL, "scala/util/boundary$Label", "<init>", "()V", false),
      VarOp(ASTORE, 1),
      Label(4),
      Field(GETSTATIC, "scala/runtime/BoxedUnit", "UNIT", "Lscala/runtime/BoxedUnit;"),
      VarOp(ASTORE, 2),
      Op(RETURN), // break()
      Label(8),
      FrameEntry(0, List(), List("java/lang/Throwable")),
      Op(NOP),
      Op(NOP),
      Op(NOP),
      Op(NOP),
      Op(NOP),
      Op(ATHROW),
      Label(16),
      FrameEntry(4, List(), List("java/lang/Throwable")),
      Op(NOP),
      Op(NOP),
      Op(ATHROW),
      Label(21),
      FrameEntry(0, List("Test", "scala/util/boundary$Label"), List("scala/util/boundary$Break")),
      VarOp(ASTORE, 3),
      VarOp(ALOAD, 3),
      Invoke(INVOKEVIRTUAL, "scala/util/boundary$Break", "label", "()Lscala/util/boundary$Label;", false),
      VarOp(ALOAD, 1),
      Jump(IF_ACMPNE, Label(29)),
      Jump(GOTO, Label(33)),
      Label(29),
      FrameEntry(1, List(0, "scala/util/boundary$Break"), List()),
      VarOp(ALOAD, 3),
      Op(ATHROW),
      Label(33),
      FrameEntry(3, List(), List()),
      Jump(GOTO, Label(36)),
      Label(36),
      FrameEntry(3, List(), List()),
      Op(RETURN),
    )
  }

  private def testLabelBytecode(code: String, tpe: String, expected: Instruction*): Unit = {
    val source =
      s"""import scala.util.*
         |class Test:
         |  def test: $tpe = {
         |    ${code.lines().toList().asScala.mkString("", "\n    ", "")}
         |  }
         |  def nonLocalBreak[T](value: T)(using boundary.Label[T]): Nothing = break(value)
         |  def nonLocalBreak()(using boundary.Label[Unit]): Nothing = break(())
      """.stripMargin

    checkBCode(source) { dir =>
      val clsIn   = dir.lookupName("Test.class", directory = false).input
      val clsNode = loadClassNode(clsIn)
      val method  = getMethod(clsNode, "test")

      val instructions = instructionsFromMethod(method)

      val expectedList = expected.toList

      assert(instructions == expectedList,
        "`test` was not properly generated\n" + diffInstructions(instructions, expectedList))
    }
  }

}
