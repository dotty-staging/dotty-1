package dotty.tools
package dotc
package fromtasty

import core._
import Contexts._
import Phases.Phase

class TASTYCompiler extends Compiler {

  override protected def frontendPhases: List[List[Phase]] =
    List(new ReadTasty) :: Nil

  override protected def picklerPhases: List[List[Phase]] =
    super.picklerPhases.filterNot(_.exists(_.isInstanceOf[sbt.ExtractAPI]))

  override def newRun(using Context): Run = {
    reset()
    new TASTYRun(this, ctx.addMode(Mode.ReadPositions))
  }
}
