package dotty.tools.dotc.core.tasty
import dotty.tools.tasty.TastyBuffer
import collection.mutable

class ScratchData:
  var delta, delta1 = new Array[Int](50000)

  val positionBuffer = new TastyBuffer(5000)
  val pickledIndices = new mutable.BitSet

  val commentBuffer = new TastyBuffer(5000)

  def reset() =
    positionBuffer.reset()
    pickledIndices.clear()
    commentBuffer.reset()

