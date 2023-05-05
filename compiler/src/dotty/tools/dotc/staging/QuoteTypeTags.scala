package dotty.tools.dotc.staging

import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.StdNames._
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.staging.StagingLevel.*
import dotty.tools.dotc.util.Property

import scala.collection.mutable.LinkedHashSet

object QuoteTypeTags:

  private val TaggedTypes = new Property.Key[LinkedHashSet[TermRef]]

  def inContextWithQuoteTypeTags[T](body: Context ?=> T)(using Context): T =
    body(using ctx.fresh.setProperty(TaggedTypes, LinkedHashSet.empty))

  def getTagRef(spliced: TermRef)(using Context): Type =
    ctx.property(TaggedTypes).get += spliced
    spliced.select(tpnme.Underlying)

  def getTypeTags()(using Context): List[TermRef] =
    ctx.property(TaggedTypes).get.toList
