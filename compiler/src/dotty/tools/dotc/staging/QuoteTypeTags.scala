package dotty.tools.dotc.staging

import dotty.tools.dotc.ast.{tpd, untpd}
import dotty.tools.dotc.core.Annotations._
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.NameKinds._
import dotty.tools.dotc.core.StdNames._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.staging.StagingLevel.*
import dotty.tools.dotc.util.Property
import dotty.tools.dotc.util.Spans._

object QuoteTypeTags {

  private val TaggedTypes = new Property.Key[QuoteTypeTags]

  def contextWithQuoteTypeTags(taggedTypes: QuoteTypeTags)(using Context) =
    ctx.fresh.setProperty(TaggedTypes, taggedTypes)

  def getQuoteTypeTags(using Context): QuoteTypeTags =
    ctx.property(TaggedTypes).get
}

class QuoteTypeTags(span: Span)(using Context) {
  import tpd.*

  private val tags = collection.mutable.LinkedHashSet.empty[TermRef]

  def getTagRef(spliced: TermRef): Type = {
    tags += spliced
    spliced.select(tpnme.Underlying)
  }

  def getTypeTags: List[TermRef] = tags.toList

}
