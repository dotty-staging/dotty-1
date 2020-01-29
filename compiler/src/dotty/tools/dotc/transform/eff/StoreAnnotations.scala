package dotty.tools.dotc
package transform
package eff

import core._
import core.Decorators._
import core.Symbols._

import NameOps.NameDecorator

import Types._

import ast.tpd
import ast.tpd._
import core.Contexts
import core.Contexts.Context
import config.Printers.debug

import Annotations.Annotation
import Constants.Constant

import printing.{Showable, Printer}
import printing.Texts.Text

import transform.MegaPhase.MiniPhase

// import util.SimpleIdentityMap
import util.SimpleIdentitySet

class StoreAnnotations extends MiniPhase {

  override def phaseName = StoreAnnotations.name

  override def transformDefDef(tree: DefDef)(implicit ctx: Context): Tree = {

    var didMap: Boolean = false
    val mapped = tree.vparamss.foreach { _.foreach { vparam =>

      vparam.typeOpt.widen match {
        case AppliedType(tycon: TypeRef, params) if tycon.symbol.name.isFunction =>
          val localParams =
            params.iterator.zipWithIndex
              .filter { case (tp, _) => tp.hasAnnotation(ctx.definitions.LocalAnnot) }
              .map { case (_, idx) => idx }
              .toSeq

          if (localParams.nonEmpty) {
            didMap = true
            val stringifiedLocalParams = localParams.mkString(",")
            vparam.symbol.addAnnotation(Annotation.apply(
                ctx.definitions.LocalParamsAnnot,
                tpd.Literal(Constant(stringifiedLocalParams))
            ))
          }

        case _ => ;
      }
    }}

    if (didMap) then
      debug.println(i"StoreAnnotations/mapped: ${tree.symbol}")
      tree.symbol.addAnnotation(Annotation.apply(
        ctx.definitions.LocalParamsAnnot,
        tpd.Literal(Constant(null))
      ))

    tree

  }
}

object StoreAnnotations {

  val name = "StoreAnnotations"

}
