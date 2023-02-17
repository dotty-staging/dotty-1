package dotty.tools
package dotc
package refine

import core.*
import Types.*, Symbols.*, Contexts.*, ast.tpd.*
import scala.collection.mutable
import Refinement.*
import dotty.tools.dotc.reporting.trace

abstract class RefinementsSolver:
  extension (self: Refinement)
    def tryImply(other: Refinement)(using Context): Boolean
    def instantiate()(using Context): Refinement

  def push(): Unit
  def abort(): Unit
  def pop(): Unit

  def freshVar(): Var

final class RefinementsNaiveSolver extends RefinementsSolver:
  /*
  private val subsetsOf = mutable.Map[Refinements, Set[Refinements]]()
  private val supersetsOf = mutable.Map[Refinements, Set[Refinements]]()

  extension (self: Refinements)
    def elems: Set[Refinement] =
      self match
        case StaticRefinements(elems) => elems
        case self: VariableRefinements => subsetsOf(self).flatMap(_.elems)

    private def impliesAll(otherElems: Set[Refinement]): Boolean =
      self match
        case StaticRefinements(elems) => otherElems.subsetOf(elems)
        case self: VariableRefinements => supersetsOf(self).forall(_.impliesAll(otherElems))

    override def tryImply(other: Refinements): Boolean =
      if self.impliesAll(other.elems) then
        // Save implication
        val prevSubsets = subsetsOf.getOrElse(self, Set.empty)
        subsetsOf.update(self, prevSubsets + other)
        val prevSupersets = supersetsOf.getOrElse(other, Set.empty)
        supersetsOf.update(other, prevSupersets + self)
        if inTransaction then transaction.addOne((self, other))
        true
      else
        false

  */

  private val implicationsOf = mutable.Map[Refinement.Var, Set[Refinement]]()
  private val emptyVars = mutable.Set[Var]()

  extension (self: Refinement)
    override def tryImply(other: Refinement)(using Context): Boolean =
      trace(f"tryImply ${self.show} ==> ${other.show}") {
        self match
          case True =>
            other match
              case True => true
              case other: Var =>
                if emptyVars.contains(other) then
                  true
                else
                  if implicationsOf.getOrElse(other, Set.empty).isEmpty then
                    emptyVars.add(other)
                    println(f"Empty var: ${other.show}")
                    true
                  else
                    false
              case Leaf(tree) =>
                false
              case And(left, right) =>
                self.tryImply(left) && self.tryImply(right)
              case Or(left, right) =>
                self.tryImply(left) || self.tryImply(right)
          case self: Var =>
            if emptyVars.contains(self) then
              other == True
            else
              val prev = implicationsOf.getOrElse(self, Set.empty)
              implicationsOf.update(self, prev + other)
              println(f"Constraint: ${self.show} -> ${other.show}")
              true
          case Leaf(tree) =>
            true
          case And(left, right) =>
            left.tryImply(other) || right.tryImply(other)
          case Or(left, right) =>
            left.tryImply(other) && right.tryImply(other)
      }


    override def instantiate()(using Context): Refinement =
      val res = self match
        case And(left, right) => and(left.instantiate(), right.instantiate())
        case Or(left, right) => or(left.instantiate(), right.instantiate())
        case self: Var => implicationsOf.getOrElse(self, Set.empty).map(_.instantiate()).fold(True)(and)
        case _ => self
      //println(f"instantiate(${self.show}) == ${res.show}"
      res

  private val transaction = mutable.ArrayBuffer[(Refinement.Var, Refinement)]()
  private var inTransaction = false

  override def push() =
    assert(!inTransaction && transaction.isEmpty)
    inTransaction = true

  override def abort() =
    for case (from, to) <- transaction do
      implicationsOf.update(from, implicationsOf(from) - to)
    pop()

  override def pop() =
    assert(inTransaction)
    inTransaction = false
    transaction.clear()


  var i: Int = 0

  override def freshVar(): Var =
    i = i + 1
    Var(i)
