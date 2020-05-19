/** Implements a lightweight call-graph analysis for (whole) Scala programs. */
package dotty.tools
package dotc
package transform
package callgraph

import ast.tpd._

import core._
import core.Flags._
import core.Denotations._
import core.Contexts.Context
import core.Phases._
import core.Symbols._
import core.StdNames._
import core.Types._

import util.Property._

import MegaPhase._
import transform.MegaPhase.MiniPhase

val ResolvedMethodCallKey : Key[Set[Symbol]] = new StickyKey[Set[Symbol]]

// Helper methods for recording the (discovered) class hierarchy in the set of source programs.
class ClassHierarchy {
  val children = scala.collection.mutable.Map[Symbol, Set[Symbol]]()
  val parents = scala.collection.mutable.Map[Symbol, Set[Symbol]]()

  def record(cls : Symbol, parent: Symbol) : Unit = {
    if (cls != parent) {
      children.updateWith(parent) { 
        case Some(symbols) => Some(symbols + cls)
        case None => Some(Set(cls))
      }
      parents.updateWith(cls) {
        case Some(symbols) => Some(symbols + parent)
        case None => Some(Set(parent))
      }
    }
  }

  def recordType(clsType: Type)(implicit ctx: Context) : Unit = {
    // The first element of .baseClasses is always the current class.
    for (parent <- clsType.baseClasses) {
      record(clsType.typeSymbol, parent)
    } 
  }
}

// Helper definitions for recording reachable classes.
type SymbolSet = scala.collection.mutable.Set[Symbol]
def SymbolSet() = scala.collection.mutable.Set[Symbol]()

// Class for doing the method/class reachability analysis.
class ReachabilityEngine(val classHierarchy: ClassHierarchy) {
  val reachableClasses = SymbolSet()
  val reachableMethods = SymbolSet()

  // This is awful, but it looks like it's the best way to actually walk a tree...
  // We will run repeated instances of this single phase until we make no more progress.
  object reachabilitySinglePhase extends MiniPhase {
    val phaseName = "ReachabilityPhase.reachabilitySinglePhase"
    var changed = false

    def enclosingClass(implicit ctx: Context) : Symbol = {
      ctx.outersIterator.dropWhile(!_.isClassDefContext).next().owner
    }

    def enclosingMethod(implicit ctx: Context) : Option[Symbol] = {
      val iter = ctx.outersIterator.dropWhile((outer: Context) => !(outer.owner.denot.is(Method)))

      if (iter.isEmpty) {
        None
      } else {
        Some(iter.next().owner)
      }
    }

    /** Tests if a method is reachable, and if it is, stores it in the reachable methods set.
        Note that the side effect of storing it in the reachable methods set is important! */
    def isReachable(thisClass: Symbol, thisPossibleMethod: Option[Symbol])(implicit ctx: Context) : Boolean = {
      // A block of code is reachable _if_
      // -1) Some special conditions hold (being _main_, inheriting from App)
      //  0) The enclosing method M and class T is directly reachable --> i.e T.M has been invoked somewhere,
      //      and T has been allocated by a reachable statement.
      //  1) The enclosing class T is reachable (i.e the type has been allocated by some reachable statement) 
      //  2) The enclosing method M is indirectly reachable (i.e X.M has been invoked for some superclass X of T)

      thisPossibleMethod match {
      case None => return true
      case Some(thisMethod) => 
        // Some things are always reachable (like the main method inside a object)
        // TODO: Deal with <init> in objects that extend App.
        if (thisMethod.name == nme.main) {
          // Mark the method as visible also
          reachableMethods += thisMethod
          return true
        }

        // If the method is directly reachable, we're good (note that the method symbol contains its owner).
        // OR if we've seen it before as reachable through an indirect call.
        if (reachableMethods.contains(thisMethod)) {
          return true
        }

        // Calls to <init> must be directly reachable.
        if (thisMethod.name == nme.CONSTRUCTOR) {
          return false
        }

        // Otherwise, we need to check if the target class is reachable.
        if (reachableClasses.contains(enclosingClass)) {  
          // Otherwise, the method is not directly reachable, but may be reachable through a virtual method call.
          // Iterate over all other methods...
          for (otherMethod <- reachableMethods) {
            // If the method names match and the owner of the other method is a supertype
            // of the current method's owner, we are reachable.
            if (otherMethod.name == thisMethod.name && {
                  classHierarchy.children.get(otherMethod.owner) match {
                    case Some(childrenSet) =>
                      if (childrenSet.contains(thisClass)) {
                        // Mark it as reachable so we save time in the future.
                        reachableMethods += thisMethod
                        changed = true
                        return true
                      } else {
                        return false
                      }
                    case None => return false
                  }
                }) {
              return true
            } 
          }
        }
        // Otherwise, the method is not reachable.
        return false
      }
    }

    override def transformDefDef(tree: DefDef)(implicit ctx: Context): Tree = {
      if (isReachable(enclosingClass, Some(tree.symbol))) {
        // We just evaluate isReachable for the effect here...just to force methods
        // to appear in the reachable methods set.
      }
      tree
    }

    override def transformNew(tree: New)(implicit ctx: Context): Tree = {
      if (isReachable(enclosingClass, enclosingMethod)) {
        // If we reach a new T(...), T becomes a reachable class.
        if (!reachableClasses.contains(tree.tpe.classSymbol)) {
          reachableClasses += tree.tpe.classSymbol
          changed = true
        }
      }
      tree
    }

    override def transformApply(tree: Apply)(implicit ctx: Context): Tree = {
      val Apply(target, _) = tree
      if (isReachable(enclosingClass, enclosingMethod)) {
        // If we reach obj.foo(), T.foo becomes reachable for all subtypes T <: Type(obj)
        // Note that T does not become reachable, as we track that through new T() invocations.
        if (!reachableMethods.contains(target.symbol)) {
          reachableMethods += target.symbol
          changed = true
        }
      }
      tree
    }
   
    // Override run and runOn to prevent new phases from being introduced (we're running as part of one squashed phase)
    override def run(implicit ctx: Context) = {
      ctx.compilationUnit.tpdTree =
        singletonGroup.transformUnit(ctx.compilationUnit.tpdTree)
    }
    override def runOn(units: List[CompilationUnit])(implicit ctx: Context) : List[CompilationUnit] = {
      for (unit <- units) {
        this.run(ctx.fresh.setCompilationUnit(unit))
      }
      units
    }
  }

  // Run the inner phase in a fixed-point.
  def runOn(units: List[CompilationUnit])(implicit ctx: Context) : List[CompilationUnit] = {
    var modifiedUnits = units
    while ({
      reachabilitySinglePhase.changed = false
      reachabilitySinglePhase.runOn(modifiedUnits)
      reachabilitySinglePhase.changed
    }) ()
    modifiedUnits
  }
}

class CallGraphAnalysis extends Phase {
  val phaseName = "CallGraphAnalysis"
  val classHierarchy = new ClassHierarchy()
  val reachabilityEngine = new ReachabilityEngine(classHierarchy)
 
  // Phase for recording all the classes used. 
  object recordClassesPhase extends MiniPhase {
    val phaseName = "CallGraphAnalysis.recordClassesPhase"

    override def transformTypeDef(tree: TypeDef)(implicit ctx: Context): Tree = {
      if (tree.isClassDef) {
        classHierarchy.recordType(tree.tpe)
      }
      tree
    }

    // Override run and runOn to prevent new phases from being introduced (we're running as part of one squashed phase)
    override def run(implicit ctx: Context) = {
      ctx.compilationUnit.tpdTree =
        singletonGroup.transformUnit(ctx.compilationUnit.tpdTree)
    }
    override def runOn(units: List[CompilationUnit])(implicit ctx: Context) : List[CompilationUnit] = {
      for (unit <- units) {
        this.run(ctx.fresh.setCompilationUnit(unit))
      }
      units
    }
  }

  // Phase for tagging each method call with the list of targets that can be used
  object tagApplyPhase extends MiniPhase {
    val phaseName = "CallGraphAnalysis.tagApplyPhase"

    override def transformApply(tree: Apply)(implicit ctx: Context): Tree = {
      // Get the symbol, mark it with the possible targets
      val target = tree.symbol
      if (target.name != nme.NO_NAME) {
        val owner = target.owner
        val children = classHierarchy.children.get(owner) match {
          case Some(set) => set
          case None => Set()
        }


        // If we're making a super[X] call, it resolves exactly.
        val explicitSuper = tree match {
          case Apply(Select(Super(_, _), _), _) => true
          case _ => false
        }

        val resolved = {
          // Calls to a constructor are exact.
          if (target.name == nme.CONSTRUCTOR) {
            Set(target)
          }
          // Calls to a explicit super type are exact.
          else if (explicitSuper) {
            Set(target)
          }
          else {
            reachabilityEngine.reachableMethods.filter(
              otherMethod => {
                // A direct match is OK
                if (target == otherMethod) {
                  true
                }
                // Otherwise get instantiated methods from subclasses.
                else if (otherMethod.name == target.name && children.contains(otherMethod.owner)) {
                  true
                } 
                else {
                  false
                }
            })
          }
        }
        Console.err.println(s"Tagged call to ${owner}/${target} with ${resolved}")

        if (tree.hasAttachment(ResolvedMethodCallKey)) {
          Console.err.println(s"Call to ${owner}/${target} already has attachment!")
        }
        else {
          // Tag the tree node
          tree.pushAttachment(ResolvedMethodCallKey, resolved)
        }
      } else {
          tree.pushAttachment(ResolvedMethodCallKey, Set(target))
      }
      tree
    }
    
    // Override run and runOn to prevent new phases from being introduced (we're running as part of one squashed phase)
    override def run(implicit ctx: Context) = {
      ctx.compilationUnit.tpdTree =
        singletonGroup.transformUnit(ctx.compilationUnit.tpdTree)
    }
    override def runOn(units: List[CompilationUnit])(implicit ctx: Context) : List[CompilationUnit] = {
      for (unit <- units) {
        this.run(ctx.fresh.setCompilationUnit(unit))
      }
      units
    }
  }

  override def run(implicit ctx: Context) : Unit = {
    // We do nothing here, it's handled in runOn
    throw new RuntimeException("CallGraphAnalysis cannot be run on a single CompilationUnit!")
  }

  override def runOn(units: List[CompilationUnit])(implicit ctx: Context) : List[CompilationUnit] = {
    val ourContext = ctx.withPhase(this)
    val afterCHA = recordClassesPhase.runOn(units)(ourContext)
    val afterReach = reachabilityEngine.runOn(units)(ourContext)
    val afterTag = tagApplyPhase.runOn(units)(ourContext)
    afterTag
  }
}
