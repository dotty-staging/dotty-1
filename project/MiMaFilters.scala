
import com.typesafe.tools.mima.core._

object MiMaFilters {
  val Library: Seq[ProblemFilter] = Seq(
    ProblemFilters.exclude[MissingClassProblem]("scala.annotation.internal.MappedAlternative"),

    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.runtime.LazyVals.getStaticFieldOffset"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.runtime.LazyVals.objCAS"),
    ProblemFilters.exclude[MissingClassProblem]("scala.runtime.LazyVals$LazyValControlState"),
    ProblemFilters.exclude[MissingClassProblem]("scala.runtime.LazyVals$Evaluating$"),
    ProblemFilters.exclude[MissingClassProblem]("scala.runtime.LazyVals$NullValue$"),
    ProblemFilters.exclude[MissingClassProblem]("scala.runtime.LazyVals$Waiting"),
    ProblemFilters.exclude[MissingFieldProblem]("scala.runtime.LazyVals.Evaluating"),
    ProblemFilters.exclude[MissingFieldProblem]("scala.runtime.LazyVals.NullValue"),

    ProblemFilters.exclude[MissingFieldProblem]("scala.runtime.stdLibPatches.language#experimental.pureFunctions"),
    ProblemFilters.exclude[MissingFieldProblem]("scala.runtime.stdLibPatches.language#experimental.captureChecking"),
    ProblemFilters.exclude[MissingClassProblem]("scala.runtime.stdLibPatches.language$experimental$pureFunctions$"),
    ProblemFilters.exclude[MissingClassProblem]("scala.runtime.stdLibPatches.language$experimental$captureChecking$"),
    ProblemFilters.exclude[MissingClassProblem]("scala.caps"),

    ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.quoted.Quotes#reflectModule#AppliedTypeModule.apply"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.quoted.Quotes#reflectModule#AppliedTypeModule.apply"),
    ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.quoted.Quotes#reflectModule#SymbolMethods.asQuotes"),
    ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.quoted.Quotes#reflectModule#SymbolMethods.typeRef"),
    ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.quoted.Quotes#reflectModule#SymbolMethods.termRef"),
    ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.quoted.Quotes#reflectModule#TypeTreeModule.ref"),
    // Private to the compiler - needed for forward binary compatibility
    ProblemFilters.exclude[MissingClassProblem]("scala.annotation.since"),

    // Macro annotations
    ProblemFilters.exclude[MissingClassProblem]("scala.annotation.MacroAnnotation"),
  )
}
