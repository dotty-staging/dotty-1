package dotty.tools.dotc.sbt

import dotty.tools.dotc.core.Contexts.{Context, ctx}
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.NameOps.stripModuleClassSuffix
import dotty.tools.dotc.core.Names.Name
import dotty.tools.io

inline val TermNameHash = 1987 // 300th prime
inline val TypeNameHash = 1993 // 301st prime
inline val InlineParamHash = 1997 // 302nd prime

  // val underlying = sf.file.underlying
  // if underlying == null then
  //   sys.error(s"no underlying file for $sf [${sf.file.getClass}]\n${sf.tracer}")
  // else underlying
extension (file: io.AbstractFile) def zincVirtualFile(using Context): xsbti.VirtualFile =
  ctx.zincInitialFiles.get(file.absolutePath).nn
  // val underlying = file.underlying
  // if underlying == null then
  //   sys.error(s"no underlying file for $file [${file.getClass}]\n${file.tracer}")
  // else underlying
      // file match
      // case file: io.PlainFile =>
      //   new xsbti.VirtualFile with xsbti.PathBasedFile {
      //     def toPath() = file.jpath

      //           // Members declared in xsbti.VirtualFile
      //     def contentHash(): Long = 0L // should not be accessed
      //     def input(): java.io.InputStream = file.input

      //     // Members declared in xsbti.VirtualFileRef
      //     def id(): String = file.path
      //     def name(): String = file.name
      //     def names(): Array[String] = file.givenPath.segments.toArray
      //   }
      // case file: io.VirtualFile =>
      //   new xsbti.VirtualFile {
      //           // Members declared in xsbti.VirtualFile
      //     def contentHash(): Long = 0L // should not be accessed
      //     def input(): java.io.InputStream = file.input

      //     // Members declared in xsbti.VirtualFileRef
      //     def id(): String = file.path
      //     def name(): String = file.name
      //     def names(): Array[String] = Array(file.path)
      //   }

extension (sym: Symbol)

  def constructorName(using Context) =
    sym.owner.fullName ++ ";init;"

  /** Mangle a JVM symbol name in a format better suited for internal uses by sbt. */
  def zincMangledName(using Context): Name =
    if (sym.isConstructor) constructorName
    else sym.name.stripModuleClassSuffix
