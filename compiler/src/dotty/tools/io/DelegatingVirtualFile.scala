package dotty.tools.io

import dotty.tools.dotc.interfaces

import java.io.{InputStream, File}
import java.nio.file.{Path => JPath}

object DelegatingVirtualFile:
  def pathFromNames(virtualFile: interfaces.incremental.SourceHandle): String =
    val ns = virtualFile.names
    if ns.length == 0 then ""
    else ns.init.mkString("", File.separator, File.separator)

/** Should only be constructed if virtualFile is not backed by a file */
class DelegatingVirtualFile(virtualFile: interfaces.incremental.SourceHandle)
extends VirtualFile(virtualFile.name, DelegatingVirtualFile.pathFromNames(virtualFile)):
  require(virtualFile.jfileOrNull == null)


  override final def id: String = virtualFile.id
  override final def names: Array[String] = virtualFile.names
  override final def input: InputStream = virtualFile.input
  override final def contentHash: Long = virtualFile.contentHash
  // override final def file: File = virtualFile.jfileOrNull

  // private var myJPath: JPath = null

  // def absolute: AbstractFile = unsupported()
  // def container: AbstractFile = unsupported()
  // def create(): Unit = unsupported()
  // def delete(): Unit = unsupported()
  // def isDirectory: Boolean = unsupported()
  // def iterator(): Iterator[AbstractFile] = unsupported()

  // override final def jpath: JPath =
  //   if myJPath == null then
  //     val base = file
  //     if base != null then
  //       myJPath = base.toPath
  //   myJPath

  // def lastModified: Long = unsupported()
  // def lookupName(name: String, directory: Boolean): AbstractFile = unsupported()
  // def lookupNameUnchecked(name: String, directory: Boolean): AbstractFile = unsupported()
  // def output: java.io.OutputStream = unsupported()

  // def path: String =
  //   val ns = names
  //   if ns.length == 0 then ""
  //   else ns.init.mkString("", File.separator, File.separator)
