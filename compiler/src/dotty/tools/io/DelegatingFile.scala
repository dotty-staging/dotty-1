package dotty.tools.io

import java.io.{
  IOException, InputStream, OutputStream, BufferedOutputStream,
  ByteArrayOutputStream
}
import java.net.URL
import java.nio.file.{FileAlreadyExistsException, Files, Paths}

import dotty.tools.dotc.interfaces

final class DelegatingFile(val handle: interfaces.incremental.SourceHandle) extends AbstractFile:

  override def input: InputStream = handle.input
  override def absolute: DelegatingFile = this

  def jpath: JPath = handle.pathOrNull

  def create(): Unit = unsupported()
  def delete(): Unit = unsupported()

  def container: AbstractFile = NoAbstractFile
  def isDirectory: Boolean = false

  /** Returns all abstract subfiles of this abstract directory. */
  def iterator: Iterator[AbstractFile] = {
    assert(isDirectory, "not a directory '" + this + "'")
    Iterator.empty
  }

  def lastModified: Long = 0

  /**
   * Returns the abstract file in this abstract directory with the
   * specified name. If there is no such file, returns null. The
   * argument "directory" tells whether to look for a directory or
   * or a regular file.
   */
  def lookupName(name: String, directory: Boolean): AbstractFile = {
    assert(isDirectory, "not a directory '" + this + "'")
    null
  }

  /** Returns an abstract file with the given name. It does not
   *  check that it exists.
   */
  def lookupNameUnchecked(name: String, directory: Boolean): AbstractFile = unsupported()

  def name: String =
    def slow(): String =
      val idx = handle.id.lastIndexOf('/')
      handle.id.substring(idx + 1)
    val path0 = handle.pathOrNull
    if path0 == null then slow()
    else path0.getFileName.toString

  def output: OutputStream = unsupported()

  def path: String =
    val path0 = handle.pathOrNull
    if path0 == null then handle.id
    else path0.toString
