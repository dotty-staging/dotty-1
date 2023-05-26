/*
 * Copyright (c) 2014 Contributor. All rights reserved.
 */
package dotty.tools
package dotc.classpath

import scala.language.unsafeNulls

import java.io.{File => JFile, FileFilter}
import java.net.URL
import dotty.tools.io.AbstractFile

/**
 * Common methods related to Java files and abstract files used in the context of classpath
 */
object FileUtils {
  extension (file: AbstractFile) {
    def isPackage: Boolean = file.isDirectory && mayBeValidPackage(file.name)

    def isClass: Boolean = !file.isDirectory && file.hasExtension("class") && !file.name.endsWith("$class.class")
      // FIXME: drop last condition when we stop being compatible with Scala 2.11

    def isTasty: Boolean = !file.isDirectory && file.hasExtension("tasty")
      // FIXME: Tuple fails if loaded from TASTy.
      // Reproduce with: scala3-bootstrapped/testCompilation tests/pos-macros/i7358.scala
      // assertion failed: wrong source set for runtime.Tuples.cons(x$1, Tuple_this).asInstanceOf[*:[quoted.Type[H], Tuple]]:*:[quoted.Type[H], Tuple] # -1 of class dotty.tools.dotc.ast.Trees$Typed, set to library/src/scala/Tuple.scala but context had /Users/nicolasstucki/GitHub/dotty/library/../out/bootstrap/scala3-library-bootstrapped/scala-3.3.1-RC1-bin-SNAPSHOT-nonbootstrapped/scala3-library_3-3.3.1-RC1-bin-SNAPSHOT.jar(scala/Tuple.tasty)
      //    final <method> <permanent>
      //         at scala.runtime.Scala3RunTime$.assertFailed(Scala3RunTime.scala:8)
      //         at dotty.tools.dotc.transform.YCheckPositions$$anon$1.traverse(YCheckPositions.scala:36)
      //         at dotty.tools.dotc.transform.YCheckPositions$$anon$1.traverse(YCheckPositions.scala:51)
      && !file.path.endsWith("scala/Tuple.tasty")
      && !file.path.endsWith("scala/util/boundary.tasty")

    def isScalaOrJavaSource: Boolean = !file.isDirectory && (file.hasExtension("scala") || file.hasExtension("java"))

    // TODO do we need to check also other files using ZipMagicNumber like in scala.tools.nsc.io.Jar.isJarOrZip?
    def isJarOrZip: Boolean = file.hasExtension("jar") || file.hasExtension("zip")

    /**
     * Safe method returning a sequence containing one URL representing this file, when underlying file exists,
     * and returning given default value in other case
     */
    def toURLs(default: => Seq[URL] = Seq.empty): Seq[URL] = if (file.file == null) default else Seq(file.toURL)
  }

  extension (file: JFile) {
    def isPackage: Boolean = file.isDirectory && mayBeValidPackage(file.getName)

    def isClass: Boolean = file.isFile && file.getName.endsWith(SUFFIX_CLASS) && !file.getName.endsWith("$class.class")
    // FIXME: drop last condition when we stop being compatible with Scala 2.11

    def isTasty: Boolean = file.isFile && file.getName.endsWith(SUFFIX_TASTY)
  }

  private val SUFFIX_CLASS = ".class"
  private val SUFFIX_SCALA = ".scala"
  private val SUFFIX_TASTY = ".tasty"
  private val SUFFIX_JAVA = ".java"
  private val SUFFIX_SIG = ".sig"

  def stripSourceExtension(fileName: String): String =
    if (endsScala(fileName)) stripClassExtension(fileName)
    else if (endsJava(fileName)) stripJavaExtension(fileName)
    else throw new FatalError("Unexpected source file ending: " + fileName)

  def dirPath(forPackage: String): String = forPackage.replace('.', JFile.separatorChar)

  def dirPathInJar(forPackage: String): String = forPackage.replace('.', '/')

  inline private def ends (filename:String, suffix:String) = filename.endsWith(suffix) && filename.length > suffix.length

  def endsClass(fileName: String): Boolean =
    ends (fileName, SUFFIX_CLASS) || fileName.endsWith(SUFFIX_SIG)

  def endsScalaOrJava(fileName: String): Boolean =
    endsScala(fileName) || endsJava(fileName)

  def endsJava(fileName: String): Boolean =
    ends (fileName, SUFFIX_JAVA)

  def endsScala(fileName: String): Boolean =
    ends (fileName, SUFFIX_SCALA)

  def stripClassExtension(fileName: String): String =
    fileName.substring(0, fileName.lastIndexOf('.'))

  def stripJavaExtension(fileName: String): String =
    fileName.substring(0, fileName.length - 5) // equivalent of fileName.length - SUFFIX_JAVA.length

  // probably it should match a pattern like [a-z_]{1}[a-z0-9_]* but it cannot be changed
  // because then some tests in partest don't pass
  def mayBeValidPackage(dirName: String): Boolean =
    (dirName != "META-INF") && (dirName != "") && (dirName.charAt(0) != '.')

  def mkFileFilter(f: JFile => Boolean): FileFilter = new FileFilter {
    def accept(pathname: JFile): Boolean = f(pathname)
  }
}
