import scala.quoted.*
import scala.tasty.inspector.*

import scala.io.Source

import java.io.File
import java.nio.file.Path
import java.nio.file.Files
import java.nio.file.Paths
import java.text.Collator
import java.util.stream.Collectors
import scala.collection.JavaConverters._

import java.net.URLClassLoader

object Test:
  def main(args: Array[String]): Unit =
    val files: List[Path] = Files.walk(Paths.get("tests/run-custom-args/tasty-inspector/i12669/resources")).collect(Collectors.toList).asScala.toList.filter(_.toString.endsWith("tasty"))

    // There are 8 tasty files.
    assert(files.size == 8)
    // All tasty files have the same content.
    assert(files.map(f => Files.readAllBytes(f).toVector).distinct.size == 1)

    val inspector = new Inspector:
      override def inspect(using q: Quotes)(tastys: List[Tasty[quotes.type]]): Unit =
        import q.reflect.*
        for tasty <- tastys do
          // File should not be empty; i.e. its AST should be a PackageDef.
          assert(tasty.ast.isInstanceOf[dotty.tools.dotc.ast.Trees.PackageDef[?]])

    for file <- files do
      TastyInspector.inspectTastyFiles(List(file.toAbsolutePath.toString))(inspector)
