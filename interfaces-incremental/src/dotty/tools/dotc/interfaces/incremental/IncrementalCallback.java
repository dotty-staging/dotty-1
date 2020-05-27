/*
 * Zinc - The incremental compiler for Scala.
 * Copyright Lightbend, Inc. and Mark Harrah
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */
package dotty.tools.dotc.interfaces.incremental;

import xsbti.UseScope;
import xsbti.api.DependencyContext;

import java.io.File;
import java.nio.file.Path;
import java.util.EnumSet;

public interface IncrementalCallback {

  void startSource(SourceHandle source);

  void classDependency(String onClassName, String sourceClassName, DependencyContext context);

  void binaryDependency(Path onBinaryEntry, String onBinaryClassName, String fromClassName,
      SourceHandle fromSourceFile, DependencyContext context);

  void generatedNonLocalClass(SourceHandle source, Path classFile, String binaryClassName, String srcClassName);

  void generatedLocalClass(SourceHandle source, Path classFile);

  void api(SourceHandle sourceFile, xsbti.api.ClassLike classApi);

  void mainClass(SourceHandle sourceFile, String className);

  void usedName(String className, String name, EnumSet<UseScope> useScopes);

}
