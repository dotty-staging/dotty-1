package xsbt;

import xsbti.AnalysisCallback;
import xsbti.UseScope;
import xsbti.api.DependencyContext;

import java.nio.file.Path;
import java.util.EnumSet;

import dotty.tools.dotc.interfaces.incremental.IncrementalCallback;
import dotty.tools.dotc.interfaces.incremental.SourceHandle;

public final class IncrementalCallbackImpl implements IncrementalCallback {

  private final AnalysisCallback delegate;

  public IncrementalCallbackImpl(AnalysisCallback delegate) {
    this.delegate = delegate;
  }

  public void startSource(SourceHandle source) {
    delegate.startSource(source.jfile());
  }

  public void classDependency(String onClassName, String sourceClassName, DependencyContext context) {
    delegate.classDependency(onClassName, sourceClassName, context);
  }

  public void binaryDependency(Path onBinaryEntry, String onBinaryClassName, String fromClassName, SourceHandle fromSourceFile,
      DependencyContext context) {
    delegate.binaryDependency(onBinaryEntry.toFile(), onBinaryClassName, fromClassName, fromSourceFile.jfile(), context);
  }

  public void generatedNonLocalClass(SourceHandle source, Path classFile, String binaryClassName, String srcClassName) {
    delegate.generatedNonLocalClass(source.jfile(), classFile.toFile(), binaryClassName, srcClassName);
  }

  public void generatedLocalClass(SourceHandle source, Path classFile) {
    delegate.generatedLocalClass(source.jfile(), classFile.toFile());
  }

  public void api(SourceHandle sourceFile, xsbti.api.ClassLike classApi) {
    delegate.api(sourceFile.jfile(), classApi);
  }

  public void mainClass(SourceHandle sourceFile, String className) {
    delegate.mainClass(sourceFile.jfile(), className);
  }

  public void usedName(String className, String name, EnumSet<UseScope> useScopes) {
    delegate.usedName(className, name, useScopes);
  }


}
