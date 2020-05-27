package xsbt;

import xsbti.AnalysisCallback;
import xsbti.UseScope;
import xsbti.VirtualFile;
import xsbti.api.DependencyContext;

import java.nio.file.Path;
import java.util.EnumSet;

import dotty.tools.dotc.interfaces.incremental.IncrementalCallback;
import dotty.tools.dotc.interfaces.incremental.SourceHandle;

public final class IncrementalCallbackImpl implements IncrementalCallback {

  private final AnalysisCallback delegate;

  private VirtualFile fromHandle(SourceHandle source) {
    return (source.jfile() != null) ? new SourceVirtualPathBasedFile(source) : new SourceVirtualFile(source);
  }

  public IncrementalCallbackImpl(AnalysisCallback delegate) {
    this.delegate = delegate;
  }

  public void startSource(SourceHandle source) {
    delegate.startSource(fromHandle(source));
  }

  public void classDependency(String onClassName, String sourceClassName, DependencyContext context) {
    delegate.classDependency(onClassName, sourceClassName, context);
  }

  public void binaryDependency(Path onBinaryEntry, String onBinaryClassName, String fromClassName, SourceHandle fromSourceFile,
      DependencyContext context) {
    delegate.binaryDependency(onBinaryEntry, onBinaryClassName, fromClassName, fromHandle(fromSourceFile), context);
  }

  public void generatedNonLocalClass(SourceHandle source, Path classFile, String binaryClassName, String srcClassName) {
    delegate.generatedNonLocalClass(fromHandle(source), classFile, binaryClassName, srcClassName);
  }

  public void generatedLocalClass(SourceHandle source, Path classFile) {
    delegate.generatedLocalClass(fromHandle(source), classFile);
  }

  public void api(SourceHandle sourceFile, xsbti.api.ClassLike classApi) {
    delegate.api(fromHandle(sourceFile), classApi);
  }

  public void mainClass(SourceHandle sourceFile, String className) {
    delegate.mainClass(fromHandle(sourceFile), className);
  }

  public void usedName(String className, String name, EnumSet<UseScope> useScopes) {
    delegate.usedName(className, name, useScopes);
  }


}
