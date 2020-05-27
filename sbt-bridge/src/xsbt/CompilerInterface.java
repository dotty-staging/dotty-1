/* sbt -- Simple Build Tool
 * Copyright 2008, 2009 Mark Harrah
 */
package xsbt;

import xsbti.AnalysisCallback;
import xsbti.Logger;
import xsbti.Reporter;
import xsbti.Severity;
import xsbti.VirtualFile;
import xsbti.compile.*;

import java.io.File;

import dotty.tools.dotc.core.Contexts.ContextBase;
import dotty.tools.dotc.Main;
import dotty.tools.dotc.interfaces.*;

import java.lang.reflect.InvocationTargetException;
import java.net.URLClassLoader;

public final class CompilerInterface {
  public CachedCompiler newCompiler(String[] options, Output output, Logger initialLog, Reporter initialDelegate) {
    // The classloader that sbt uses to load the compiler bridge is broken
    // (see CompilerClassLoader#fixBridgeLoader for details). To workaround
    // this we construct our own ClassLoader and then run the following code
    // with it:
    //   new CachedCompilerImpl(options, output)

    try {
      ClassLoader bridgeLoader = this.getClass().getClassLoader();
      ClassLoader fixedLoader = CompilerClassLoader.fixBridgeLoader(bridgeLoader);
      Class<?> cciClass = fixedLoader.loadClass("xsbt.CachedCompilerImpl");
      return (CachedCompiler) cciClass.getConstructors()[0].newInstance(options, output);
    } catch (ClassNotFoundException | InstantiationException | IllegalAccessException e) {
      throw new RuntimeException(e);
    } catch (InvocationTargetException ite) {
      if (ite.getCause() != null) {
        throw new RuntimeException(ite.getCause().getMessage(), ite.getCause());
      } else {
        throw new RuntimeException(ite.getMessage());
      }
    }
  }

  public void run(VirtualFile[] sources, DependencyChanges changes, AnalysisCallback callback, Logger log, Reporter delegate,
      CompileProgress progress, CachedCompiler cached) {
    cached.run(sources, changes, callback, log, delegate, progress);
  }

  /**
   * Keep for backwards compat
   */
  public void run(File[] sources, DependencyChanges changes, AnalysisCallback callback, Logger log,
      Reporter delegate, CompileProgress progress, CachedCompiler cached) {
    Class<?> cciClass = cached.getClass();
    try {
      java.lang.reflect.Method run = cciClass.getMethod("run", File[].class, DependencyChanges.class, AnalysisCallback.class, Logger.class, Reporter.class);
      run.invoke(cached, sources, changes, callback, log, delegate, progress);
    } catch (NoSuchMethodException| IllegalAccessException e) {
      throw new RuntimeException("method run with java.io.File[] not available on " + cciClass.toString());
    } catch (InvocationTargetException ite) {
      if (ite.getCause() != null) {
        throw new RuntimeException(ite.getCause().getMessage(), ite.getCause());
      } else {
        throw new RuntimeException(ite.getMessage());
      }
    }
  }
}
