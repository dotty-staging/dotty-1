/* sbt -- Simple Build Tool
 * Copyright 2008, 2009 Mark Harrah
 */
package xsbt;

import xsbti.Logger;
import xsbti.Severity;
import xsbti.VirtualFile;

import java.lang.reflect.Method;
import java.io.InputStream;
import java.lang.reflect.InvocationTargetException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.ArrayList;

import dotty.tools.dotc.core.Contexts.Context;
import dotty.tools.dotc.core.Contexts.ContextBase;
import dotty.tools.dotc.reporting.Reporter;
import dotty.tools.dotc.interfaces.incremental.SourceHandle;

public class DottydocRunner {
  private final VirtualFile[] sources0;
  private final String[] args;
  private final Logger log;
  private final xsbti.Reporter delegate;

  public DottydocRunner(VirtualFile[] sources0, String[] args, Logger log, xsbti.Reporter delegate) {
    super();
    this.sources0 = sources0;
    this.args = args;
    this.log = log;
    this.delegate = delegate;
  }

  private static boolean exists(VirtualFile source) {
    try (InputStream input = source.input()) {
      return input != null;
    } catch (Exception e) {
      return false;
    }
  }

  public void run() {
    log.debug(() -> {
      StringBuilder msg =
        new StringBuilder("Calling Dottydoc with arguments  (ScaladocInterface):");
      for (String arg : args) {
        msg.append("\n\t");
        msg.append(arg);
      }
      for (VirtualFile source: sources0) {
        msg.append("\n\t");
        msg.append(source);
      }
      return msg.toString();
    });

    // When running with `-from-tasty`, remove the source files from arg list.
    VirtualFile[] sources;
    boolean fromTasty = false;
    for (String arg : args) {
      if ("-from-tasty".equals(arg)) {
        fromTasty = true;
        break;
      }
    }
    if (fromTasty) {
      ArrayList<VirtualFile> excluded = new ArrayList<>(sources0.length);
      ArrayList<VirtualFile> retained = new ArrayList<>(sources0.length);
      for (VirtualFile source : sources0) {
        if ((source.id().endsWith(".scala") || source.id().endsWith(".java")) && DottydocRunner.exists(source))
          excluded.add(source);
        else
          retained.add(source);
      }
      log.debug(() -> {
        StringBuilder msg =
          new StringBuilder("Running `-from-tasty`, excluding source files:");
        for (VirtualFile source : excluded) {
          msg.append("\n\t");
          msg.append(source);
        }
        return msg.toString();
      });
      sources = retained.toArray(new VirtualFile[retained.size()]);
    } else {
      sources = sources0;
    }
    SourceHandle[] sourceHandles =
      Arrays.stream(sources).map(VirtualSourceHandle::new).toArray(SourceHandle[]::new);

    Context ctx = new ContextBase().initialCtx().fresh()
      .setReporter(new DelegatingReporter(delegate));

    try {
      Class<?> dottydocMainClass = Class.forName("dotty.tools.dottydoc.Main");
      Method processMethod = dottydocMainClass.getMethod("process", String[].class, SourceHandle[].class, Context.class);
      Reporter reporter = (Reporter) processMethod.invoke(null, args, sourceHandles, ctx);
      if (reporter.hasErrors())
        throw new InterfaceCompileFailed(args, new xsbti.Problem[0]);
    } catch (ClassNotFoundException | NoSuchMethodException | IllegalAccessException e) {
      throw new RuntimeException(e);
    } catch (InvocationTargetException ite) {
      if (ite.getCause() != null) {
        throw new RuntimeException("Error in reflective call: " + ite.getCause().toString(), ite.getCause());
      } else {
        throw new RuntimeException("Error in reflective call: " + ite.getMessage(), ite);
      }
    }
  }
}
