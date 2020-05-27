/* sbt -- Simple Build Tool
 * Copyright 2008, 2009 Mark Harrah
 */
package xsbt;

import xsbti.AnalysisCallback;
import xsbti.Logger;
import xsbti.Reporter;
import xsbti.Severity;
import xsbti.VirtualFile;
import xsbti.PathBasedFile;
import xsbti.compile.*;

import java.io.File;

import dotty.tools.dotc.core.Contexts.Context;
import dotty.tools.dotc.core.Contexts.ContextBase;
import dotty.tools.dotc.Main;
import dotty.tools.dotc.interfaces.*;

import java.net.URLClassLoader;

public class CachedCompilerImpl implements CachedCompiler {
  private final String[] args;
  private final Output output;
  private final String[] outputArgs;

  public CachedCompilerImpl(String[] args, Output output) {
    super();
    this.args = args;
    this.output = output;

    if (!(output instanceof SingleOutput))
      throw new IllegalArgumentException("output should be a SingleOutput, was a " + output.getClass().getName());

    this.outputArgs =
      new String[] { "-d", ((SingleOutput) output).getOutputDirectory().toAbsolutePath().toString() };
  }

  private String absoluteFromVF(VirtualFile source) {
    if (source instanceof PathBasedFile) {
      return ((PathBasedFile) source).toPath().toAbsolutePath().toString();
    }
    return source.id();
  }

  public String[] commandArguments(File[] sources) {
    String[] sortedSourcesAbsolute = new String[sources.length];
    for (int i = 0; i < sources.length; i++)
      sortedSourcesAbsolute[i] = sources[i].getAbsolutePath();
    return commandArgumentsCommon(sortedSourcesAbsolute);
  }

  private String[] commandArgumentsCommon(String[] sortedSourcesAbsolute) {
    java.util.Arrays.sort(sortedSourcesAbsolute);
    // Concatenate outputArgs, args and sortedSourcesAbsolute
    String[] result = new String[outputArgs.length + args.length + sortedSourcesAbsolute.length];
    int j = 0;
    for (int i = 0; i < outputArgs.length; i++, j++)
      result[j] = outputArgs[i];
    for (int i = 0; i < args.length; i++, j++)
      result[j] = args[i];
    for (int i = 0; i < sortedSourcesAbsolute.length; i++, j++)
      result[j] = sortedSourcesAbsolute[i];

    return result;
  }

  private String[] commandArgumentsVF(VirtualFile[] sources) {
    String[] sortedSourcesAbsolute = new String[sources.length];
    for (int i = 0; i < sources.length; i++)
      sortedSourcesAbsolute[i] = absoluteFromVF(sources[i]);
    return commandArgumentsCommon(sortedSourcesAbsolute);
  }

  synchronized public void run(VirtualFile[] sources, DependencyChanges changes, AnalysisCallback callback, Logger log,
      Reporter delegate, CompileProgress progress) {
    log.debug(() -> {
      String msg = "Calling Dotty compiler with arguments  (CompilerInterface):";
      for (String arg : args)
        msg = msg + "\n\t" + arg;
      return msg;
    });

    Context ctx = new ContextBase().initialCtx().fresh().setSbtCallback(callback)
        .setReporter(new DelegatingReporter(delegate));

    dotty.tools.dotc.reporting.Reporter reporter = Main.process(commandArgumentsVF(sources), ctx);
    if (reporter.hasErrors()) {
      throw new InterfaceCompileFailed(args, new Problem[0]);
    }
  }
}
