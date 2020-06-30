/* sbt -- Simple Build Tool
 * Copyright 2008, 2009 Mark Harrah
 */
package xsbt;

import xsbti.Logger;
import xsbti.Reporter;
import xsbti.VirtualFile;
import xsbti.PathBasedFile;

public class ScaladocInterface {
  public void run(VirtualFile[] sources, String[] args, Logger log, xsbti.Reporter delegate) {
    String[] args1 = new String[sources.length + args.length];
    int i = args.length;
    System.arraycopy(args, 0, args1, 0, args.length);
    for (VirtualFile file: sources) {
      System.out.println("inspecting file " + file);
      if (!(file instanceof PathBasedFile)) {
        log.error(() -> file + " is an unsupported virtual file."); // TODO: is it worth updating dotty doc for virtual files, or wait for tastydoc?
        return;
      }
      args1[i++] = ((PathBasedFile)file).toPath().toString();
    }
    System.out.println("final args:\n" + java.util.Arrays.toString(args1));
    new DottydocRunner(args1, log, delegate).run();
  }
}
