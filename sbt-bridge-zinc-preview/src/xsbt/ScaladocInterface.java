/* sbt -- Simple Build Tool
 * Copyright 2008, 2009 Mark Harrah
 */
package xsbt;

import xsbti.Logger;
import xsbti.Reporter;
import xsbti.VirtualFile;

public class ScaladocInterface {
  public void run(VirtualFile[] sources, String[] args, Logger log, xsbti.Reporter delegate) {
    new DottydocRunner(sources, args, log, delegate).run();
  }
}
