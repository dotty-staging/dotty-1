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

import java.io.File;
import java.io.InputStream;

/** Contains the minimum interface for bridging between versions of Zinc that used either File or VirtualFile */
public interface SourceHandle {
  long contentHash();
  InputStream input();
  public String id();
  public String name();
  public String[] names();

  /** Nullable if virtual */
  public File jfile();
}
