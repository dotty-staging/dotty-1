/*
 * Zinc - The incremental compiler for Scala.
 * Copyright Lightbend, Inc. and Mark Harrah
 */

package dotty.tools.xsbt;

import dotty.tools.io.Streamable;
import xsbti.PathBasedFile;
import xsbti.VirtualFile;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

public class ZincPlainFile extends dotty.tools.io.PlainFile {
  private final PathBasedFile _underlying;

  public ZincPlainFile(PathBasedFile underlying) {
    super(new dotty.tools.io.Path(underlying.toPath()));
    this._underlying = underlying;
  }

	@Override
  public VirtualFile underlying() {
    return _underlying;
  }
}
