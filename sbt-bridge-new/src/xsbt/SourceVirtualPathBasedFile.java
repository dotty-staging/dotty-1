package xsbt;

import xsbti.PathBasedFile;

import java.nio.file.Path;

import dotty.tools.dotc.interfaces.incremental.SourceHandle;

public class SourceVirtualPathBasedFile extends SourceVirtualFile implements PathBasedFile {

  public SourceVirtualPathBasedFile(SourceHandle source) {
    super(source);
  }

  public Path toPath() {
    return delegate().jfileOrNull().toPath();
  }
}
