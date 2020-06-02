package xsbt;

import xsbti.VirtualFile;
import xsbti.PathBasedFile;

import java.io.InputStream;
import java.nio.file.Path;

import dotty.tools.dotc.interfaces.incremental.SourceHandle;

public class VirtualSourceHandle implements SourceHandle {
  private final VirtualFile _source;

  public VirtualSourceHandle(VirtualFile source) {
    this._source = source;
  }

  public VirtualFile virtualFile() {
    return _source;
  }

  public InputStream input() {
    return _source.input();
  }

  public String id() {
    return _source.id();
  }

  public Path pathOrNull() {
    if (_source instanceof PathBasedFile) {
      return ((PathBasedFile) _source).toPath();
    }
    else {
      return null;
    }
  }

}
