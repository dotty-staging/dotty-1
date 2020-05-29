package xsbt;

import xsbti.VirtualFile;
import xsbti.PathBasedFile;

import java.io.InputStream;
import java.io.File;

import dotty.tools.dotc.interfaces.incremental.SourceHandle;

public class VirtualSourceHandle implements SourceHandle {
  private final VirtualFile _source;

  public VirtualSourceHandle(VirtualFile source) {
    this._source = source;
  }

  public VirtualFile delegate() {
    return _source;
  }

  public long contentHash() {
    return _source.contentHash();
  }

  public InputStream input() {
    return _source.input();
  }

  public String id() {
    return _source.id();
  }

  public String name() {
    return _source.name();
  }

  public String[] names() {
    return _source.names();
  }

  public File jfileOrNull() {
    if (_source instanceof PathBasedFile) {
      return ((PathBasedFile) _source).toPath().toFile();
    }
    else {
      return null;
    }
  }

}
