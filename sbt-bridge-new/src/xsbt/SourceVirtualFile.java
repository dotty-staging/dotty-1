package xsbt;

import xsbti.VirtualFile;
import xsbti.VirtualFileRef;

import java.io.InputStream;

import dotty.tools.dotc.interfaces.incremental.SourceHandle;

public class SourceVirtualFile implements VirtualFile {
  private final SourceHandle _source;

  public SourceVirtualFile(SourceHandle source) {
    this._source = source;
  }

  public SourceHandle delegate() {
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

  // public boolean equals(Object obj) {
  //   if (this == obj) {
  //     return true;
  //   } else if (!(obj instanceof VirtualFileRef)) {
  //     return false;
  //   } else {
  //     VirtualFileRef o = (VirtualFileRef) obj;
  //     return this.id().equals(o.id());
  //   }
  // }

  // public int hashCode() {
  //   return 37 * (37 * (17 + "xsbt.SourceVirtualFile".hashCode()) + id().hashCode());
  // }

  // public String toString() {
  //   return id();
  // }

}
