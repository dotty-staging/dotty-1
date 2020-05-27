package dotty.tools.dotc.interfaces.incremental;

import dotty.tools.dotc.interfaces.SourceFile;

import java.io.ByteArrayInputStream;
import java.io.FileInputStream;
import java.io.InputStream;
import java.io.File;
import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.charset.Charset;
import java.util.Optional;

// TODO unneccessary, can be made in the compiler
public final class SourceFileWrapper implements SourceHandle {

  private final SourceFile _source;

  public SourceFileWrapper(SourceFile sourceFile) {
    this._source = sourceFile;
  }

  public long contentHash() { // naive for now
    char[] content = _source.content();
    if (content.length == 0) return 0;
    int hash = 7;
    for (int i = 0; i < content.length; i++) {
      hash = hash * 31 + content[i];
    }
    return hash;
  }

  private InputStream inputFromContents() {
    Charset c = Charset.defaultCharset();
    CharBuffer cb = CharBuffer.wrap(_source.content());
    ByteBuffer bb = c.encode(cb);
    byte[] b = new byte[bb.remaining()];
    bb.get(b);
    return new ByteArrayInputStream(b);
  }

  public InputStream input() {
    return _source.jfile().map(file -> {
      try {
        return new FileInputStream(file);
      } catch (java.io.FileNotFoundException e) {
        return inputFromContents();
      }
    }).orElseGet(this::inputFromContents);
  }

  public String id() {
    return _source.jfile().map(Object::toString).orElseGet(() -> {
      String parent = _source.path();
      if (parent == null || parent == "") {
        return _source.name();
      }
      return parent + "/" + _source.name();
    });
  }

  public String name() {
    return _source.name();
  }

  public String[] names() {
    String parent = _source.path();
    if (parent == null || parent == "") {
      return new String[] { _source.name() };
    }
    String[] parts = parent.split("/");
    String[] results = new String[parts.length + 1];
    int i = 0;
    for (i = 0; i < parts.length; i++)
      results[i] = parts[i];
    results[i] = _source.name();
    return results;
  }

  public File jfile() {
    return _source.jfile().orElse(null);
  }

}
