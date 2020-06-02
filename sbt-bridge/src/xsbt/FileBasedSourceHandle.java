package xsbt;

import java.io.InputStream;
import java.util.Objects;
import java.nio.file.Path;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;

import dotty.tools.dotc.interfaces.incremental.SourceHandle;

public class FileBasedSourceHandle implements SourceHandle {
  private final File _source;

  public FileBasedSourceHandle(File source) {
    this._source = source.getAbsoluteFile();
  }

  public File file() {
    return _source;
  }

  public InputStream input() {
    try {
      return new FileInputStream(_source);
    } catch (FileNotFoundException e) {
      throw new IllegalStateException(e);
    }
  }

  public String id() {
    return _source.toString();
  }

  public Path pathOrNull() {
    return _source.toPath();
  }

}
