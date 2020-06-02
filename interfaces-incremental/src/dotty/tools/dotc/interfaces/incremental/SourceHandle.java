package dotty.tools.dotc.interfaces.incremental;

import java.nio.file.Path;
import java.io.InputStream;

/**
 * If this SourceHandle is supplied as input to a compilation run, implementations can rely
 * on recieving this same SourceHandle in the IncrementalCallback.
 */
public interface SourceHandle {
  InputStream input();
  String id();
  Path pathOrNull();
}
