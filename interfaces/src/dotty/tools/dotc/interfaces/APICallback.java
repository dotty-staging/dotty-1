package dotty.tools.dotc.interfaces;

import java.nio.file.Path;

public interface APICallback {
  default void startSource(Path src) {}
}
