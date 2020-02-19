package dotty.tools.dotc.sbt;

import java.nio.file.Path;
import xsbti.api.DefinitionType;

public interface APICallback {
  default void startSource(Path src) {}

  default void startClassLike(DefinitionType dt, String name) {}

  default void endSource() {}

  default void endClassLike() {}
}
