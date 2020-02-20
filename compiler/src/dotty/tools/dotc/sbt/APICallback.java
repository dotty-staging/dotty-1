package dotty.tools.dotc.sbt;

import java.nio.file.Path;
import xsbti.api.DefinitionType;

public interface APICallback {

  default void startSource(Path src) {}
  default void endSource() {}

  default void startClassLikeDef(DefinitionType dt, String name) {}
  default void endClassLikeDef() {}

  default void startVal(String name) {}
  default void endVal() {}

  default void startDef(String name) {}
  default void endDef() {}

  default void startParamList(boolean isImplicit) {}
  default void endParamList() {}

  default void startProjection(String selected) {}
  default void endProjection() {}

  default void startConstant(Object constant) {}
  default void endConstant() {}

  default void startSingleton() {}
  default void endSingleton() {}

  default void startPath() {}
  default void endPath() {}

  default void id(String name) {}
  default void thisId() {}
  default void publicAPI() {}
  default void modifiers(
    boolean isAbstract,
    boolean isOverride,
    boolean isFinal,
    boolean isSealed,
    boolean isImplicit,
    boolean isLazy,
    boolean isMacro,
    boolean isSuperAccessor
  ) {}

}
