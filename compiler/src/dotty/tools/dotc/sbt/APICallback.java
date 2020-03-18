package dotty.tools.dotc.sbt;

import java.nio.file.Path;

public interface APICallback {

  interface DefinitionType {
    int TRAIT = 0;
    int CLASS_DEF = 1;
    int MODULE = 2;
    int PACKAGE_MODULE = 3;
  }

  interface Variance {
    int CONTRAVARIANT = 0;
    int COVARIANT = 1;
    int INVARIANT = 2;
  }

  default void startSource(Path src) {}
  default void endSource() {}

  default void startClassLike(int definitionType, String name, boolean isTopLevel) {}
  default void endClassLike() {}

  default void startClassLikeDef(int definitionType, String name) {}
  default void endClassLikeDef() {}

  /** register the last seen value as a nonLocalClass and consume it */
  default void saveNonLocalClass() {}

  default void registerMainClass(String name) {}

  /**
   * Cache the last seen value by id.
   * @param id a key to store the last seen value.
   */
  default void registerSharedWith(long id) {}

  /**
   * Retrieve a value by id
   * @param id a key for a shared value
   */
  default void sharedValue(long id) {}

  default void startEvaluatedTask() {}
  default void endEvaluatedTask() {}
  default void delayTask(Runnable task) {}
  default void forceDelayedTasks() {}

  default void startStructure(){}
  default void endStructure(){}

  default void endTypeSequence() {}
  default void endClassDefinitionSequence() {}
  default void endTypeParameterSequence() {}
  default void endAnnotationSequence() {}
  default void endAnnotationArgumentSequence() {}
  default void endParameterListSequence() {}
  default void endStringSequence() {}

  default void startTypeParameter(String name, int variance) {}
  default void endTypeParameter() {}

  default void startTypeDeclaration(String name) {}
  default void endTypeDeclaration() {}

  default void startTypeAlias(String name) {}
  default void endTypeAlias() {}

  default void startAnnotation() {}
  default void endAnnotation() {}

  default void startVal(String name) {}
  default void endVal() {}

  default void startVar(String name) {}
  default void endVar() {}

  default void startDef(String name) {}
  default void endDef() {}

  default void startParameterList(boolean isImplicit) {}
  default void endParameterList() {}

  default void startProjection(String selected) {}
  default void endProjection() {}

  default void startConstant(String value) {}
  default void endConstant() {}

  default void startSingleton() {}
  default void endSingleton() {}

  default void startPath() {}
  default void endPath() {}

  default void emptyType() {}

  default void annotationArgument(String name, String value) {}
  default void id(String name) {}
  default void thisId() {}
  default void publicAPI() {}
  default void localAPI(boolean isProtected) {}
  default void qualifiedAPI(boolean isProtected, String optionalQualifier) {}
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
