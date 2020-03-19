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

  interface ParameterModifier {
    int REPEATED = 0;
    int PLAIN = 1;
    int BY_NAME = 2;
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

  default void startTypeSequence() {}
  default void endTypeSequence() {}

  default void startClassDefinitionSequence() {}
  default void endClassDefinitionSequence() {}

  default void startTypeParameterSequence() {}
  default void endTypeParameterSequence() {}

  default void startAnnotationSequence() {}
  default void endAnnotationSequence() {}

  default void startAnnotationArgumentSequence() {}
  default void endAnnotationArgumentSequence() {}

  default void startParameterListSequence() {}
  default void endParameterListSequence() {}

  default void startStringSequence() {}
  default void endStringSequence() {}

  default void startTypeParameter(String name, int variance) {}
  default void endTypeParameter() {}

  default void startTypeDeclaration(String name) {}
  default void endTypeDeclaration() {}

  default void startTypeAlias(String name) {}
  default void endTypeAlias() {}

  default void startAnnotated() {}
  default void endAnnotated() {}

  default void startAnnotation() {}
  default void endAnnotation() {}

  default void startParameterized() {}
  default void endParameterized() {}

  default void startPolymorphic() {}
  default void endPolymorphic() {}

  default void startExistential() {}
  default void endExistential() {}

  default void startVal(String name) {}
  default void endVal() {}

  default void startVar(String name) {}
  default void endVar() {}

  default void startDef(String name) {}
  default void endDef() {}

  default void startParameterList(boolean isImplicit) {}
  default void endParameterList() {}

  default void startMethodParameter(String name, boolean isDefault, int parameterModifier) {}
  default void endMethodParameter() {}

  default void startProjection(String selected) {}
  default void endProjection() {}

  default void startConstant(String value) {}
  default void endConstant() {}

  default void startSingleton() {}
  default void endSingleton() {}

  default void startPath() {}
  default void endPath() {}

  default void emptyType() {}

  default void parameterRef(String paramName) {}
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
