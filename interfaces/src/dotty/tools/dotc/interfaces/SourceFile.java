package dotty.tools.dotc.interfaces;

/** A source file.
 *
 *  User code should not implement this interface, but it may have to
 *  manipulate objects of this type.
 */
public interface SourceFile extends AbstractFile {

  /**
   * @return The identifier for this file, for example, the concatenation of
   *         {@link #name()} and {@link #path()}, but may be different e.g. if
   *         this is a virtual file. Instances that do not use reference equality
   *         should base equality on this method.
   */
  String id();

  /** @return The content of this file as seen by the compiler. */
  char[] content();
}
