---
layout: doc-page
title: "Changes in Scripting"
nightlyOf: https://docs.scala-lang.org/scala3/reference/changed-features/scripting.html
---

start = `^(::)?#!.*`
end = `^(::)?!#.*(\r|\n|\r\n)`

The motivation for the `@main` functions is to make Scala scripting friendly. So far we do not plan to support something more complex than the above – we believe if a user needs a complex command line parsing capability, they can always fall back to the conventional `def main(args: Array[String])` syntax plus a dedicated library like [scopt](https://github.com/scopt/scopt). The changes described above, however, are already enough to make script development much less tedious than before.

To learn more, see the [documentation](https://dotty.epfl.ch/docs/reference/changed-features/main-functions.html).

### From Blog
[PR #10491](https://github.com/lampepfl/dotty/pull/10491) introduced scripting support in Scala 3. Consider the following source named `Main.scala`:

```scala
@main def Test(name: String): Unit =
  println(s"Hello ${name}!")
```

If you have Scala 3 binaries on your path (which you can get by following the steps on the [Dotty website](https://dotty.epfl.ch/), in the section "Try Dotty"), you can run the following command:

    $ scala Main.scala World

This will compile the source in question to a temporary directory and run the discovered main method with the argument `World`.

Note the difference from the Scala 2 scripting implementation. In Scala 2, we do not require the users to have a `main` method in their scripts due to it being too cumbersome to write. In Scala 3, thanks to the top-level definitions and the `@main` annotations, `main` methods are one-liners and hence are more suited for scripts.

The documentation for this feature is available [here](https://dotty.epfl.ch/docs/usage/getting-started.html#scala-3-for-scripting).


### Scala 3 for Scripting
If you have followed the steps in "Standalone Installation" section and have the `scala` executable on your `PATH`, you can run `*.scala` files as scripts. Given a source named Test.scala:

```scala
@main def Test(name: String): Unit =
  println(s"Hello ${name}!")
```

You can run: `scala Test.scala World` to get an output `Hello World!`.

A "script" is an ordinary Scala file which contains a main method. The semantics of the `scala Script.scala` command is as follows:

- Compile `Script.scala` with `scalac` into a temporary directory.
- Detect the main method in the `*.class` files produced by the compilation.
- Execute the main method.

```scala
// SourceFile.scala

/** Return true if file is a script:
   *  if filename extension is not .scala and has a script header.
   */
  def isScript(file: AbstractFile | Null, content: Array[Char]): Boolean =
    ScriptSourceFile.hasScriptHeader(content)
```
