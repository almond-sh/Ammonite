Layering
========

The Ammonite codebase is laid out in the following modules, with arrows
representing dependencies:

```
                 +------------- terminal <---+
                 |                           |
 amm/util <------+--- amm/repl/api <---------+-- amm/repl <--- amm <--- shell
      ^                    ^                                    |
      |                    |                                    |
      +--- amm/compiler ---+                                    |
                    ^                                           |
                    +---------- (ServiceLoader, at run time) ---+
```

`amm` is the entry point for the "main" Ammonite REPL. Internally it is
modularized into submodules to help maintain the layering, and e.g. avoid
accidental use of unnecessary APIs (and paying their classloading/initialization
cost) in the core codepaths.

- `amm/util`: basic data-structures and logic common throughout the codebase,
  along with `ammonite.compiler.iface`, the abstract compiler API
  (`Compiler`, `CompilerBuilder`, `Parser`, `Preprocessor`, `CodeWrapper`)
  that lets everything else stay independent of a concrete Scala compiler

- `amm/repl/api`: the APIs user code sees from within the REPL and from
  scripts - the rich `ReplAPI`, and the core `InterpAPI` scripts call

- `amm/compiler`: preprocesses Scala source code and compiles it to bytecode.
  The only module written against compiler internals (`scala-compiler` /
  `scala3-compiler`). Two unpublished modules sit under it, and their classes
  are bundled in its JAR:

  - `amm/compiler/interfaces`: the traits that stand for the compiler internals
    that move between patch releases (`ReporterMaker`, `GlobalMaker`,
    `ClassPathMaker`, `SymbolOps` for Scala 2; `CompilationUnitMaker`,
    `GivenNames`, `CompletionMaker` for Scala 3), and `InternalsLoader`, which
    picks an implementation of one at run time

  - `amm/compiler/internal`: those implementations, one per interval of Scala
    versions, each compiled against the oldest compiler of its interval

- `amm/repl`: everything necessary to run an Ammonite REPL that takes in stdin
  and prints to stdout; includes JLine, `ammonite-terminal`, REPL-specific
  `ReplAPI`s, and other code specific to interactive REPL work. Also holds

  - `ammonite.runtime`: everything necessary to run an Ammonite Scala Script
    that has already been compiled and cached. This code is the "critical path"
    for using Ammonite to run slow-changing scripts (i.e. most of them) and
    should be fast and without heavy dependencies like `scala-compiler`.

  - `ammonite.interp`: everything necessary to run an Ammonite Scala Script
    that has *not* been compiled and cached; drives the compiler through the
    `ammonite.compiler.iface` abstractions rather than depending on it

- `amm`: contains the Ammonite's main entry-points: for the REPL,
  script-runner, debugger (same as REPL), etc. and associated code for
  marshalling command-line script arguments into the Ammonite's main methods.

Note that `amm` does not depend on `amm/compiler` - it picks a compiler up at
run time, see "Cross-publishing" below.

There are many classes involved in the Ammonite REPL that can conceivably be
thought of as "the thing which runs your code". This diagram roughly breaks
down the relationship between these classes:

```
amm:                        Main
                             |  \
                             |   \
                             |    \
                             |     v
amm/repl                     |    Repl ------------
  (ammonite.repl)            |     /               |
                             |    /                v
                             |   /                FrontEnd
                             v  v
  (ammonite.interp)         Interpreter ----------------------------------
                             |               |              |             |
                             |               v              v             v
                             |         (amm/compiler, reached through ammonite.compiler.iface)
                             |              Compiler       Pressy        Preprocessor
                             v
  (ammonite.runtime)        Evaluator
```

The distribution of responsibilities is

- `Evaluator`: runs Java bytecode

  Manages classloaders, caching, etc. to make that happen

- `Interpreter`: runs Scala source code

  Made up of `Evaluator` + `Compiler` (and `Pressy`). Runs source code by
  transforming it via `Preprocessor`, compiling it to bytecode via `Compiler`
  and sending it to `Evaluator` to execute

- `Repl`: runs user-input

  Made up of `Interpreter` + `FrontEnd`, handles the full pipeline from taking
  user input at the command prompt to executing it

- `Main`: a nicer API/CLI around `Repl`

  Provides the nice external-API and CLI in a separate place from all the
  messy `Repl` internals


This is the ideal layering that we want to achieve. It's likely that the
current implementation does not entirely line up with this, and there is code
living in places it shouldn't, but over time we should try to move it to this
layering.
Cross-publishing
================

Every module is published once per *binary* Scala version - `ammonite_3`,
`ammonite-repl_2.13`, `ammonite-compiler_2.12`, … A newly supported Scala
version costs no new module at all, and the whole project publishes 47 of them
rather than the 262 it once did.

That is not free for `amm/compiler`, the one module compiled against compiler
internals (`scala-compiler` for Scala 2, `scala3-compiler` for Scala 3), which
move from one patch release to the next. It is split in three:

- `amm/compiler` itself is built once per binary Scala version and holds
  everything that links against every compiler of that binary version

- `amm/compiler/interfaces` holds a trait per piece of the compiler that does
  move - `ReporterMaker`, `GlobalMaker`, `ClassPathMaker` and `SymbolOps` for
  Scala 2, `CompilationUnitMaker`, `GivenNames`, `CompletionMaker`,
  `MessageRenderer` and `TermRefNames` for Scala 3

- `amm/compiler/internal` implements those traits, once per interval of Scala
  versions. Its source directories are named after the interval they are for,
  and so are the classes in them, so that the implementations for a whole
  binary version can sit side by side in one JAR:

  ```
  scala-2.12.0-2.12.12/…/ReporterMaker2_12_0__2_12_12.scala
  scala-2.12.13-2.13.11/…/ReporterMaker2_12_13__2_13_11.scala
  scala-2.13.12+/…/ReporterMaker2_13_12.scala
  ```

  Each directory is compiled with the oldest full Scala version it covers, once
  per binary Scala version it covers - `compilerInternalDirs` in `build.mill`
  says which directory covers what, and everything else follows from it.

Both are unpublished, and `amm/compiler` bundles their class files in its own
JAR. At run time `ammonite.compiler.internal.CompilerInternals` - generated by
mill from `compilerInternalDirs` - reads the version of the compiler it finds on
the class path out of its `compiler.properties`, and `InternalsLoader` loads the
implementation written for it by name.

Every other module only uses the abstractions in
`ammonite-compiler-interface` (`Compiler`, `CompilerBuilder`, `Parser`,
`Preprocessor`, `CodeWrapper`), never compiler internals.

`amm` deliberately has no dependency on `amm/compiler` at all - not even a run-time
one - so that nothing pins a single Scala version for all users of a binary version.

Two consequences:

- **Users depend on two artifacts**, the entry point and the compiler:

  ```
  sh.almond.ammonite::ammonite:<version>
  sh.almond.ammonite::ammonite-compiler:<version>
  ```

  `ammonite` deliberately does *not* depend on `ammonite-compiler` - it would
  drag `scala-compiler` into every classpath that only ever runs cached scripts.
  It finds the compiler at run time instead, via
  `ammonite.compiler.iface.CompilerBuilderFactory`, a `java.util.ServiceLoader`
  service that `ammonite-compiler` registers in
  `META-INF/services/`. Missing it is reported as
  `CompilerBuilderFactory.NoCompilerException`.

- **Published modules are built with the oldest Scala version we support for
  their binary version** (`binCrossScalaVersions` in `build.mill`). Scala 2
  patch releases are only forward binary compatible, so `ammonite-repl_2.13` has
  to be built with 2.13.3 rather than 2.13.18 to be usable across the whole 2.13
  range we support. For Scala 3 the LTS plays that role, its TASTy files being
  readable by later Scala 3 versions.

Two kinds of incompatibility decide what has to go in `amm/compiler/internal`,
and neither shows up at compile time:

- **calling** a compiler member that moved - `HasFlags.isPackage` was gone in
  2.13.17, `NamedType.designator` changed its return type in 3.8.2 - which the
  older build turns into a `NoSuchMethodError`

- **extending** a compiler trait that grew a field, whose mixin setter the older
  build does not implement, which turns into an `AbstractMethodError`. This is
  what keeps `GlobalMaker` split for 2.12, where the analyzer has to be
  subclassed to reach the macro class loader. Scala 2.13 needs no analyzer of
  its own - overriding `Global.findMacroClassLoader`, as scalac's own REPL does,
  is enough - which is worth some care to keep: a subclass of `Analyzer` is
  roughly 150kB of mixin forwarders, and only ever links against the exact
  scalac it was compiled with.

The nested test crosses below are what catches both.

Only the pieces that need one specific compiler are cross-built over full Scala
versions:

- **test modules** are nested crosses over the full Scala versions of their
  module's binary version, so `amm/repl` built with 2.13.3 has its tests built
  and run with each of 2.13.3 … 2.13.18:

  ```
  ./mill 'amm.repl[2.13.3].test[2.13.18]'
  ./mill 'amm[2.13.3].test[2.13.18]'
  ```

  This also means the tests exercise the artifacts we actually publish, rather
  than a build of them made with the Scala version under test. Mill's
  `JavaTests`/`ScalaTests` assume a test module shares its module's Scala
  version and inherit `resolutionParams`, `scalaCompilerBridge`, the scalac
  plugins, `jvmId` and `javaHome` from it - those are overridden in `build.mill`
  to follow the test's own Scala version, taking them from
  `amm.compiler.internal`, the one module still built per full Scala version.

- **`amm/compiler/internal`** is cross-built over every full Scala version, but
  only the instances that are the oldest version of one of their directories
  have any source to compile.

- **`shell`** is an unpublished module cross-built over every full Scala version,
  pairing `amm` with `amm/compiler` to build the launcher and the assembly.
  `integration` tests run against it.

`./mill show publishedArtifacts` lists what a release would push to Maven
Central, and fails if two module instances would collide on one artifact id.
