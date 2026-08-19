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
  `scala3-compiler`), and the only one cross-published for full Scala versions

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

Ammonite supports many Scala versions, and `amm/compiler` is compiled against
compiler internals (`scala-compiler` for Scala 2, `scala3-compiler` for Scala 3)
which change from one patch release to the next - see the many
`amm/compiler/src/main/scala-<version-range>/` source directories. It is
therefore *fully* cross-published, once per supported Scala version:

```
ammonite-compiler_2.12.8, …, ammonite-compiler_2.12.21,
ammonite-compiler_2.13.3, …, ammonite-compiler_2.13.18,
ammonite-compiler_3.3.8, ammonite-compiler_3.8.4
```

Every other module only uses the abstractions in
`ammonite-compiler-interface` (`Compiler`, `CompilerBuilder`, `Parser`,
`Preprocessor`, `CodeWrapper`), never compiler internals, so they are published
per *binary* Scala version instead - `ammonite_3`, `ammonite-repl_2.13`,
`ammonite-util_2.12`, … This keeps the number of modules we publish to Maven
Central down: a newly supported Scala version costs one module rather than
eight, and the whole project publishes 47 modules rather than 262.

Two consequences:

- **Users depend on two artifacts**, the binary-versioned entry point and the
  compiler matching the exact Scala version they run with:

  ```
  sh.almond.ammonite::ammonite:<version>
  sh.almond.ammonite:ammonite-compiler_<full-scala-version>:<version>
  ```

  `ammonite` deliberately does *not* depend on `ammonite-compiler` - it would
  have to pin one Scala version for all users of a binary version. It finds the
  compiler at run time instead, via
  `ammonite.compiler.iface.CompilerBuilderFactory`, a `java.util.ServiceLoader`
  service that `ammonite-compiler` registers in
  `META-INF/services/`. Missing it is reported as
  `CompilerBuilderFactory.NoCompilerException`.

- **Binary cross-published modules are built with the oldest Scala version we
  support for their binary version** (`binCrossScalaVersions` in `build.mill`).
  Scala 2 patch releases are only forward binary compatible, so
  `ammonite-repl_2.13` has to be built with 2.13.3 rather than 2.13.18 to be
  usable across the whole 2.13 range we support. For Scala 3 the LTS plays that
  role, its TASTy files being readable by later Scala 3 versions.

Note that `amm`, `amm/repl` and `amm/compiler` are still *built* for every
supported Scala version, even though the first two are only published per binary
version: their test suites and the `amm` assemblies need to run against each
compiler. `isPublishedCrossInstance` in `build.mill` picks the instances we
actually publish.

`./mill show publishedArtifacts` lists what a release would push to Maven
Central, and fails if two module instances would collide on one artifact id.
