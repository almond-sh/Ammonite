package ammonite.runtime

import ammonite.directives.{DirectiveValue, UsingDirective}
import ammonite.interp.Interpreter
import ammonite.util.Name
import ammonite.util.Util.CodeSource
import coursierapi.RepositoryParser

import scala.util.control.NonFatal

/**
 * Handles a kind of using directive (`//> using key value1 value2 …`), like
 * [[ImportHook]] handles a kind of magic import
 *
 * Directives are only read from the beginning of commands and scripts, before any code
 * (they can only be preceded by other directives, comments, and blank lines).
 */
trait DirectiveHook {

  /** Handles a parsed directive that this directive hook was registered for */
  def handle(
      source: CodeSource,
      directive: UsingDirective,
      interp: ImportHook.InterpreterInterface,
      wrapperPath: Seq[Name]
  ): Either[String, Seq[ImportHook.Result]]

  /**
   * Completes a value of a directive handled by this hook
   *
   * @param directory directory relative paths in values are relative to
   * @param prefix value being typed, up to the cursor
   * @return the index in `prefix` where completions start, and the completions
   */
  def complete(
      interp: Interpreter,
      directory: os.Path,
      prefix: String
  ): (Int, Seq[String]) =
    (0, Nil)
}

object DirectiveHook {

  /** The values of a directive, like the dependencies of `//> using dep a b` */
  def values(directive: UsingDirective): Either[String, Seq[String]] = {
    val values0 = directive.values.filter {
      case _: DirectiveValue.EmptyVal => false
      case _ => true
    }
    if (values0.isEmpty) Left(s"Expected a value after '//> using ${directive.key}'")
    else Right(values0.map(_.stringValue))
  }

  object Dependencies extends DirectiveHook {
    def handle(
        source: CodeSource,
        directive: UsingDirective,
        interp: ImportHook.InterpreterInterface,
        wrapperPath: Seq[Name]
    ): Either[String, Seq[ImportHook.Result]] =
      for {
        signatures <- values(directive)
        depsResolved <- ImportHook.Ivy.resolve(interp, signatures, "dependencies")
        (deps, files) = depsResolved
      } yield Seq(ImportHook.Result.ClassPath(Some(deps), files.map(os.Path(_)), plugin = false))

    override def complete(
        interp: Interpreter,
        directory: os.Path,
        prefix: String
    ): (Int, Seq[String]) =
      if (prefix.isEmpty) (0, Nil)
      else {
        val (from, completions) = interp.dependencyComplete(prefix)
        (from, sortCompletions(prefix.take(from), completions))
      }

    /**
     * Whether a dependency being typed is at its version, like `org.typelevel::cats-core:` or
     * `org.typelevel:cats-core_2.13:2.1`, rather than at its organization or module name
     */
    def isVersion(dependencyPrefix: String): Boolean = {
      val idx = dependencyPrefix.lastIndexOf(':')
      idx >= 0 && {
        // the module name is the last non-empty part before the last ':', the parts in between
        // being empty in 'org::name:' (Scala module) or 'org:::name:' (full cross-version one)
        val parts = dependencyPrefix.substring(0, idx).split(":", -1)
        parts.length >= 2 && parts.last.nonEmpty
      }
    }

    /**
     * Sorts dependency completions, and removes duplicates from them
     *
     * Versions are sorted semantically, highest first, with the coursier version ordering
     * (the one used to pick versions when resolving dependencies). Organizations and module
     * names are left as is (in the order of the repository listings, sorted alphabetically
     * for Maven Central).
     */
    def sortCompletions(dependencyPrefix: String, completions: Seq[String]): Seq[String] = {
      // several repositories may list the same organizations, module names, or versions
      val completions0 = completions.distinct
      if (isVersion(dependencyPrefix))
        completions0.sortWith((a, b) => coursierapi.Version.compare(a, b) > 0)
      else
        completions0
    }
  }

  /**
   * Repositories, like `//> using repository jitpack` or
   * `//> using repository https://repo.example.com/maven2`
   *
   * Accepts the repositories coursier accepts (`central`, `jitpack`, `sonatype:snapshots`,
   * `m2Local`, `ivy2Local`, `ivy:…` patterns, URLs of Maven repositories…).
   */
  object Repositories extends DirectiveHook {
    def handle(
        source: CodeSource,
        directive: UsingDirective,
        interp: ImportHook.InterpreterInterface,
        wrapperPath: Seq[Name]
    ): Either[String, Seq[ImportHook.Result]] =
      values(directive).flatMap { inputs =>
        val results = inputs.map { input =>
          try Right(ImportHook.Result.Repo(RepositoryParser.repository(input)))
          catch {
            // RepositoryParsingError is the declared error, but malformed URLs actually
            // end up as IllegalArgumentException-s
            case e @ (_: coursierapi.error.RepositoryParsingError |
                _: IllegalArgumentException) =>
              Left(s"Error parsing repository '$input': ${e.getMessage}")
          }
        }
        results.collectFirst { case Left(err) => err }.toLeft(results.collect {
          case Right(r) => r
        })
      }
  }

  /** Compiler options, like `//> using option -deprecation` or `//> using options -Xasync` */
  object ScalacOptions extends DirectiveHook {
    def handle(
        source: CodeSource,
        directive: UsingDirective,
        interp: ImportHook.InterpreterInterface,
        wrapperPath: Seq[Name]
    ): Either[String, Seq[ImportHook.Result]] =
      values(directive).map(options => Seq(ImportHook.Result.ScalacOptions(options)))
  }

  /**
   * Scripts, like `//> using script path/to/script.sc`, loaded like `import $file` does
   *
   * Paths are relative to the directory of the script holding the directive (or to the
   * working directory, for REPL commands). The `.sc` extension can be omitted. The wrapper
   * object of each script is brought in scope, under the name of the script file (`script`
   * for `script.sc`).
   */
  object Scripts extends DirectiveHook {
    def handle(
        source: CodeSource,
        directive: UsingDirective,
        interp: ImportHook.InterpreterInterface,
        wrapperPath: Seq[Name]
    ): Either[String, Seq[ImportHook.Result]] =
      source.path.map(os.Path(_)) match {
        case None => Left("Cannot load scripts from code without source")
        case Some(currentScriptPath) =>
          values(directive).flatMap { inputs =>
            val maybeFiles = inputs.map { input =>
              try {
                val path = os.Path(input, currentScriptPath / os.up)
                Right(if (path.last.endsWith(".sc")) path else path / os.up / (path.last + ".sc"))
              } catch {
                case NonFatal(e) => Left(s"Malformed script path '$input': ${e.getMessage}")
              }
            }
            maybeFiles.collectFirst { case Left(err) => err } match {
              case Some(err) => Left(err)
              case None =>
                val files = maybeFiles.collect { case Right(f) => f }
                files.foreach(interp.watch)
                val missing = files.filter(!os.isFile(_))
                if (missing.nonEmpty)
                  Left(
                    (if (missing.length == 1) "Script not found: " else "Scripts not found: ") +
                      missing.mkString(", ")
                  )
                else
                  Right(
                    files.map { file =>
                      ImportHook.SourceHook.result(
                        source,
                        currentScriptPath,
                        file,
                        file.last.stripSuffix(".sc"),
                        exec = false,
                        wrapperPath
                      )
                    }
                  )
            }
          }
      }

    /** Completes paths of scripts and of the directories that may hold some */
    override def complete(
        interp: Interpreter,
        directory: os.Path,
        prefix: String
    ): (Int, Seq[String]) = {
      val idx = prefix.lastIndexOf('/')
      val (dirPart, namePrefix) =
        if (idx < 0) ("", prefix)
        else (prefix.substring(0, idx + 1), prefix.substring(idx + 1))
      val dirOpt =
        try Some(if (dirPart.isEmpty) directory else os.Path(dirPart, directory))
        catch { case NonFatal(_) => None }
      val completions = dirOpt.filter(os.isDir(_)).toSeq.flatMap { dir =>
        val entries =
          try os.list(dir)
          catch { case NonFatal(_) => Nil }
        entries
          .filter(_.last.startsWith(namePrefix))
          .filter(p => !p.last.startsWith(".") || namePrefix.startsWith("."))
          .flatMap { p =>
            if (os.isDir(p)) Seq(p.last + "/")
            else if (p.last.endsWith(".sc")) Seq(p.last)
            else Nil
          }
          .sorted
      }
      (dirPart.length, completions)
    }
  }

  /** The directive hooks Ammonite uses by default, by directive key */
  val defaults: Map[String, DirectiveHook] = {
    // Scala CLI's names (most directives accept both singular and plural keys)
    val hooks = Seq(
      Seq("dep", "deps", "dependency", "dependencies", "lib", "libs") -> Dependencies,
      Seq("repository", "repositories") -> Repositories,
      Seq(
        "option",
        "options",
        "scalacOption",
        "scalacOptions",
        "scalac-option",
        "scalac-options"
      ) -> ScalacOptions,
      Seq("script", "scripts") -> Scripts
    )
    hooks.flatMap { case (keys, hook) => keys.map(_ -> hook) }.toMap
  }
}
