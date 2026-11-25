package ammonite.compiler

import ammonite.util.Util._
import ammonite.util.{Classpath, ImportTree, Printer}
import ammonite.util.Util.ClassFiles

import java.nio.file.Path

import dotty.tools.dotc
import dotc.{Compiler => DottyCompiler}
import dotc.core.Contexts.FreshContext
import scala.collection.mutable

/**
 * Wraps up the `Compiler` and `Pressy`, ensuring that they get properly
 * initialized before use. Mostly deals with ensuring the object lifecycles
 * are properly dealt with; `Compiler` and `Pressy` are the ones which deal
 * with the compiler's nasty APIs
 *
 * Exposes a simple API where you can just call methods like `compilerClass`
 * `configureCompiler` any-how and not worry about ensuring the necessary
 * compiler objects are initialized, or worry about initializing them more
 * than necessary
 */
class CompilerLifecycleManager(
    headFrame: => ammonite.util.Frame,
    dependencyCompleteOpt: => Option[String => (Int, Seq[String])],
    classPathWhitelist: Set[Seq[String]],
    initialClassLoader: ClassLoader,
    val outputDir: Option[Path],
    initialSettings: Seq[String]
) extends ammonite.compiler.iface.CompilerLifecycleManager {

  def scalaVersion = dotc.config.Properties.versionNumberString

  def forceInit(): Unit = init(force = true)
  def init(): Unit = init(force = false)

  private[this] object Internal {
    outputDir.foreach(java.nio.file.Files.createDirectories(_))
    val dynamicClasspath = Compiler.directory(outputDir.getOrElse(CompilerUtil.tempDir()))
    var compiler: ammonite.compiler.Compiler = null
    val onCompilerInit = mutable.Buffer.empty[DottyCompiler => Unit]
    val onSettingsInit = mutable.Buffer.empty[FreshContext => Unit] // TODO Pass a SettingsState too
    var preConfiguredSettingsChanged: Boolean = false
    val addedSettings = mutable.Buffer.empty[String]
    var compilationCount = 0
    var (lastFrame, lastFrameVersion) = (headFrame, headFrame.version)
  }

  import Internal._

  // Public to expose it in the REPL so people can poke at it at runtime
  // Not for use within Ammonite! Use one of the other methods to ensure
  // that `Internal.compiler` is properly initialized before use.
  def compiler: ammonite.compiler.Compiler = Internal.compiler
  def compilationCount = Internal.compilationCount

  // def pressy: Pressy = Internal.pressy

  def preprocess(fileName: String) = synchronized {
    init()
    compiler.preprocessor(fileName)
  }

  // We lazily force the compiler to be re-initialized by setting the
  // compilerStale flag. Otherwise, if we re-initialized the compiler eagerly,
  // we end up sometimes re-initializing it multiple times unnecessarily before
  // it gets even used once. Empirically, this cuts down the number of compiler
  // re-initializations by about 2/3, each of which costs about 30ms and
  // probably creates a pile of garbage

  def init(force: Boolean = false) = synchronized {
    if (
      compiler == null ||
      (headFrame ne lastFrame) ||
      headFrame.version != lastFrameVersion ||
      Internal.preConfiguredSettingsChanged ||
      force
    ) {

      lastFrame = headFrame
      lastFrameVersion = headFrame.version

      val initialClassPath = Classpath.classpath(initialClassLoader)
      val headFrameClassPath =
        Classpath.classpath(headFrame.classloader)

      Internal.compiler = new Compiler(
        Internal.dynamicClasspath,
        initialClassPath,
        headFrameClassPath,
        headFrame.classloader,
        classPathWhitelist,
        dependencyCompleteOpt = dependencyCompleteOpt,
        contextInit = c => onSettingsInit.foreach(_(c)),
        settings = initialSettings ++ addedSettings
      )
      onCompilerInit.foreach(_(compiler.compiler))

      Internal.preConfiguredSettingsChanged = false
    }
  }

  def complete(
      offset: Int,
      previousImports: String,
      snippet: String
  ): (Int, Seq[String], Seq[String]) = synchronized {
    init()
    Internal.compiler.complete(offset, previousImports, snippet)
  }

  def compileClass(
      processed: ammonite.compiler.iface.Preprocessor.Output,
      printer: Printer,
      fileName: String
  ): Option[ammonite.compiler.iface.Compiler.Output] = synchronized {
    init()
    val compiled = compiler.compile(
      processed.code.getBytes(scala.util.Properties.sourceEncoding),
      printer,
      processed.prefixCharLength,
      processed.userCodeNestingLevel,
      fileName,
      processed.suffixCharLength
    )
    Internal.compilationCount += 1
    compiled
  }

  def configureCompiler(callback: DottyCompiler => Unit) = synchronized {
    onCompilerInit.append(callback)
    if (compiler != null) {
      callback(compiler.compiler)
    }
  }

  def preConfigureCompiler(callback: FreshContext => Unit) =
    synchronized {
      onSettingsInit.append(callback)
      preConfiguredSettingsChanged = true
    }

  override def checkScalacOptions(
      options: Seq[String]
  ): Either[String, (Seq[String], Seq[String])] =
    if (options.isEmpty) Right((Nil, Nil))
    else {
      val group = CompilerLifecycleManager.settingGroup
      val summary = group.processArguments(options.toList, true, group.defaultState)
      if (summary.errors.isEmpty) {
        // Like the compiler, we only warn about unknown options, and ignore them. The other
        // warnings (like those about deprecated aliases) are left to the compiler.
        val unknown = summary.warnings.filter(_.startsWith("bad option"))
        val ignored = unknown.flatMap(CompilerLifecycleManager.badOption.findFirstMatchIn(_))
          .map(_.group(1))
          .toSet
        Right((options.filterNot(ignored), unknown))
      } else
        Left(summary.errors.mkString(newLine))
    }

  def addScalacOptions(options: Seq[String]): Unit =
    synchronized {
      addedSettings ++= options
      preConfiguredSettingsChanged = true
    }

  def addToClasspath(classFiles: ClassFiles): Unit = synchronized {
    Compiler.addToClasspath(classFiles, dynamicClasspath, outputDir)
  }
  def shutdownPressy() = () // N/A in Scala 3
}

object CompilerLifecycleManager {

  /**
   * The compiler settings, without any state (that lives in the `SettingsState`-s passed along)
   *
   * Before Scala 3.9, `ScalaSettings` is a concrete class that we instantiate. Later on, it's abstract,
   * and its companion object, that extends it, holds the settings.
   */
  private lazy val settingGroup: dotc.config.Settings.SettingGroup = {
    val cls = classOf[dotc.config.ScalaSettings]
    Class.forName(cls.getName + "$", true, cls.getClassLoader)
      .getField("MODULE$")
      .get(null) match {
      case group: dotc.config.Settings.SettingGroup => group
      case _ => cls.getConstructor().newInstance()
    }
  }

  /** Extracts the option from warnings like "bad option '-Xfoo' was ignored" */
  private val badOption = "bad option '([^']*)'".r
}
