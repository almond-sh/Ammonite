package ammonite.repl.bsp

import java.io.File
import java.net.URL
import java.nio.file.{Files, Path, Paths}
import java.util.zip.ZipFile

import ammonite.runtime.{Frame, FrameOutputs}
import ch.epfl.scala.bsp4j._

import scala.collection.JavaConverters._
import scala.util.control.NonFatal

/**
 * What a BSP server needs to know about a REPL session
 *
 * Each frame of the session is a build target, depending on the target of its parent frame.
 *
 * @param liveFrames the frames still reachable from the session
 * @param outputs where the sources and byte code of each frame are written, all frames
 *                should be registered there
 * @param scalaVersion the Scala version of the session
 * @param scalacOptions the options passed to the compiler, other than class path related ones
 * @param baseClasspath the class path the root frame starts with
 */
final class ReplBspSession(
    liveFrames: () => Seq[Frame],
    val outputs: FrameOutputs,
    val scalaVersion: String,
    val scalacOptions: Seq[String],
    val baseClasspath: Seq[URL]
) {
  import ReplBspSession._

  def frames: Seq[Frame] = liveFrames()

  def targetId(frame: Frame): BuildTargetIdentifier =
    new BuildTargetIdentifier(uri(outputs.frameDirectory(frame).toNIO))

  /** The live frames corresponding to `ids`, ignoring unknown ids */
  def framesFor(
      ids: java.util.List[BuildTargetIdentifier]
  ): Seq[(BuildTargetIdentifier, Frame)] = {
    val byId = frames.map(f => targetId(f) -> f).toMap
    ids.asScala.toVector.flatMap(id => byId.get(id).map(id -> _))
  }

  def buildTarget(frame: Frame): BuildTarget = {
    val capabilities = new BuildTargetCapabilities
    // Everything is compiled already, compile requests are no-ops
    capabilities.setCanCompile(true)
    capabilities.setCanTest(false)
    capabilities.setCanRun(false)
    capabilities.setCanDebug(false)
    val target = new BuildTarget(
      targetId(frame),
      List.empty[String].asJava,
      List("scala").asJava,
      frame.parent.map(targetId).toList.asJava,
      capabilities
    )
    target.setDisplayName(outputs.frameDirectory(frame).last)
    target.setBaseDirectory(uri(outputs.frameDirectory(frame).toNIO))
    target.setDataKind(BuildTargetDataKind.SCALA)
    target.setData(scalaBuildTarget)
    target
  }

  private lazy val scalaBuildTarget: ScalaBuildTarget = {
    val jvmTarget = new JvmBuildTarget
    jvmTarget.setJavaHome(uri(Paths.get(sys.props("java.home"))))
    jvmTarget.setJavaVersion(sys.props("java.version"))
    val target = new ScalaBuildTarget(
      "org.scala-lang",
      scalaVersion,
      scalaBinaryVersion(scalaVersion),
      ScalaPlatform.JVM,
      scalaJars(baseClasspath).asJava
    )
    target.setJvmBuildTarget(jvmTarget)
    target
  }

  /** The class directory of a frame, then the ones of its parents */
  private def classDirectories(frame: Frame): Seq[Path] =
    Iterator.iterate(Option(frame))(_.flatMap(_.parent))
      .takeWhile(_.nonEmpty)
      .flatten
      .map(outputs.classesDirectory(_).toNIO)
      .toVector

  def classpath(frame: Frame): Seq[String] =
    (classDirectories(frame).map(uri) ++
      (frame.classpath ++ baseClasspath).map(_.toURI.toASCIIString)).distinct

  def classDirectory(frame: Frame): String =
    uri(outputs.classesDirectory(frame).toNIO)

  def frameScalacOptions(frame: Frame): Seq[String] =
    scalacOptions ++ pluginOptions(frame.pluginClasspath)

  def sources(frame: Frame): Seq[FrameOutputs.Source] =
    outputs.sources(frame)

  def dependencySources(frame: Frame): Seq[String] =
    (frame.classpath ++ baseClasspath)
      .flatMap(sourceJar)
      .distinct
      .map(uri)
}

object ReplBspSession {

  /** URI of a file or directory, without a trailing slash */
  def uri(path: Path): String =
    path.toAbsolutePath.normalize.toUri.toASCIIString.stripSuffix("/")

  def scalaBinaryVersion(scalaVersion: String): String =
    if (scalaVersion.startsWith("3.")) "3"
    else scalaVersion.split('.').take(2).mkString(".")

  private def fileName(url: URL): Option[String] =
    if (url.getProtocol == "file") Some(url.getPath.split('/').last)
    else None

  private val scalaJarPrefixes = Seq(
    "scala-compiler-",
    "scala-library-",
    "scala-reflect-",
    "scala3-compiler_3-",
    "scala3-library_3-",
    "scala3-interfaces-",
    "tasty-core_3-"
  )

  /** The JARs of the Scala compiler and its dependencies, among `classpath` */
  def scalaJars(classpath: Seq[URL]): Seq[String] =
    classpath
      .filter(url => fileName(url).exists(name => scalaJarPrefixes.exists(name.startsWith)))
      .map(_.toURI.toASCIIString)

  /**
   * The source JAR of a JAR, if it's either a source JAR itself, or if a source JAR with the
   * same name is found alongside it (like in the coursier cache, if it fetched it)
   */
  def sourceJar(url: URL): Option[Path] =
    if (url.getProtocol != "file") None
    else {
      val path = Paths.get(url.toURI)
      val name = path.getFileName.toString
      if (name.endsWith("-sources.jar")) Some(path)
      else if (name.endsWith(".jar"))
        Some(path.resolveSibling(name.stripSuffix(".jar") + "-sources.jar"))
          .filter(Files.isRegularFile(_))
      else None
    }

  private def isCompilerPlugin(path: Path): Boolean =
    Files.isRegularFile(path) && {
      var zf: ZipFile = null
      try {
        zf = new ZipFile(path.toFile)
        // the descriptors of Scala 2 and Scala 3 compiler plugins
        zf.getEntry("scalac-plugin.xml") != null || zf.getEntry("plugin.properties") != null
      } catch {
        case NonFatal(_) => false
      } finally {
        if (zf != null) zf.close()
      }
    }

  /**
   * Compiler options loading the plugins among `pluginClasspath`
   *
   * Like Ammonite does, we load each plugin along with all the JARs of `pluginClasspath`, that
   * are typically the plugin dependencies.
   */
  def pluginOptions(pluginClasspath: Seq[URL]): Seq[String] = {
    val paths = pluginClasspath
      .filter(_.getProtocol == "file")
      .map(url => Paths.get(url.toURI))
      .distinct
    val (plugins, others) = paths.partition(isCompilerPlugin)
    plugins.map { plugin =>
      "-Xplugin:" + (plugin +: others).map(_.toString).mkString(File.pathSeparator)
    }
  }
}
