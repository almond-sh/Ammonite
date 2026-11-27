package ammonite.debugger

import java.io.File
import java.net.{URI, URLClassLoader}
import java.util.Properties

import scala.jdk.CollectionConverters._

/**
 * scala-debug-adapter, running in the JVM of the session (see
 * `ammonite.debugadapter.InProcessDebugAdapter`, in the ammonite-debug-adapter module)
 *
 * It's loaded in a class loader of its own, with only JDI shared with the session, so that its
 * dependencies don't clash with those of the session, and its Scala version doesn't need to be
 * the one of the session. We talk to it via reflection, with Java types only.
 */
final class DebugAdapter private (instance: AnyRef) {

  private def method(name: String, params: Class[_]*) =
    instance.getClass.getMethod(name, params: _*)

  /** Where its DAP server listens */
  val uri: URI = new URI(method("uri").invoke(instance).asInstanceOf[String])

  /** Tells the adapter about classes compiled since the session started */
  def classesUpdated(classNames: Seq[String]): Unit =
    method("classesUpdated", classOf[Array[String]]).invoke(instance, classNames.toArray)

  /**
   * Tells the adapter that the line numbers of the TASTy files of classes are those of their
   * byte code plus `shift`
   */
  def lineShift(classNames: Seq[String], shift: Int): Unit =
    method("lineShift", classOf[Array[String]], classOf[Int])
      .invoke(instance, classNames.toArray, Int.box(shift))

  def stop(): Unit =
    method("close").invoke(instance)
}

object DebugAdapter {

  def className = "ammonite.debugadapter.InProcessDebugAdapter"

  /** Organization and name of the module of the debug adapter */
  def organization = "sh.almond.ammonite"
  def moduleName = "ammonite-debug-adapter_2.13"

  /**
   * Environment variable with the class path of the debug adapter, used rather than fetching it
   */
  def classPathEnvVar = "AMMONITE_DEBUG_ADAPTER_CLASS_PATH"

  /** Where snapshot versions of Ammonite are published */
  def snapshotsRepository: coursierapi.Repository =
    coursierapi.MavenRepository.of("https://central.sonatype.com/repository/maven-snapshots")

  /**
   * The class path of the debug adapter: fetched with coursier, from `repositories` and the
   * default repositories of coursier (that are taken from `COURSIER_REPOSITORIES` if it's set),
   * along with the snapshot repository of Maven Central for snapshot versions
   *
   * @param version the Ammonite version of the debug adapter
   */
  def classPath(version: String, repositories: Seq[coursierapi.Repository]): Seq[File] =
    Option(System.getenv(classPathEnvVar)).map(_.trim).filter(_.nonEmpty) match {
      case Some(cp) =>
        cp.split(File.pathSeparator).toSeq.filter(_.nonEmpty).map(new File(_))
      case None =>
        val snapshots =
          if (version.endsWith("-SNAPSHOT")) Seq(snapshotsRepository)
          else Nil
        coursierapi.Fetch.create()
          .addDependencies(coursierapi.Dependency.of(organization, moduleName, version))
          .addRepositories(repositories ++ snapshots: _*)
          .fetch()
          .asScala
          .toVector
    }

  /** Only shares JDI with the class loaders it's the parent of */
  private lazy val jdiLoader: ClassLoader = {
    val jdiClassLoader = Class.forName("com.sun.jdi.Bootstrap").getClassLoader
    new ClassLoader(null) {
      override def loadClass(name: String, resolve: Boolean): Class[_] =
        if (name.startsWith("com.sun.jdi.") || name.startsWith("com.sun.tools.jdi.")) {
          val cls = Class.forName(name, false, jdiClassLoader)
          if (resolve) resolveClass(cls)
          cls
        } else super.loadClass(name, resolve)
    }
  }

  /** Whether JDI is available (the JVM of the session needs to be a JDK, not a JRE) */
  def jdiAvailable: Boolean =
    try { jdiLoader; true }
    catch { case _: ClassNotFoundException | _: LinkageError => false }

  /**
   * The class loader of the debug adapter, kept across debug sessions, as the adapter keeps its
   * JDWP connection there
   */
  private var loaderCache = Option.empty[(Seq[File], URLClassLoader)]
  private def adapterLoader(classPath: Seq[File]): URLClassLoader = synchronized {
    loaderCache match {
      case Some((cp, loader)) if cp == classPath => loader
      case _ =>
        val loader = new URLClassLoader(classPath.map(_.toURI.toURL).toArray, jdiLoader)
        loaderCache = Some((classPath, loader))
        loader
    }
  }

  /**
   * Starts the debug adapter
   *
   * @param config see `ammonite.debugadapter.InProcessDebugAdapter.start`
   * @param suspendable names of the threads that can be suspended when the adapter wants to
   *                    suspend all threads
   */
  def start(
      classPath: Seq[File],
      config: Map[String, String],
      suspendable: String => Boolean,
      log: Logger
  ): DebugAdapter = {
    val loader = adapterLoader(classPath)
    val cls = loader.loadClass(className)
    val props = new Properties
    for ((k, v) <- config)
      props.setProperty(k, v)
    val suspendable0: java.util.function.Predicate[String] =
      new java.util.function.Predicate[String] {
        def test(name: String): Boolean = suspendable(name)
      }
    val logger: java.util.function.Consumer[String] =
      new java.util.function.Consumer[String] {
        def accept(msg: String): Unit = {
          val (level, rest) = msg.span(_ != ' ')
          val msg0 = s"[debug-adapter] ${rest.trim}"
          level match {
            case "error" => log.error(msg0)
            case "warn" => log.warn(msg0)
            case "info" => log.info(msg0)
            case _ => log.debug(msg0)
          }
        }
      }
    val instance = cls
      .getMethod(
        "start",
        classOf[Properties],
        classOf[java.util.function.Predicate[_]],
        classOf[java.util.function.Consumer[_]]
      )
      .invoke(null, props, suspendable0, logger)
    new DebugAdapter(instance.asInstanceOf[AnyRef])
  }
}
