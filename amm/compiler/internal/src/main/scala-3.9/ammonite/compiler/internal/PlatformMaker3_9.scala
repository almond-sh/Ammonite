package ammonite.compiler.internal

import java.net.URL
import java.nio.file.{Files, Paths}

import dotty.ammonite.compiler.{UrlJarClassPath3_9, WhiteListClassPath3_9}
import dotty.tools.dotc.classpath
import dotty.tools.dotc.config.{JavaPlatform, Platform}
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.io.{AbstractFile, ClassPath}

/** The [[PlatformMaker]] for the compilers of 3.9.x. */
class PlatformMaker3_9 extends PlatformMaker {

  /** The JDK classes, read from the modules of the JVM we run on (Java >= 9 only) */
  private lazy val jrtClassPath: Seq[ClassPath] =
    ammonite.util.Classpath.jrtFileSystem.map(new classpath.JrtClassPath(_)).toSeq

  def platform(
      initialClassPath: Seq[URL],
      classPath: Seq[URL],
      dynamicClassPath: AbstractFile,
      whiteList: Set[Seq[String]]
  ): Platform = {
    val userClassPath = classPath
    new JavaPlatform {
      private var classPath0: ClassPath = null
      override def classPath(using Context): ClassPath = {
        if (classPath0 == null)
          classPath0 = classpath.AggregateClassPath(Seq(
            asDottyClassPath(initialClassPath, whiteList = Some(whiteList)),
            // Like everything in initialClassPath, which userClassPath lists too, the JDK
            // classes aren't white-listed
            asDottyClassPath(userClassPath, jrtClassPath),
            classpath.ClassPathFactory.newClassPath(dynamicClassPath)
          ))
        classPath0
      }
    }
  }

  private def asDottyClassPath(
      cp: Seq[URL],
      extra: Seq[ClassPath] = Nil,
      whiteList: Option[Set[Seq[String]]] = None
  )(using Context): ClassPath = {
    val (dirs, jars) = cp.partition { url =>
      url.getProtocol == "file" && Files.isDirectory(Paths.get(url.toURI))
    }

    val dirsCp = dirs.map(u => classpath.ClassPathFactory.newClassPath(AbstractFile.getURL(u)))
    val jarsCp = jars
      .filter(ammonite.util.Classpath.canBeOpenedAsJar)
      .map { u =>
        if (u.getProtocol == "file")
          classpath.ZipAndJarClassPathFactory.create(AbstractFile.getURL(u))
        else
          // dotty only reads JARs from the file system, we read the others ourselves,
          // like the JARs that coursier standalone launchers embed
          new UrlJarClassPath3_9(u, UrlJarFiles.root(UrlJarArchive.root(u)))
      }

    whiteList match {
      case Some(list) => new WhiteListClassPath3_9(dirsCp ++ jarsCp ++ extra, list)
      case None => classpath.AggregateClassPath(dirsCp ++ jarsCp ++ extra)
    }
  }
}
