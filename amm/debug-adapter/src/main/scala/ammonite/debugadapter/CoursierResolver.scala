package ammonite.debugadapter

import ch.epfl.scala.debugadapter.{BuildInfo, DebugToolsResolver, Logger, ScalaVersion}

import java.net.URLClassLoader
import java.nio.file.Path

import scala.collection.concurrent.TrieMap
import scala.jdk.CollectionConverters._
import scala.util.Try

/** Fetches the expression compiler and the Scala 3 decoder of scala-debug-adapter with coursier */
final class CoursierResolver(repositories: Seq[String], logger: Logger)
    extends DebugToolsResolver {

  private val cache = TrieMap.empty[Seq[coursierapi.Dependency], Seq[Path]]

  // Parsed when first needed, so that parsing errors surface in the Try of the resolve methods
  private lazy val repos =
    coursierapi.RepositoryParser.repositories(repositories.asJava).asScala.toVector

  private def fetch(deps: coursierapi.Dependency*): Seq[Path] =
    cache.getOrElseUpdate(
      deps, {
        logger.info(s"Fetching ${deps.mkString(", ")}")
        val fetch = coursierapi.Fetch.create()
          .withDependencies(deps: _*)
        val fetch0 =
          if (repos.isEmpty) fetch
          else fetch.withRepositories(repos: _*)
        fetch0.fetch().asScala.toVector.map(_.toPath)
      }
    )

  def resolveExpressionCompiler(scalaVersion: ScalaVersion): Try[ClassLoader] =
    Try {
      val deps =
        // Since Scala 3.7, the expression compiler is part of the compiler itself
        if (scalaVersion.isScala3 && scalaVersion.minor >= 7)
          Seq(coursierapi.Dependency.of("org.scala-lang", "scala3-compiler_3", scalaVersion.value))
        else
          Seq(
            coursierapi.Dependency.of(
              BuildInfo.organization,
              s"${BuildInfo.expressionCompilerName}_${scalaVersion.value}",
              BuildInfo.version
            )
          )
      val jars = fetch(deps: _*)
      new URLClassLoader(jars.map(_.toUri.toURL).toArray, null)
    }

  def resolveDecoder(scalaVersion: ScalaVersion): Try[Seq[Path]] =
    Try {
      fetch(
        coursierapi.Dependency.of(
          BuildInfo.organization,
          s"${BuildInfo.decoderName}_3",
          BuildInfo.version
        ),
        coursierapi.Dependency.of("org.scala-lang", "scala3-library_3", scalaVersion.value),
        // The decoder depends on tasty-query 1.6.1, that can't read the TASTy files of the
        // standard library since Scala 3.8 (it fails on scala/IArray$package.tasty, or on
        // scala.ImpureFunction1), so that it can't decode the methods of the standard library
        // (Range.foreach, …). Newer versions can, and are compatible with the decoder. They need
        // the Scala 3.10 library (coursier keeps the highest scala3-library version).
        coursierapi.Dependency.of(
          "ch.epfl.scala",
          "tasty-query_3",
          CoursierResolver.tastyQueryVersion
        ),
        coursierapi.Dependency.of(
          "org.scala-lang",
          "scala3-library_3",
          CoursierResolver.minDecoderScalaVersion
        )
      )
    }
}

object CoursierResolver {
  def tastyQueryVersion = "1.10.0"
  def minDecoderScalaVersion = "3.10.1"
}
