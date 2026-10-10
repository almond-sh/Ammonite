package ammonite.repl.bsp

import java.net.URI
import java.nio.file.Paths
import java.util.concurrent.CompletableFuture

import ch.epfl.scala.bsp4j._
import org.eclipse.lsp4j.jsonrpc.ResponseErrorException
import org.eclipse.lsp4j.jsonrpc.messages.{ResponseError, ResponseErrorCode}
import scala.build.bsp.{
  ScalaScriptBuildServer,
  WrappedSourceItem,
  WrappedSourcesItem,
  WrappedSourcesParams,
  WrappedSourcesResult
}

import scala.collection.JavaConverters._
import scala.util.control.NonFatal

/**
 * Answers the BSP requests of a single client, about a REPL session
 *
 * The `.sc` files of each frame are reported as sources, and the code actually compiled for
 * them can be retrieved via the `buildTarget/wrappedSources` request, like Scala CLI does for
 * scripts.
 *
 * @param onExit called when the client sends `build/exit`
 */
class ReplBuildServer(session: ReplBspSession, onExit: () => Unit) extends BuildServer
    with ScalaBuildServer
    with JavaBuildServer
    with ScalaScriptBuildServer {
  import ReplBuildServer._

  def buildInitialize(params: InitializeBuildParams): CompletableFuture[InitializeBuildResult] =
    future {
      val capabilities = new BuildServerCapabilities
      capabilities.setCompileProvider(new CompileProvider(List("scala").asJava))
      capabilities.setInverseSourcesProvider(true)
      capabilities.setDependencySourcesProvider(true)
      capabilities.setDependencyModulesProvider(false)
      capabilities.setResourcesProvider(true)
      capabilities.setOutputPathsProvider(true)
      capabilities.setBuildTargetChangedProvider(true)
      capabilities.setJvmRunEnvironmentProvider(false)
      capabilities.setJvmTestEnvironmentProvider(false)
      capabilities.setCanReload(true)
      new InitializeBuildResult("ammonite", ammonite.Constants.version, bspVersion, capabilities)
    }
  def onBuildInitialized(): Unit = ()
  def buildShutdown(): CompletableFuture[Object] =
    future(null)
  def onBuildExit(): Unit =
    onExit()

  def workspaceBuildTargets(): CompletableFuture[WorkspaceBuildTargetsResult] =
    future {
      new WorkspaceBuildTargetsResult(session.frames.map(session.buildTarget).asJava)
    }
  def workspaceReload(): CompletableFuture[Object] =
    // we always answer with up-to-date data, nothing to reload
    future(null)

  def buildTargetSources(params: SourcesParams): CompletableFuture[SourcesResult] =
    future {
      val items = session.framesFor(params.getTargets).map {
        case (id, frame) =>
          val sources = session.sources(frame).map { source =>
            new SourceItem(uri(source.path), SourceItemKind.FILE, false)
          }
          val item = new SourcesItem(id, sources.asJava)
          item.setRoots(List(uri(session.outputs.sourcesDirectory(frame))).asJava)
          item
      }
      new SourcesResult(items.asJava)
    }

  def buildTargetWrappedSources(
      params: WrappedSourcesParams
  ): CompletableFuture[WrappedSourcesResult] =
    future {
      val items = session.framesFor(params.getTargets).map {
        case (id, frame) =>
          val sources = session.sources(frame).flatMap { source =>
            source.wrapped.map { wrapped =>
              val item = new WrappedSourceItem(uri(source.path), uri(wrapped.path))
              item.setTopWrapper(wrapped.topWrapper)
              item.setBottomWrapper(wrapped.bottomWrapper)
              item
            }
          }
          new WrappedSourcesItem(id, sources.asJava)
      }
      new WrappedSourcesResult(items.asJava)
    }

  def buildTargetInverseSources(
      params: InverseSourcesParams
  ): CompletableFuture[InverseSourcesResult] =
    future {
      val pathOpt =
        try Some(os.Path(Paths.get(new URI(params.getTextDocument.getUri))))
        catch { case NonFatal(_) => None }
      val targets = pathOpt.toSeq.flatMap { path =>
        session.frames.filter { frame =>
          session.sources(frame).exists { source =>
            source.path == path || source.wrapped.exists(_.path == path)
          }
        }
      }
      new InverseSourcesResult(targets.map(session.targetId).asJava)
    }

  def buildTargetDependencySources(
      params: DependencySourcesParams
  ): CompletableFuture[DependencySourcesResult] =
    future {
      val items = session.framesFor(params.getTargets).map {
        case (id, frame) =>
          new DependencySourcesItem(id, session.dependencySources(frame).asJava)
      }
      new DependencySourcesResult(items.asJava)
    }

  def buildTargetDependencyModules(
      params: DependencyModulesParams
  ): CompletableFuture[DependencyModulesResult] =
    future {
      val items = session.framesFor(params.getTargets).map {
        case (id, _) =>
          new DependencyModulesItem(id, List.empty[DependencyModule].asJava)
      }
      new DependencyModulesResult(items.asJava)
    }

  def buildTargetResources(params: ResourcesParams): CompletableFuture[ResourcesResult] =
    future {
      val items = session.framesFor(params.getTargets).map {
        case (id, _) =>
          new ResourcesItem(id, List.empty[String].asJava)
      }
      new ResourcesResult(items.asJava)
    }

  def buildTargetOutputPaths(params: OutputPathsParams): CompletableFuture[OutputPathsResult] =
    future {
      val items = session.framesFor(params.getTargets).map {
        case (id, frame) =>
          val dir = session.outputs.frameDirectory(frame)
          val path = new OutputPathItem(uri(dir) + "/", OutputPathItemKind.DIRECTORY)
          new OutputPathsItem(id, List(path).asJava)
      }
      new OutputPathsResult(items.asJava)
    }

  def buildTargetCompile(params: CompileParams): CompletableFuture[CompileResult] =
    future {
      // The REPL compiled everything already
      val res = new CompileResult(StatusCode.OK)
      res.setOriginId(params.getOriginId)
      res
    }

  def buildTargetRun(params: RunParams): CompletableFuture[RunResult] =
    notSupported("buildTarget/run")
  def buildTargetTest(params: TestParams): CompletableFuture[TestResult] =
    notSupported("buildTarget/test")
  def debugSessionStart(params: DebugSessionParams): CompletableFuture[DebugSessionAddress] =
    notSupported("debugSession/start")

  def buildTargetCleanCache(params: CleanCacheParams): CompletableFuture[CleanCacheResult] =
    future {
      val res = new CleanCacheResult(false)
      res.setMessage("Cleaning the outputs of a REPL session isn't supported")
      res
    }

  def buildTargetScalacOptions(
      params: ScalacOptionsParams
  ): CompletableFuture[ScalacOptionsResult] =
    future {
      val items = session.framesFor(params.getTargets).map {
        case (id, frame) =>
          new ScalacOptionsItem(
            id,
            session.frameScalacOptions(frame).asJava,
            session.classpath(frame).asJava,
            session.classDirectory(frame)
          )
      }
      new ScalacOptionsResult(items.asJava)
    }

  def buildTargetJavacOptions(
      params: JavacOptionsParams
  ): CompletableFuture[JavacOptionsResult] =
    future {
      val items = session.framesFor(params.getTargets).map {
        case (id, frame) =>
          new JavacOptionsItem(
            id,
            List.empty[String].asJava,
            session.classpath(frame).asJava,
            session.classDirectory(frame)
          )
      }
      new JavacOptionsResult(items.asJava)
    }

  def buildTargetScalaMainClasses(
      params: ScalaMainClassesParams
  ): CompletableFuture[ScalaMainClassesResult] =
    future {
      val items = session.framesFor(params.getTargets).map {
        case (id, _) =>
          new ScalaMainClassesItem(id, List.empty[ScalaMainClass].asJava)
      }
      new ScalaMainClassesResult(items.asJava)
    }

  def buildTargetScalaTestClasses(
      params: ScalaTestClassesParams
  ): CompletableFuture[ScalaTestClassesResult] =
    future {
      val items = session.framesFor(params.getTargets).map {
        case (id, _) =>
          new ScalaTestClassesItem(id, List.empty[String].asJava)
      }
      new ScalaTestClassesResult(items.asJava)
    }
}

object ReplBuildServer {

  /** The version of the BSP protocol we implement, that of bsp4j */
  def bspVersion = "2.1.1"

  private def uri(path: os.Path): String =
    ReplBspSession.uri(path.toNIO)

  // All our answers are computed quickly from in-memory data, no need to off-load them
  private def future[T](t: => T): CompletableFuture[T] =
    try CompletableFuture.completedFuture(t)
    catch {
      case NonFatal(e) =>
        val f = new CompletableFuture[T]
        f.completeExceptionally(e)
        f
    }

  private def notSupported[T](method: String): CompletableFuture[T] = {
    val f = new CompletableFuture[T]
    f.completeExceptionally(
      new ResponseErrorException(
        new ResponseError(
          ResponseErrorCode.MethodNotFound,
          s"$method isn't supported for REPL sessions",
          null
        )
      )
    )
    f
  }
}
