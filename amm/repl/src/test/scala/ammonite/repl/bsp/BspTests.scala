package ammonite.repl.bsp

import java.net.URI
import java.nio.file.Paths
import java.util.concurrent.{CompletableFuture, ConcurrentLinkedQueue, Executors, TimeUnit}

import ammonite.TestRepl
import ammonite.util.{Classpath, Res, Util}
import ch.epfl.scala.bsp4j._
import org.eclipse.lsp4j.jsonrpc.Launcher
import org.eclipse.lsp4j.jsonrpc.services.JsonRequest
import scala.build.bsp.{WrappedSourcesParams, WrappedSourcesResult}
import utest._

import scala.collection.JavaConverters._

object BspTests extends TestSuite {

  // Not extending scala.build.bsp.ScalaScriptBuildServer, whose class file requires Java 17:
  // that would prevent loading this test suite on older JVMs, where it's skipped
  trait TestBuildServer extends BuildServer with ScalaBuildServer with JavaBuildServer {
    @JsonRequest("buildTarget/wrappedSources")
    def buildTargetWrappedSources(
        params: WrappedSourcesParams
    ): CompletableFuture[WrappedSourcesResult]
  }

  class TestBuildClient extends BuildClient {
    val events = new ConcurrentLinkedQueue[BuildTargetEvent]
    def onBuildShowMessage(params: ShowMessageParams): Unit = ()
    def onBuildLogMessage(params: LogMessageParams): Unit = ()
    def onBuildPublishDiagnostics(params: PublishDiagnosticsParams): Unit = ()
    def onBuildTargetDidChange(params: DidChangeBuildTarget): Unit =
      events.addAll(params.getChanges)
    def onBuildTaskStart(params: TaskStartParams): Unit = ()
    def onBuildTaskProgress(params: TaskProgressParams): Unit = ()
    def onBuildTaskFinish(params: TaskFinishParams): Unit = ()

    /** Waits for an event about `id`, and removes it and the ones that came before it */
    def waitFor(kind: BuildTargetEventKind, id: BuildTargetIdentifier): Unit = {
      val deadline = System.currentTimeMillis() + 10000L
      def found = events.asScala.exists(e => e.getKind == kind && e.getTarget == id)
      while (!found && System.currentTimeMillis() < deadline)
        Thread.sleep(20L)
      assert(found)
      events.clear()
    }
  }

  def withSession[T](f: (TestRepl, os.Path, TestBuildServer, TestBuildClient) => T): T = {
    val tmpDir = os.temp.dir(prefix = "amm-bsp-tests")
    val pool = Executors.newCachedThreadPool()
    try {
      val sessionDir = tmpDir / "session"
      val repl = new TestRepl(sessionDirectory = Some(sessionDir))
      val session = new ReplBspSession(
        () => repl.sess0.liveFrames,
        repl.frameOutputs.get,
        repl.scalaVersion,
        repl.interp.scalacOptions,
        Classpath.classpath(repl.initialClassLoader, None)
      )
      val socketPath = tmpDir / "bsp.sock"
      val server = ReplBspServer.start(socketPath.toString, session)
      try {
        val channel = UnixDomainSockets.connect(socketPath.toNIO)
        try {
          val client = new TestBuildClient
          val launcher = new Launcher.Builder[TestBuildServer]()
            .setExecutorService(pool)
            .setInput(UnixDomainSockets.inputStream(channel))
            .setOutput(UnixDomainSockets.outputStream(channel))
            .setRemoteInterface(classOf[TestBuildServer])
            .setLocalService(client)
            .create()
          launcher.startListening()
          val remote = launcher.getRemoteProxy
          remote.buildInitialize(new InitializeBuildParams(
            "tests",
            "0.1",
            ReplBuildServer.bspVersion,
            sessionDir.toNIO.toUri.toASCIIString,
            new BuildClientCapabilities(List("scala").asJava)
          )).get(10L, TimeUnit.SECONDS)
          remote.onBuildInitialized()
          f(repl, sessionDir, remote, client)
        } finally channel.close()
      } finally server.close()
    } finally {
      pool.shutdown()
      os.remove.all(tmpDir)
    }
  }

  def run(repl: TestRepl, code: String): Unit = {
    val (res, _, _, _, errors, _) = repl.run(code, repl.currentLine)
    assert(errors.isEmpty)
    assert(res.isInstanceOf[Res.Success[_]])
  }

  def targetId(sessionDir: os.Path, frame: Int): BuildTargetIdentifier =
    new BuildTargetIdentifier(ReplBspSession.uri((sessionDir / s"frame-$frame").toNIO))

  def path(uri: String): os.Path = os.Path(Paths.get(new URI(uri)))

  // The BSP server requires Java 17
  def bspSupported = Util.javaMajorVersion >= 17

  val tests = Tests {
    test("frames") {
      if (bspSupported) frames()
      else "Disabled on Java < 17"
    }
  }

  def frames(): Unit =
    withSession { (repl, sessionDir, server, client) =>
      val frame0 = targetId(sessionDir, 0)
      val frame1 = targetId(sessionDir, 1)
      val frame2 = targetId(sessionDir, 2)

      def targets() =
        server.workspaceBuildTargets().get(10L, TimeUnit.SECONDS).getTargets.asScala.toList

      val initialTargets = targets()
      assert(initialTargets.map(_.getId) == List(frame0))
      assert(initialTargets.head.getDependencies.isEmpty)
      val scalaTarget = initialTargets.head.getData match {
        case t: ScalaBuildTarget => t
        case other =>
          // bsp4j hands us JSON
          new com.google.gson.Gson().fromJson(
            other.asInstanceOf[com.google.gson.JsonElement],
            classOf[ScalaBuildTarget]
          )
      }
      assert(scalaTarget.getScalaVersion == repl.scalaVersion)

      run(repl, "val a = 2")
      // saving the session freezes the current frame, and creates a new one after that
      run(repl, "repl.sess.save()")
      client.waitFor(BuildTargetEventKind.CREATED, frame1)

      val extraDir = os.temp.dir(dir = sessionDir / os.up, prefix = "extra-cp")
      run(
        repl,
        s"""interp.load.cp(java.nio.file.Paths.get("${extraDir.toString.replace(
            "\\",
            "\\\\"
          )}"))"""
      )
      client.waitFor(BuildTargetEventKind.CHANGED, frame1)
      run(repl, "val b = a + 1")

      val targets1 = targets()
      assert(targets1.map(_.getId) == List(frame0, frame1))
      assert(targets1(1).getDependencies.asScala.toList == List(frame0))

      // sources and byte code are written in the directory of each frame
      val sources = server
        .buildTargetSources(new SourcesParams(List(frame0, frame1).asJava))
        .get(10L, TimeUnit.SECONDS)
        .getItems
        .asScala
        .map(item => item.getTarget -> item.getSources.asScala.map(s => path(s.getUri)).toList)
        .toMap
      val cmd0 = sessionDir / "frame-0" / "sources" / "ammonite" / "$sess" / "cmd0.sc"
      val cmd2 = sessionDir / "frame-1" / "sources" / "ammonite" / "$sess" / "cmd2.sc"
      val cmd3 = sessionDir / "frame-1" / "sources" / "ammonite" / "$sess" / "cmd3.sc"
      assert(sources(frame0).contains(cmd0))
      assert(sources(frame1) == List(cmd2, cmd3))
      assert(os.read(cmd3).contains("val b = a + 1"))
      assert(os.isFile(sessionDir / "frame-0" / "classes" / "ammonite" / "$sess" / "cmd0.class"))
      assert(os.isFile(sessionDir / "frame-1" / "classes" / "ammonite" / "$sess" / "cmd3.class"))

      // the wrapped code is the user code, surrounded by the top and bottom wrappers
      val wrapped = server
        .buildTargetWrappedSources(new WrappedSourcesParams(List(frame1).asJava))
        .get(10L, TimeUnit.SECONDS)
        .getItems
        .asScala
        .flatMap(_.getSources.asScala)
        .toList
      assert(wrapped.map(s => path(s.getUri)) == List(cmd2, cmd3))
      val wrapped0 = wrapped.last
      assert(
        os.read(path(wrapped0.getGeneratedUri)) ==
          wrapped0.getTopWrapper + os.read(cmd3) + wrapped0.getBottomWrapper
      )

      // a frame class path has its class directory, then the ones of its parents
      val scalacOptions = server
        .buildTargetScalacOptions(new ScalacOptionsParams(List(frame1).asJava))
        .get(10L, TimeUnit.SECONDS)
        .getItems
        .asScala
        .head
      val classpath = scalacOptions.getClasspath.asScala.toList.map(path)
      assert(path(scalacOptions.getClassDirectory) == sessionDir / "frame-1" / "classes")
      assert(
        classpath.take(2) ==
          List(sessionDir / "frame-1" / "classes", sessionDir / "frame-0" / "classes")
      )
      assert(classpath.contains(extraDir))

      val inverse = server
        .buildTargetInverseSources(
          new InverseSourcesParams(new TextDocumentIdentifier(cmd3.toNIO.toUri.toASCIIString))
        )
        .get(10L, TimeUnit.SECONDS)
        .getTargets
        .asScala
        .toList
      assert(inverse == List(frame1))

      // loading the former session drops frame-1, and creates frame-2 on top of frame-0
      run(repl, "repl.sess.load()")
      client.waitFor(BuildTargetEventKind.CREATED, frame2)
      val targets2 = targets()
      assert(targets2.map(_.getId) == List(frame0, frame2))
      assert(targets2(1).getDependencies.asScala.toList == List(frame0))
    }
}
