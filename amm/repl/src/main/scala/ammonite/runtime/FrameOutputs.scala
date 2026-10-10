package ammonite.runtime

import ammonite.compiler.iface.Preprocessor
import ammonite.util.Util.ClassFiles

import scala.collection.mutable

/**
 * Writes the code evaluated in each frame of a session on disk, along with its byte code.
 *
 * Each frame gets its own directory under `directory`, `frame-<id>`, with
 * - a `sources` directory, with the user code of each compiled command (`.sc` files), or the
 *   sources passed as is to the compiler (`.scala` files),
 * - a `generated-sources` directory, with the code actually passed to the compiler for each
 *   `.sc` file of `sources`, that is the user code wrapped by Ammonite,
 * - a `classes` directory, with the byte code of the commands evaluated in this frame, and
 *   their SemanticDB files if `semanticDbs` is true.
 *
 * Interested parties, like a BSP server, can be notified of new frames, and of frames whose
 * sources or class path change, via [[FrameOutputs.Listener]].
 *
 * @param semanticDbs whether the compiler should generate SemanticDB files
 * @param mapSemanticDbsToSources whether the SemanticDB files of wrapped code should be
 *                                rewritten so that they refer to the `.sc` files rather than to
 *                                the generated code (like Scala CLI does for scripts)
 */
final class FrameOutputs(
    val directory: os.Path,
    val semanticDbs: Boolean = false,
    val mapSemanticDbsToSources: Boolean = false
) {
  import FrameOutputs._

  private val sources0 = mutable.Map.empty[Int, Vector[Source]]
  @volatile private var listeners = List.empty[Listener]

  def frameDirectory(frame: Frame): os.Path = directory / s"frame-${frame.id}"
  def sourcesDirectory(frame: Frame): os.Path = frameDirectory(frame) / "sources"
  def generatedSourcesDirectory(frame: Frame): os.Path =
    frameDirectory(frame) / "generated-sources"
  def classesDirectory(frame: Frame): os.Path = frameDirectory(frame) / "classes"

  /**
   * Where the compiler writes SemanticDB files, before we move them to the class directory
   * of the frame they belong to
   */
  def semanticDbTargetDirectory: os.Path = directory / ".semanticdb"

  def addListener(listener: Listener): Unit = synchronized {
    listeners = listeners :+ listener
  }
  def removeListener(listener: Listener): Unit = synchronized {
    listeners = listeners.filter(_ ne listener)
  }

  /**
   * Starts tracking `frame`
   *
   * Should be called once per frame, before any code is evaluated in it. Anything left in the
   * directory of that frame, by a former session say, gets removed.
   */
  def register(frame: Frame): Unit = {
    val isNew = synchronized {
      !sources0.contains(frame.id) && {
        sources0(frame.id) = Vector.empty
        true
      }
    }
    if (isNew) {
      os.remove.all(frameDirectory(frame))
      os.makeDir.all(sourcesDirectory(frame))
      os.makeDir.all(classesDirectory(frame))
      frame.addChangeListener(() => listeners.foreach(_.frameChanged(frame)))
      listeners.foreach(_.frameCreated(frame))
    }
  }

  /**
   * Writes the code about to be compiled in `frame` on disk
   *
   * Should be followed by a call to [[commit]] if compilation succeeds, or to [[discard]] if
   * it fails.
   *
   * @param path where the source should be written, relative to the source directories, and
   *             without extension
   */
  def prepare(frame: Frame, path: os.SubPath, processed: Preprocessor.Output): Pending = {
    val pending = Pending(frame, path, processed)
    os.write.over(pending.compiledFile, processed.code, createFolders = true)
    pending
  }

  /**
   * Path of the file the compiler should compile, relative to `directory`
   *
   * When generating SemanticDBs, `directory` is the source root, which the compilers resolve
   * this path against.
   */
  def compilerFileName(pending: Pending): String =
    pending.compiledFile.relativeTo(directory).segments.mkString("/")

  /** Removes the source written by [[prepare]] */
  def discard(pending: Pending): Unit = {
    os.remove(pending.compiledFile, checkExists = false)
    if (semanticDbs)
      os.remove(stagedSemanticDb(pending), checkExists = false)
  }

  /**
   * Records the source written by [[prepare]] as being part of its frame, and writes the
   * compiler output in the class directory of that frame
   */
  def commit(pending: Pending, classFiles: ClassFiles): Unit = {
    val frame = pending.frame
    addClassFiles(frame, classFiles)
    if (pending.isWrapped)
      os.write.over(pending.sourceFile, pending.processed.userCode, createFolders = true)
    if (semanticDbs)
      moveSemanticDb(pending)
    val source = Source(
      pending.sourceFile,
      if (pending.isWrapped)
        Some(WrappedSource(
          pending.compiledFile,
          pending.processed.topWrapper,
          pending.processed.bottomWrapper
        ))
      else None
    )
    synchronized {
      val former = sources0.getOrElse(frame.id, Vector.empty)
      sources0(frame.id) = former.filter(_.path != source.path) :+ source
    }
    listeners.foreach(_.frameChanged(frame))
  }

  private def semanticDbPath(source: os.Path): os.SubPath = {
    val rel = source.relativeTo(directory)
    os.sub / "META-INF" / "semanticdb" / rel.segments.init / s"${rel.last}.semanticdb"
  }

  private def stagedSemanticDb(pending: Pending): os.Path =
    semanticDbTargetDirectory / semanticDbPath(pending.compiledFile)

  private def moveSemanticDb(pending: Pending): Unit = {
    val staged = stagedSemanticDb(pending)
    val classes = classesDirectory(pending.frame)
    if (os.isFile(staged)) {
      if (pending.isWrapped && mapSemanticDbsToSources) {
        val processed = pending.processed
        // The top wrapper ends with a new line, so that the user code starts at the beginning
        // of a line, and the lines of the user code are those of the generated code, shifted
        val topLines = processed.topWrapper.count(_ == '\n')
        val userCodeLines = processed.userCode.count(_ == '\n') + 1
        val adjust: Int => Option[Int] = { line =>
          val line0 = line - topLines
          if (line0 >= 0 && line0 < userCodeLines) Some(line0)
          else None
        }
        SemanticdbProcessor.postProcess(
          Some(processed.userCode),
          pending.sourceFile.relativeTo(directory),
          adjust,
          staged,
          classes / semanticDbPath(pending.sourceFile)
        )
      } else
        // Not changing positions here, but the source path the Scala 2 SemanticDB plugin
        // writes is wrongly URL-encoded (as a whole), so we write it back
        SemanticdbProcessor.postProcess(
          None,
          pending.compiledFile.relativeTo(directory),
          Some(_),
          staged,
          classes / semanticDbPath(pending.compiledFile)
        )
      os.remove(staged)
    }
  }

  /** Writes byte code (and other compiler outputs, like TASTy files) in the classes of `frame` */
  def addClassFiles(frame: Frame, classFiles: ClassFiles): Unit =
    for ((name, bytes) <- classFiles)
      os.write.over(
        classesDirectory(frame) / name.split('/').toSeq,
        bytes,
        createFolders = true
      )

  /** The sources written so far for `frame` */
  def sources(frame: Frame): Seq[Source] = synchronized {
    sources0.getOrElse(frame.id, Vector.empty)
  }

  /**
   * Code about to be compiled
   *
   * @param path path of the source, relative to the source directories, without extension
   */
  final case class Pending(frame: Frame, path: os.SubPath, processed: Preprocessor.Output) {

    /** Whether Ammonite wrapped the user code, or passes it as is to the compiler */
    def isWrapped: Boolean =
      processed.prefixCharLength > 0 || processed.suffixCharLength > 0

    /** The file users are meant to look at */
    def sourceFile: os.Path =
      if (isWrapped) sourcesDirectory(frame) / path.segments.init / s"${path.last}.sc"
      else sourcesDirectory(frame) / path.segments.init / s"${path.last}.scala"

    /** The file passed to the compiler */
    def compiledFile: os.Path =
      if (isWrapped) generatedSourcesDirectory(frame) / path.segments.init / s"${path.last}.scala"
      else sourceFile
  }
}

object FrameOutputs {

  /**
   * A source file of a frame
   *
   * @param path the file users are meant to look at
   * @param wrapped if `path` is a `.sc` file, details about the code actually compiled for it
   */
  final case class Source(path: os.Path, wrapped: Option[WrappedSource])

  /**
   * Code actually passed to the compiler for a `.sc` file
   *
   * The content of `path` is the one of `topWrapper`, followed by the one of the `.sc` file,
   * followed by the one of `bottomWrapper`.
   *
   * @param path file with the wrapped code
   */
  final case class WrappedSource(path: os.Path, topWrapper: String, bottomWrapper: String)

  trait Listener {

    /** Called when a new frame is registered */
    def frameCreated(frame: Frame): Unit

    /** Called when sources, JARs, or compiler plugins are added to a frame */
    def frameChanged(frame: Frame): Unit
  }
}
