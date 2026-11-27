package ammonite.debugger

import java.io.{BufferedInputStream, ByteArrayOutputStream, InputStream}
import java.net.Socket
import java.nio.charset.StandardCharsets
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicInteger

import scala.concurrent.{Future, Promise}
import scala.jdk.CollectionConverters._
import scala.util.control.NonFatal

/**
 * Client of a Debug Adapter Protocol server, over a socket
 *
 * Messages are JSON objects, each preceded by a `Content-Length` header.
 *
 * @param onEvent
 *   called with the events the server sends, from the thread reading its messages
 * @param onClose
 *   called once the connection is closed
 */
final class DapClient(
    socket: Socket,
    onEvent: ujson.Obj => Unit,
    onClose: () => Unit,
    log: Logger
) {

  private val seq = new AtomicInteger(1)
  private val pending = new ConcurrentHashMap[Int, Promise[ujson.Obj]]
  private val out = socket.getOutputStream
  private val lock = new Object

  @volatile private var closed = false

  def isClosed: Boolean = closed

  /** Sends a request, and returns the response the server sends back */
  def request(command: String, arguments: ujson.Value = ujson.Obj()): Future[ujson.Obj] = {
    val seq0 = seq.getAndIncrement()
    val promise = Promise[ujson.Obj]()
    pending.put(seq0, promise)
    val msg = ujson.Obj(
      "seq" -> seq0,
      "type" -> "request",
      "command" -> command,
      "arguments" -> arguments
    )
    try send(msg)
    catch {
      case NonFatal(e) =>
        pending.remove(seq0)
        promise.tryFailure(e)
    }
    if (closed)
      for (p <- Option(pending.remove(seq0)))
        p.tryFailure(new Exception("Connection to the debug adapter closed"))
    promise.future
  }

  private def send(msg: ujson.Value): Unit = {
    val content = ujson.write(msg).getBytes(StandardCharsets.UTF_8)
    log.debug(s"DAP >>> ${ujson.write(msg)}")
    val header = s"Content-Length: ${content.length}\r\n\r\n".getBytes(StandardCharsets.US_ASCII)
    lock.synchronized {
      out.write(header)
      out.write(content)
      out.flush()
    }
  }

  private def readMessage(in: InputStream): Option[Array[Byte]] = {
    // headers, separated by \r\n, ending with an empty line
    var contentLength = -1
    var done = false
    var eof = false
    while (!done && !eof) {
      val line = new ByteArrayOutputStream
      var c = in.read()
      while (c >= 0 && c != '\n') {
        if (c != '\r') line.write(c)
        c = in.read()
      }
      if (c < 0) eof = true
      else {
        val line0 = new String(line.toByteArray, StandardCharsets.US_ASCII).trim
        if (line0.isEmpty) {
          if (contentLength >= 0) done = true
        } else if (line0.toLowerCase(java.util.Locale.ROOT).startsWith("content-length:"))
          contentLength = line0.drop("content-length:".length).trim.toInt
      }
    }
    if (eof) None
    else {
      val buf = new Array[Byte](contentLength)
      var read = 0
      while (read < contentLength && !eof) {
        val n = in.read(buf, read, contentLength - read)
        if (n < 0) eof = true
        else read += n
      }
      if (eof) None else Some(buf)
    }
  }

  private val readerThread = new Thread("ammonite-dap-client") {
    setDaemon(true)
    override def run(): Unit = {
      val in = new BufferedInputStream(socket.getInputStream)
      try {
        var msgOpt = readMessage(in)
        while (msgOpt.nonEmpty) {
          for (msg <- msgOpt)
            try handle(ujson.read(msg))
            catch {
              case NonFatal(e) =>
                log.warn(
                  s"Error processing DAP message ${new String(msg, StandardCharsets.UTF_8)}",
                  e
                )
            }
          msgOpt = readMessage(in)
        }
      } catch {
        case NonFatal(e) if closed =>
          log.debug("Ignoring exception after the connection was closed", e)
        case NonFatal(e) =>
          log.warn("Error reading DAP messages", e)
      } finally close()
    }
  }

  private def handle(msg: ujson.Value): Unit = {
    log.debug(s"DAP <<< ${ujson.write(msg)}")
    val obj = msg.obj
    obj.get("type").map(_.str) match {
      case Some("response") =>
        val requestSeq = obj("request_seq").num.toInt
        Option(pending.remove(requestSeq)) match {
          case Some(p) => p.trySuccess(ujson.Obj.from(obj))
          case None => log.warn(s"Got response to unknown request $requestSeq")
        }
      case Some("event") =>
        onEvent(ujson.Obj.from(obj))
      case other =>
        log.warn(s"Ignoring DAP message of type $other")
    }
  }

  readerThread.start()

  def close(): Unit =
    if (!closed) {
      closed = true
      try socket.close()
      catch {
        case NonFatal(e) =>
          log.debug("Ignoring error closing DAP socket", e)
      }
      for (p <- pending.values().asScala)
        p.tryFailure(new Exception("Connection to the debug adapter closed"))
      pending.clear()
      onClose()
    }
}
