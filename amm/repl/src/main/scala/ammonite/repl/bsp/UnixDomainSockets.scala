package ammonite.repl.bsp

import java.io.{IOException, InputStream, OutputStream}
import java.lang.reflect.InvocationTargetException
import java.net.{ProtocolFamily, SocketAddress, StandardProtocolFamily}
import java.nio.ByteBuffer
import java.nio.channels.{ClosedChannelException, ServerSocketChannel, SocketChannel}
import java.nio.file.Path

/**
 * Helpers to listen on and connect to Unix domain sockets
 *
 * The JDK supports those since Java 16, on Linux and macOS, but also on Windows 10 and later.
 * As we compile against older JDK APIs, we access them via reflection.
 */
object UnixDomainSockets {

  private def invoke(cls: Class[_], method: String, argType: Class[_], arg: AnyRef): AnyRef =
    try cls.getMethod(method, argType).invoke(null, arg)
    catch {
      case e: InvocationTargetException if e.getCause != null =>
        throw e.getCause
    }

  private def unixFamily: ProtocolFamily =
    StandardProtocolFamily.values().find(_.name == "UNIX").getOrElse {
      throw new UnsupportedOperationException(
        "Unix domain sockets require Java 16 or later " +
          s"(current Java version: ${sys.props.getOrElse("java.version", "unknown")})"
      )
    }

  /**
   * The address of the socket at `path`
   *
   * Relative paths are relative to the current directory. They allow to work around the
   * maximum length of socket paths (about 100 characters).
   */
  def address(path: Path): SocketAddress = {
    unixFamily // ensures we get a nice error message on older JVMs
    invoke(
      Class.forName("java.net.UnixDomainSocketAddress"),
      "of",
      classOf[Path],
      path
    ).asInstanceOf[SocketAddress]
  }

  /** Creates a socket file at `path`, and listens on it */
  def listen(path: Path): ServerSocketChannel = {
    val channel = invoke(classOf[ServerSocketChannel], "open", classOf[ProtocolFamily], unixFamily)
      .asInstanceOf[ServerSocketChannel]
    try channel.bind(address(path))
    catch {
      case e: Throwable =>
        channel.close()
        throw e
    }
    channel
  }

  def connect(path: Path): SocketChannel =
    SocketChannel.open(address(path))

  /*
   * We don't use java.nio.channels.Channels.{newInputStream, newOutputStream} for those, as
   * prior to Java 19, the streams they return lock the channel during reads and writes. That
   * prevents writing while a read is blocked (JDK-8279339), which is what JSON-RPC does all
   * the time.
   */

  def inputStream(channel: SocketChannel): InputStream =
    new InputStream {
      override def read(): Int = {
        val buf = new Array[Byte](1)
        if (read(buf, 0, 1) <= 0) -1
        else buf(0) & 0xff
      }
      override def read(b: Array[Byte], off: Int, len: Int): Int =
        if (len == 0) 0
        else
          try channel.read(ByteBuffer.wrap(b, off, len))
          catch {
            // channel closed on our side, possibly by another thread
            case _: ClosedChannelException => -1
          }
      override def close(): Unit =
        try channel.shutdownInput()
        catch { case _: IOException => }
    }

  def outputStream(channel: SocketChannel): OutputStream =
    new OutputStream {
      override def write(b: Int): Unit =
        write(Array(b.toByte), 0, 1)
      override def write(b: Array[Byte], off: Int, len: Int): Unit = {
        val buf = ByteBuffer.wrap(b, off, len)
        while (buf.hasRemaining)
          channel.write(buf)
      }
      override def close(): Unit =
        try channel.shutdownOutput()
        catch { case _: IOException => }
    }
}
