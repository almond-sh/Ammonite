package ammonite.debugger

import java.lang.management.ManagementFactory
import java.util.Properties

import scala.jdk.CollectionConverters._
import scala.util.Try

/** The JDWP agent the JVM of a session runs with, that the debugger attaches to */
object Jdwp {

  /** Whether the debugger should be enabled when not told otherwise: on Java >= 17 */
  def enabledByDefault: Boolean =
    javaMajorVersion >= 17

  private def javaMajorVersion: Int = {
    val v = sys.props.getOrElse("java.specification.version", "1.8")
    Try(v.stripPrefix("1.").takeWhile(_.isDigit).toInt).getOrElse(8)
  }

  /**
   * JVM options running a session with a JDWP agent, that listens on a random port of the
   * loopback interface, that [[listenerAddress]] can read back
   *
   * From Java 9, the agent only starts listening when [[ensureListening]] is called (`onjcmd=y`),
   * rather than when the JVM starts, so that the port is only open while a debugger gets attached:
   * once a debugger is connected, the agent stops listening.
   */
  def javaOptions: Seq[String] =
    if (javaMajorVersion >= 9)
      Seq(
        "-agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=127.0.0.1:0,quiet=y,onjcmd=y",
        // to read the address the agent listens on
        "--add-exports=java.base/jdk.internal.vm=ALL-UNNAMED",
        // to start the agent, see startViaCommand
        "--add-opens=jdk.management/com.sun.management.internal=ALL-UNNAMED"
      )
    else
      Seq("-agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=127.0.0.1:0,quiet=y")

  private def jdwpArgs: List[String] =
    Try(ManagementFactory.getRuntimeMXBean.getInputArguments.asScala.toList)
      .toOption
      .toList
      .flatten
      .filter(arg => arg.startsWith("-agentlib:jdwp=") || arg.startsWith("-Xrunjdwp:"))

  /** Whether the current JVM runs with a JDWP agent */
  def agentLoaded(): Boolean =
    jdwpArgs.nonEmpty

  /**
   * Makes the JDWP agent start listening if it doesn't yet, and returns the address it listens on
   *
   * The address is only available until a debugger connects.
   */
  def ensureListening(): Either[String, (String, Int)] = {
    def fromAgent = agentListenerAddress().flatMap(parseAddress)
    fromAgent match {
      case Some(addr) => Right(addr)
      case None if startsOnCommand =>
        startViaCommand().flatMap { _ =>
          fromAgent.toRight("JDWP agent started, but its address couldn't be read")
        }
      case None =>
        commandLineAddress().flatMap(parseAddress)
          .toRight("Could not get the address of the JDWP agent")
    }
  }

  private def startsOnCommand: Boolean =
    jdwpArgs.exists(_.split(',').contains("onjcmd=y"))

  /**
   * Runs the `VM.start_java_debugging` diagnostic command, like `jcmd <pid>
   * VM.start_java_debugging` does, for agents running with `onjcmd=y`
   *
   * That command is hidden, so the DiagnosticCommand MBean doesn't offer it, but the method of its
   * implementation running commands accepts it.
   */
  private def startViaCommand(): Either[String, Unit] =
    Try {
      val implClass = Class.forName("com.sun.management.internal.DiagnosticCommandImpl")
      val getter = implClass.getDeclaredMethod("getDiagnosticCommandMBean")
      getter.setAccessible(true)
      val impl = getter.invoke(null)
      val exec = implClass.getDeclaredMethod("executeDiagnosticCommand", classOf[String])
      exec.setAccessible(true)
      exec.invoke(impl, "VM.start_java_debugging").asInstanceOf[String]
    }.toEither.left.map(e => s"Error starting the JDWP agent: $e").flatMap { output =>
      if (output != null && output.contains("Debugging has been started")) Right(())
      else Left(s"Could not start the JDWP agent: $output")
    }

  /** The address the JDWP agent of the current JVM listens on, if it runs with one */
  def listenerAddress(): Option[(String, Int)] =
    agentListenerAddress()
      .orElse(commandLineAddress())
      .flatMap(parseAddress)

  // The agent records the address it actually listens on (with the port it picked if it was
  // asked to pick one) in the agent properties
  private def agentListenerAddress(): Option[String] = {
    val classNames = Seq("jdk.internal.vm.VMSupport", "sun.misc.VMSupport")
    classNames.iterator
      .flatMap { name =>
        Try {
          val cls = Class.forName(name)
          val props = cls.getMethod("getAgentProperties").invoke(null).asInstanceOf[Properties]
          Option(props.getProperty("sun.jdwp.listenerAddress"))
        }.toOption.flatten.iterator
      }
      .find(_.nonEmpty)
  }

  // Fallback, if the agent properties can't be read: the address passed on the command line,
  // if it has a non-zero port
  private def commandLineAddress(): Option[String] =
    jdwpArgs
      .flatMap { arg =>
        arg.dropWhile(c => c != '=' && c != ':').drop(1).split(',').toList
      }
      .collectFirst {
        case opt if opt.startsWith("address=") => opt.stripPrefix("address=")
      }
      .filter(addr => !addr.endsWith(":0") && addr != "0")

  private def parseAddress(address0: String): Option[(String, Int)] = {
    // the agent property looks like "dt_socket:41234"
    val address = address0.stripPrefix("dt_socket:")
    val idx = address.lastIndexOf(':')
    val (host, port) =
      if (idx < 0) ("127.0.0.1", address)
      else (address.take(idx), address.drop(idx + 1))
    val host0 = if (host.isEmpty || host == "*") "127.0.0.1" else host
    Try(port.toInt).toOption.filter(_ > 0).map((host0, _))
  }
}
