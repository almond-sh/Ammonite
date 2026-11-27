package ammonite.main

import java.lang.management.ManagementFactory

import ammonite.debugger.Jdwp

import scala.jdk.CollectionConverters._

/**
 * Restarts Ammonite with a JDWP agent, that the debugger needs
 *
 * The JDWP agent can only be loaded when the JVM starts, so when Ammonite is asked to enable
 * the debugger, and doesn't run with an agent, it starts a new JVM with the same options and
 * arguments, plus those of the agent (see [[Jdwp.javaOptions]]), and waits for it.
 */
object DebuggerRestart {

  /** Set in the environment of the new JVM, so that we don't restart in a loop */
  def envVar = "AMMONITE_DEBUGGER_RESTARTED"

  def needed(): Boolean =
    !Jdwp.agentLoaded()

  def alreadyRestarted(): Boolean =
    sys.env.get(envVar).contains("1")

  /** The command starting the current JVM again, with a JDWP agent */
  def command(args: Seq[String]): Seq[String] = {
    val java = Option(ProcessHandle.current().info().command().orElse(null))
      .getOrElse(sys.props("java.home") + "/bin/java")
    val jvmArgs = ManagementFactory.getRuntimeMXBean.getInputArguments.asScala.toList
    val classPath = sys.props.getOrElse("java.class.path", "")
    val javaCommand = sys.props.getOrElse("sun.java.command", "")
    val main =
      // java -jar app.jar …: the class path is the JAR
      if (classPath.nonEmpty && javaCommand.startsWith(classPath + " ") || javaCommand == classPath)
        Seq("-jar", classPath)
      else
        Seq("-cp", classPath, javaCommand.takeWhile(_ != ' '))
    Seq(java) ++ jvmArgs ++ Jdwp.javaOptions ++ main ++ args
  }

  /** Runs Ammonite again with a JDWP agent, and returns its exit code */
  def run(args: Seq[String]): Int = {
    val cmd = command(args)
    val pb = new ProcessBuilder(cmd: _*).inheritIO()
    pb.environment().put(envVar, "1")
    val proc = pb.start()
    // Ctrl-C makes the terminal send SIGINT to both JVMs, only the new one should handle it.
    // Ignoring it only once the new JVM is started, as it would inherit that otherwise.
    sun.misc.Signal.handle(new sun.misc.Signal("INT"), sun.misc.SignalHandler.SIG_IGN)
    proc.waitFor()
  }
}
