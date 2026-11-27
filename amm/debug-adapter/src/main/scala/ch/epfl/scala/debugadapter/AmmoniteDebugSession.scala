package ch.epfl.scala.debugadapter

import ammonite.debugadapter.{FloatingPointFormatter, TastyLines}
import ch.epfl.scala.debugadapter.internal.{DebugSession, StackTraceProvider}
import com.microsoft.java.debug.core.adapter.{
  DebugAdapter,
  IDebugAdapterContext,
  IStackTraceProvider,
  IVirtualMachineManagerProvider,
  ProtocolServer,
  ProviderContext
}
import com.microsoft.java.debug.core.adapter.variables.VariableFormatter
import com.sun.jdi.VirtualMachineManager

import java.net.Socket

import scala.concurrent.{ExecutionContext, Future}

/**
 * Starts scala-debug-adapter sessions, like [[DebugServer]] does, but with a custom JDI
 * [[VirtualMachineManager]], and a stack trace provider able to decode the methods of cells (see
 * [[TastyLines]])
 *
 * Lives in this package to access the package-private [[DebugSession]].
 */
object AmmoniteDebugSession {

  def run(
      socket: Socket,
      debuggee: Debuggee,
      resolver: DebugToolsResolver,
      logger: Logger,
      config: DebugConfig,
      vmManager: VirtualMachineManager,
      tastyLines: Option[TastyLines]
  )(implicit ec: ExecutionContext): (() => Unit, Future[Unit]) = {
    val session = DebugSession(socket, debuggee, resolver, logger, config)
    // ProviderContext doesn't allow to replace providers, so we update its map ourselves. Some
    // providers are only registered upon attach (see ScalaProviderContext.configure), so we
    // replace the map with one that wraps those when they're added.
    val context = providerContext(session)
    val mapField = classOf[ProviderContext].getDeclaredField("providerMap")
    mapField.setAccessible(true)
    val former = mapField.get(context).asInstanceOf[java.util.Map[Class[_], AnyRef]]
    val providers: java.util.Map[Class[_], AnyRef] =
      new java.util.HashMap[Class[_], AnyRef](former) {
        override def put(key: Class[_], value: AnyRef): AnyRef = {
          val value0 = (value, tastyLines) match {
            case (provider: StackTraceProvider, Some(lines))
                if key == classOf[IStackTraceProvider] =>
              new TastyLines.ShiftedStackTraceProvider(provider, lines, logger)
            case _ => value
          }
          super.put(key, value0)
        }
      }
    providers.put(
      classOf[IVirtualMachineManagerProvider],
      new IVirtualMachineManagerProvider {
        def getVirtualMachineManager: VirtualMachineManager = vmManager
      }
    )
    mapField.set(context, providers)
    registerFormatters(session)
    session.start()
    val done = session.exitStatus.map(_ => session.close())
    (() => session.close(), done)
  }

  /**
   * Formats doubles and floats with all their digits, rather than with 6 decimals. java-debug
   * creates the variable formatter of the debug adapter context of each session itself, without
   * exposing it, we get it via reflection (ProtocolServer.debugAdapter, then
   * DebugAdapter.debugContext).
   */
  private def registerFormatters(session: DebugSession): Unit = {
    def field(cls: Class[_], name: String, obj: AnyRef): AnyRef = {
      val f = cls.getDeclaredField(name)
      f.setAccessible(true)
      f.get(obj)
    }
    val adapter = field(classOf[ProtocolServer], "debugAdapter", session)
    val context = field(classOf[DebugAdapter], "debugContext", adapter)
      .asInstanceOf[IDebugAdapterContext]
    context.getVariableFormatter match {
      case formatter: VariableFormatter =>
        // NumericFormatter, that handles doubles and floats by default, is registered with
        // priority 1
        formatter.registerValueFormatter(new FloatingPointFormatter, 2)
      case _ =>
    }
  }

  // DebugSession keeps its provider context private, we get it via reflection
  private def providerContext(session: DebugSession): ProviderContext = {
    val field = classOf[DebugSession].getDeclaredFields
      .find(f => classOf[ProviderContext].isAssignableFrom(f.getType))
      .getOrElse(sys.error("Provider context field of DebugSession not found"))
    field.setAccessible(true)
    field.get(session).asInstanceOf[ProviderContext]
  }
}
