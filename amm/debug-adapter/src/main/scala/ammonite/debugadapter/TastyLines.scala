package ammonite.debugadapter

import ch.epfl.scala.debugadapter.Logger
import ch.epfl.scala.debugadapter.internal.StackTraceProvider
import com.microsoft.java.debug.core.adapter.IDebugAdapterContext
import com.microsoft.java.debug.core.adapter.stacktrace.DecodedMethod
import com.microsoft.java.debug.core.protocol.Requests.StepFilters
import com.sun.jdi.{ClassLoaderReference, Location, Method, ReferenceType}

import java.lang.reflect.{InvocationHandler, InvocationTargetException, Proxy}
import java.util.concurrent.ConcurrentHashMap

import scala.jdk.CollectionConverters._

/**
 * Lets the Scala 3 decoder of scala-debug-adapter decode the methods of cells
 *
 * Ammonite compiles the code of a cell wrapped in code of its own, then shifts the line numbers of
 * the byte code, so that they're those of the cell. The positions in the TASTy files are still
 * those of the wrapped code though. The decoder matches the line numbers of methods with the
 * positions of TASTy trees (to find the lambda a method was compiled from, say), so it can't
 * decode most of the methods of cells (and falls back to their JVM names, like
 * `cmd3$Helper.$init$$$anonfun$1(int): void`).
 *
 * So we pass the decoder JDI objects whose line numbers are shifted back to those of the TASTy
 * files, for the classes of cells.
 */
final class TastyLines {

  private val shifts = new ConcurrentHashMap[String, Integer]

  /**
   * Records the line shift of classes: line numbers in their TASTy files are those of their byte
   * code plus `shift`
   */
  def update(classNames: Seq[String], shift: Int): Unit =
    for (name <- classNames)
      shifts.put(name, shift)

  private def shift(tpe: ReferenceType): Option[Int] =
    Option(shifts.get(tpe.name())).map(_.intValue)

  def method(m: Method): Method =
    shift(m.declaringType()) match {
      case Some(s) => proxy(m, classOf[Method], s)
      case None => m
    }

  /** Wraps a JDI object of a shifted class, along with the JDI objects it returns */
  private def proxy[T <: AnyRef](underlying: T, iface: Class[T], shift: Int): T =
    Proxy.newProxyInstance(
      iface.getClassLoader,
      TastyLines.jdiInterfaces(underlying.getClass),
      new TastyLines.Handler(underlying, shift, wrap)
    ).asInstanceOf[T]

  private def wrap(value: AnyRef): AnyRef =
    value match {
      case m: Method =>
        method(m)
      case l: Location =>
        shift(l.declaringType()).fold[AnyRef](l)(proxy(l, classOf[Location], _))
      case t: ReferenceType =>
        shift(t).fold[AnyRef](t)(proxy(t, classOf[ReferenceType], _))
      case l: ClassLoaderReference =>
        // its visible classes include those of cells
        proxy(l, classOf[ClassLoaderReference], 0)
      case l: java.util.List[_] =>
        l.asScala.map(e => wrap(e.asInstanceOf[AnyRef])).asJava
      case other =>
        other
    }
}

object TastyLines {

  private final class Handler(
      val underlying: AnyRef,
      shift: Int,
      wrap: AnyRef => AnyRef
  ) extends InvocationHandler {
    def invoke(self: AnyRef, m: java.lang.reflect.Method, args: Array[AnyRef]): AnyRef = {
      val args0 = Option(args).getOrElse(Array.empty[AnyRef]).map(unwrap)
      m.getName match {
        case "equals" if args0.length == 1 => Boolean.box(underlying == args0(0))
        case "hashCode" if args0.isEmpty => Int.box(underlying.hashCode())
        case "toString" if args0.isEmpty => underlying.toString
        case name =>
          val res =
            try m.invoke(underlying, args0: _*)
            catch {
              case e: InvocationTargetException => throw e.getCause
            }
          res match {
            // Location.lineNumber, -1 meaning no line number
            case line: Integer if name == "lineNumber" && line.intValue >= 0 =>
              Int.box(line.intValue + shift)
            case _ =>
              wrap(res)
          }
      }
    }
  }

  /** The object a proxy of ours wraps, so that we pass the latter to the methods of the former */
  private def unwrap(arg: AnyRef): AnyRef =
    if (arg != null && Proxy.isProxyClass(arg.getClass))
      Proxy.getInvocationHandler(arg) match {
        case h: Handler => h.underlying
        case _ => arg
      }
    else arg

  /**
   * The JDI interfaces a class implements, so that the decoder can tell class types from interface
   * types, say
   */
  private def jdiInterfaces(cls: Class[_]): Array[Class[_]] =
    Iterator
      .iterate[Class[_]](cls)(_.getSuperclass)
      .takeWhile(_ != null)
      .flatMap(_.getInterfaces)
      .filter(_.getName.startsWith("com.sun.jdi."))
      .toArray
      .distinct

  /**
   * Passes the decoder JDI objects with the line numbers of TASTy files
   *
   * Extends the stack trace provider of scala-debug-adapter, that other providers expect.
   */
  final class ShiftedStackTraceProvider(
      underlying: StackTraceProvider,
      lines: TastyLines,
      logger: Logger
  ) extends StackTraceProvider(Nil, logger, testMode = false) {
    override def initialize(
        context: IDebugAdapterContext,
        options: java.util.Map[String, AnyRef]
    ): Unit =
      underlying.initialize(context, options)
    override def close(): Unit =
      underlying.close()
    override def reload(): Unit =
      underlying.reload()
    override def skipOver(method: Method, filters: StepFilters): Boolean =
      underlying.skipOver(lines.method(method), filters)
    override def skipOut(upperLocation: Location, method: Method): Boolean =
      underlying.skipOut(upperLocation, method)
    override def decode(method: Method): DecodedMethod =
      underlying.decode(lines.method(method))
  }
}
