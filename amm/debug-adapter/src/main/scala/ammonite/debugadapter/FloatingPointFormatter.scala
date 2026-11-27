package ammonite.debugadapter

import com.microsoft.java.debug.core.adapter.formatter.{IValueFormatter, NumericFormatter}
import com.sun.jdi.{DoubleValue, FloatValue, Type, Value}

import java.util.{Map => JMap}

/**
 * Formats doubles and floats like the JVM and Scala do (`2.5`, `1.0E-9`), rather than with a fixed
 * number of decimals like java-debug's [[NumericFormatter]] (`2.500000`, `0.000000`)
 */
final class FloatingPointFormatter extends IValueFormatter {
  private val numeric = new NumericFormatter

  def acceptType(tpe: Type, options: JMap[String, AnyRef]): Boolean =
    tpe != null && {
      val sig = tpe.signature()
      sig == "D" || sig == "F"
    }

  def toString(value: AnyRef, options: JMap[String, AnyRef]): String =
    value match {
      case d: DoubleValue => java.lang.Double.toString(d.value())
      case f: FloatValue => java.lang.Float.toString(f.value())
      case _ => numeric.toString(value, options)
    }

  // used when setting the values of variables
  def valueOf(value: String, tpe: Type, options: JMap[String, AnyRef]): Value =
    numeric.valueOf(value, tpe, options)
}
