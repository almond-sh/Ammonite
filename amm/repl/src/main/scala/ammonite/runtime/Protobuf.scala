package ammonite.runtime

import java.io.ByteArrayOutputStream

import scala.collection.mutable

/**
 * Minimal Protocol Buffers decoding and encoding, enough to edit SemanticDB files
 *
 * Messages are read as a sequence of fields, whose values are left undecoded beyond their wire
 * type. Length-delimited values can then be decoded as nested messages if needed.
 */
object Protobuf {

  sealed abstract class Value extends Product with Serializable
  object Value {
    final case class VarInt(value: Long) extends Value
    final case class Fixed64(value: Long) extends Value
    final case class LengthDelimited(bytes: Array[Byte]) extends Value {
      def message: Seq[Field] = Protobuf.parse(bytes)
      def string: String = new String(bytes, "UTF-8")
    }
    final case class Fixed32(value: Int) extends Value

    def message(fields: Seq[Field]): LengthDelimited = LengthDelimited(Protobuf.write(fields))
    def string(s: String): LengthDelimited = LengthDelimited(s.getBytes("UTF-8"))
  }

  final case class Field(number: Int, value: Value)

  def parse(bytes: Array[Byte]): Seq[Field] = {
    var idx = 0
    def readVarInt(): Long = {
      var res = 0L
      var shift = 0
      var more = true
      while (more) {
        val b = bytes(idx) & 0xff
        idx += 1
        res |= (b & 0x7fL) << shift
        shift += 7
        more = (b & 0x80) != 0
      }
      res
    }
    def readFixed(len: Int): Long = {
      var res = 0L
      for (i <- 0 until len)
        res |= (bytes(idx + i) & 0xffL) << (8 * i)
      idx += len
      res
    }
    val fields = new mutable.ListBuffer[Field]
    while (idx < bytes.length) {
      val key = readVarInt()
      val number = (key >>> 3).toInt
      val value = (key & 0x7).toInt match {
        case 0 => Value.VarInt(readVarInt())
        case 1 => Value.Fixed64(readFixed(8))
        case 2 =>
          val len = readVarInt().toInt
          val b = java.util.Arrays.copyOfRange(bytes, idx, idx + len)
          idx += len
          Value.LengthDelimited(b)
        case 5 => Value.Fixed32(readFixed(4).toInt)
        case other =>
          throw new IllegalArgumentException(s"Unsupported protobuf wire type $other")
      }
      fields += Field(number, value)
    }
    fields.toList
  }

  def write(fields: Seq[Field]): Array[Byte] = {
    val out = new ByteArrayOutputStream
    def writeVarInt(value: Long): Unit = {
      var v = value
      while ((v & ~0x7fL) != 0L) {
        out.write(((v & 0x7f) | 0x80).toInt)
        v >>>= 7
      }
      out.write(v.toInt)
    }
    def writeFixed(value: Long, len: Int): Unit =
      for (i <- 0 until len)
        out.write(((value >>> (8 * i)) & 0xff).toInt)
    for (field <- fields)
      field.value match {
        case Value.VarInt(v) =>
          writeVarInt(field.number.toLong << 3)
          writeVarInt(v)
        case Value.Fixed64(v) =>
          writeVarInt((field.number.toLong << 3) | 1)
          writeFixed(v, 8)
        case Value.LengthDelimited(b) =>
          writeVarInt((field.number.toLong << 3) | 2)
          writeVarInt(b.length.toLong)
          out.write(b)
        case Value.Fixed32(v) =>
          writeVarInt((field.number.toLong << 3) | 5)
          writeFixed(v.toLong, 4)
      }
    out.toByteArray
  }
}
