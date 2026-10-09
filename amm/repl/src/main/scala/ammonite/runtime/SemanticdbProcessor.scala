package ammonite.runtime

import java.math.BigInteger
import java.nio.charset.StandardCharsets
import java.security.MessageDigest

import ammonite.runtime.Protobuf.{Field, Value}

/**
 * Rewrites a SemanticDB file, so that it refers to another source, whose lines correspond to
 * those of the original one via `adjust`
 *
 * If `originalCode` is empty, the source content and checksum in the SemanticDB file are left
 * as is, and only its source path and positions are changed.
 *
 * Same logic as Scala CLI's SemanticdbProcessor (itself adapted from a former version of this
 * file in Ammonite), see
 * https://github.com/VirtusLab/scala-cli/blob/e419386f85a3947bf4c2e98eb6f927fd9d2c6ee6/modules/build/src/main/scala/scala/build/postprocessing/SemanticdbProcessor.scala
 * but working on the protobuf fields of SemanticDB files directly, rather than via the
 * scalameta SemanticDB model, as the latter drags too many dependencies in.
 *
 * Field numbers are those of
 * https://github.com/scalameta/scalameta/blob/v4.15.2/semanticdb/semanticdb/semanticdb.proto
 */
object SemanticdbProcessor {

  // TextDocuments
  private final val Documents = 1
  // TextDocument
  private final val Uri = 2
  private final val Text = 3
  private final val Md5 = 11
  private final val Occurrences = 6
  private final val Diagnostics = 7
  private final val Synthetics = 12
  // Range
  private final val StartLine = 1
  private final val EndLine = 3
  // SymbolOccurrence, Diagnostic, Synthetic, OriginalTree
  private final val RangeField = 1
  // Synthetic
  private final val SyntheticTree = 2
  // Tree
  private final val ApplyTree = 1
  private final val FunctionTree = 2
  private final val MacroExpansionTree = 5
  private final val OriginalTree = 6
  private final val SelectTree = 7
  private final val TypeApplyTree = 8
  // ApplyTree
  private final val ApplyFunction = 1
  private final val ApplyArguments = 2
  // FunctionTree
  private final val FunctionBody = 2
  // MacroExpansionTree
  private final val BeforeExpansion = 1
  // SelectTree
  private final val Qualifier = 1
  // TypeApplyTree
  private final val TypeApplyFunction = 1

  def postProcess(
      originalCode: Option[String],
      originalPath: os.RelPath,
      adjust: Int => Option[Int],
      orig: os.Path,
      dest: os.Path
  ): Unit = {

    def mapMessage(bytes: Array[Byte])(f: Field => Option[Field]): Option[Array[Byte]] = {
      val fields = Protobuf.parse(bytes).map(f)
      if (fields.forall(_.nonEmpty)) Some(Protobuf.write(fields.flatten))
      else None
    }

    def nested(field: Field)(f: Array[Byte] => Option[Array[Byte]]): Option[Field] =
      field.value match {
        case v: Value.LengthDelimited =>
          f(v.bytes).map(b => field.copy(value = Value.LengthDelimited(b)))
        case _ => Some(field)
      }

    // proto3 leaves out fields with default values, 0 here, so we write them all back
    def mapRange(range: Array[Byte]): Option[Array[Byte]] = {
      val fields = Protobuf.parse(range)
      def line(number: Int) =
        fields.reverseIterator.collectFirst {
          case Field(`number`, Value.VarInt(v)) => v.toInt
        }.getOrElse(0)
      for {
        startLine <- adjust(line(StartLine))
        endLine <- adjust(line(EndLine))
      } yield Protobuf.write(
        Field(StartLine, Value.VarInt(startLine.toLong)) +:
          Field(EndLine, Value.VarInt(endLine.toLong)) +:
          fields.filter(f => f.number != StartLine && f.number != EndLine)
      )
    }

    // for messages with an optional range
    def withRange(message: Array[Byte]): Option[Array[Byte]] =
      mapMessage(message) {
        case f @ Field(RangeField, _) => nested(f)(mapRange)
        case f => Some(f)
      }

    def updateTree(tree: Array[Byte]): Option[Array[Byte]] = {
      def updateFields(fieldNumbers: Int*)(message: Array[Byte]) =
        mapMessage(message) {
          case f if fieldNumbers.contains(f.number) => nested(f)(updateTree)
          case f => Some(f)
        }
      mapMessage(tree) {
        case f @ Field(ApplyTree, _) => nested(f)(updateFields(ApplyFunction, ApplyArguments))
        case f @ Field(FunctionTree, _) => nested(f)(updateFields(FunctionBody))
        case f @ Field(MacroExpansionTree, _) => nested(f)(updateFields(BeforeExpansion))
        case f @ Field(OriginalTree, _) => nested(f)(withRange)
        case f @ Field(SelectTree, _) => nested(f)(updateFields(Qualifier))
        case f @ Field(TypeApplyTree, _) => nested(f)(updateFields(TypeApplyFunction))
        case f => Some(f)
      }
    }

    def updateSynthetic(synthetic: Array[Byte]): Option[Array[Byte]] =
      mapMessage(synthetic) {
        case f @ Field(RangeField, _) => nested(f)(mapRange)
        case f @ Field(SyntheticTree, _) => nested(f)(updateTree)
        case f => Some(f)
      }

    def updateDocument(doc: Array[Byte]): Array[Byte] = {
      val fields = Protobuf.parse(doc)
      val kept = fields.flatMap {
        case Field(Uri, _) => Nil
        case Field(Text | Md5, _) if originalCode.nonEmpty => Nil
        // occurrences, diagnostics, and synthetics outside of the original code are dropped
        case f @ Field(Occurrences | Diagnostics, _) => nested(f)(withRange).toList
        case f @ Field(Synthetics, _) => nested(f)(updateSynthetic).toList
        case f => List(f)
      }
      val codeFields = originalCode.toSeq.flatMap { code =>
        Seq(Field(Text, Value.string(code)), Field(Md5, Value.string(md5(code))))
      }
      Protobuf.write(
        (Field(Uri, Value.string(originalPath.toString)) +: codeFields) ++ kept
      )
    }

    if (os.isFile(orig)) {
      val docs = Protobuf.parse(os.read.bytes(orig)).map {
        case Field(Documents, v: Value.LengthDelimited) =>
          Field(Documents, Value.LengthDelimited(updateDocument(v.bytes)))
        case f => f
      }
      os.write.over(dest, Protobuf.write(docs), createFolders = true)
    } else
      System.err.println(s"Error: $orig not found (for $dest)")
  }

  private def md5(content: String): String = {
    val md = MessageDigest.getInstance("MD5")
    val digest = md.digest(content.getBytes(StandardCharsets.UTF_8))
    val res = new BigInteger(1, digest).toString(16)
    if (res.length < 32)
      ("0" * (32 - res.length)) + res
    else
      res
  }
}
