package ammonite.interp

import ammonite.TestRepl
import ammonite.runtime.Protobuf
import ammonite.runtime.Protobuf.{Field, Value}
import ammonite.util.Res
import utest._

object SemanticDbTests extends TestSuite {

  // Bits of the SemanticDB model, see
  // https://github.com/scalameta/scalameta/blob/v4.15.2/semanticdb/semanticdb/semanticdb.proto
  case class Range(startLine: Int, startCharacter: Int)
  case class Occurrence(range: Option[Range], symbol: String, role: Int)
  case class TextDocument(uri: String, text: String, occurrences: Seq[Occurrence])
  object Role {
    val REFERENCE = 1
    val DEFINITION = 2
  }

  private def string(fields: Seq[Field], number: Int): String =
    fields.collectFirst { case Field(`number`, v: Value.LengthDelimited) => v.string }
      .getOrElse("")
  private def int(fields: Seq[Field], number: Int): Int =
    fields.collectFirst { case Field(`number`, Value.VarInt(v)) => v.toInt }.getOrElse(0)
  private def messages(fields: Seq[Field], number: Int): Seq[Seq[Field]] =
    fields.collect { case Field(`number`, v: Value.LengthDelimited) => v.message }

  def parseDocuments(bytes: Array[Byte]): Seq[TextDocument] =
    messages(Protobuf.parse(bytes), 1).map { doc =>
      val occurrences = messages(doc, 6).map { occ =>
        val range = messages(occ, 1).headOption.map(r => Range(int(r, 1), int(r, 2)))
        Occurrence(range, string(occ, 2), int(occ, 3))
      }
      TextDocument(string(doc, 2), string(doc, 3), occurrences)
    }

  def withRepl[T](mapSemanticDbsToSources: Boolean)(f: (TestRepl, os.Path) => T): T = {
    val sessionDir = os.temp.dir(prefix = "amm-semanticdb-tests")
    try {
      val repl = new TestRepl(
        sessionDirectory = Some(sessionDir),
        semanticDbs = true,
        mapSemanticDbsToSources = mapSemanticDbsToSources
      )
      f(repl, sessionDir)
    } finally os.remove.all(sessionDir)
  }

  def run(repl: TestRepl, code: String): Unit = {
    val (res, _, _, _, errors, _) = repl.run(code, repl.currentLine)
    assert(errors.isEmpty)
    assert(res.isInstanceOf[Res.Success[_]])
  }

  def readDocument(path: os.Path): TextDocument = {
    val docs = parseDocuments(os.read.bytes(path))
    assert(docs.length == 1)
    docs.head
  }

  def occurrence(doc: TextDocument, symbolSuffix: String, role: Int) =
    doc.occurrences
      .find(o => o.symbol.endsWith(symbolSuffix) && o.role == role)
      .getOrElse(throw new Exception(s"$symbolSuffix not found in ${doc.occurrences}"))

  val tests = Tests {
    test("generatedSources") {
      withRepl(mapSemanticDbsToSources = false) { (repl, sessionDir) =>
        run(repl, "val a = 2")
        run(repl, "val b = a + 1")

        val rel = os.rel / "frame-0" / "generated-sources" / "ammonite" / "$sess" / "cmd1.scala"
        val semanticDb = sessionDir / "frame-0" / "classes" / "META-INF" / "semanticdb" /
          rel.segments.init / s"${rel.last}.semanticdb"
        assert(os.isFile(semanticDb))
        val doc = readDocument(semanticDb)
        assert(doc.uri == rel.toString)

        // positions are those of the generated code
        val generated = os.read(sessionDir / rel)
        val line = generated.linesIterator.indexWhere(_.contains("val b = a + 1"))
        val bDef = occurrence(doc, "cmd1.b.", Role.DEFINITION)
        assert(bDef.range.exists(_.startLine == line))

        // nothing left in the staging directory
        val staged = os.walk(sessionDir / ".semanticdb").filter(os.isFile(_))
        assert(staged.isEmpty)
      }
    }
    test("mappedToSources") {
      withRepl(mapSemanticDbsToSources = true) { (repl, sessionDir) =>
        run(repl, "val a = 2")
        run(repl, "val b = a + 1")

        val rel = os.rel / "frame-0" / "sources" / "ammonite" / "$sess" / "cmd1.sc"
        val semanticDb = sessionDir / "frame-0" / "classes" / "META-INF" / "semanticdb" /
          rel.segments.init / s"${rel.last}.semanticdb"
        assert(os.isFile(semanticDb))
        val doc = readDocument(semanticDb)
        assert(doc.uri == rel.toString)
        assert(doc.text == os.read(sessionDir / rel))

        // positions are those of the .sc file
        val bDef = occurrence(doc, "cmd1.b.", Role.DEFINITION)
        assert(bDef.range.exists(r => r.startLine == 0 && r.startCharacter == 4))
        val aRef = occurrence(doc, "cmd0.a.", Role.REFERENCE)
        assert(aRef.range.exists(r => r.startLine == 0 && r.startCharacter == 8))
        // and nothing from the wrapper code
        assert(doc.occurrences.forall(_.range.forall(_.startLine == 0)))

        // the SemanticDB of the generated code isn't kept
        val generatedSemanticDb =
          sessionDir / "frame-0" / "classes" / "META-INF" / "semanticdb" / "frame-0" /
            "generated-sources"
        assert(!os.exists(generatedSemanticDb))
      }
    }
  }
}
