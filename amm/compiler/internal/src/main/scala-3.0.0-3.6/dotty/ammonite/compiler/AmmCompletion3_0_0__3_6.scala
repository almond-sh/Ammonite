package dotty.ammonite.compiler

import dotty.tools.dotc.ast.tpd._
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Denotations.SingleDenotation
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.Names.Name
import dotty.tools.dotc.core.Phases
import dotty.tools.dotc.core.Symbols.{Symbol, defn}
import dotty.tools.dotc.interactive.{Completion, Interactive}
import dotty.tools.dotc.util.SourcePosition

/** Ammonite's completions, for the compilers of 3.0.0 to 3.6.x. */
object AmmCompletion3_0_0__3_6 {

  def backtick(completion: Completion): Completion =
    Completion.backtickCompletions(completion, completion.label.startsWith("`"))

  def scopeCompletionNames(completer: Completion.Completer)(using
      Context
  ): Map[Name, Seq[SingleDenotation]] =
    completer.scopeCompletions.names.view.mapValues(_.denots).toMap

  def completions(
    pos: SourcePosition,
    dependencyCompleteOpt: Option[String => (Int, Seq[String])],
    enableDeep: Boolean
  )(using Context): (Int, List[Completion]) = {
    val path = Interactive.pathTo(ctx.compilationUnit.tpdTree, pos.span)
    computeCompletions(
      pos,
      path,
      dependencyCompleteOpt,
      enableDeep
    )(using Interactive.contextOfPath(path).withPhase(Phases.typerPhase))
  }

  def computeCompletions(
    pos: SourcePosition,
    path: List[Tree],
    dependencyCompleteOpt: Option[String => (Int, Seq[String])],
    enableDeep: Boolean
  )(using Context): (Int, List[Completion]) = {
    val mode = Completion.completionMode(path, pos)
    val prefix = Completion.completionPrefix(path, pos)
    val completer = new DeepCompleter(mode, prefix, pos)

    var extra = List.empty[Completion]
    var start = pos.span.start - prefix.length

    val completions = path match {
      case Select(qual, _) :: _                              => completer.selectionCompletions(qual)
      case Import(Ident(name), selectors) :: _
        if name.decode.toString == "$ivy" && dependencyCompleteOpt.nonEmpty =>
        // The compiler's completion prefix stops at characters like '.', '-' or ':', which
        // dependencies are full of - we complete the whole selector up to the cursor instead
        val selectorStart = selectors.iterator
          .map(_.span)
          .filter(span => span.exists && span.start <= pos.span.point)
          .map(_.start)
          .maxOption
          .getOrElse(pos.span.point)
        val typed =
          new String(pos.source.content.slice(selectorStart, pos.span.point))
        val input = typed.stripPrefix("`")
        val (inputPos, completions) = dependencyCompleteOpt.get(input)
        val input0 = input.take(inputPos)
        // Always open a backtick, like the Scala 2 completions do: dependencies need one
        // most of the time, and the user is likely to have typed it already anyway
        extra = extra ::: completions.distinct.toList
          .map(s => Completion("`" + input0 + s, "", Nil))
        start = selectorStart
        Map.empty
      case Import(expr, _) :: _                              =>
        completer.directMemberCompletions(expr)
      case (_: untpd.ImportSelector) :: Import(expr, _) :: _ =>
        completer.directMemberCompletions(expr)
      case _                                                 =>
        scopeCompletionNames(completer) ++ {
          if (enableDeep) completer.deepCompletions
          else Nil
        }
    }

    val describedCompletions = extra ++ Completion.describeCompletions(completions).map(backtick)
    val offset = Completion.completionOffset(path)

    (start, describedCompletions)
  }

  class DeepCompleter(
    mode: Completion.Mode,
    prefix: String,
    pos: SourcePosition
  )(using Context) extends Completion.Completer(mode, pos, Nil, n => n.startsWith(prefix)):
    private def blacklisted(s: Symbol) = {
      val blacklist = Set(
        "scala.Predef.any2stringadd.+",
        "scala.Any.##",
        "java.lang.Object.##",
        "scala.<byname>",
        "scala.<empty>",
        "scala.<repeated>",
        "scala.<repeated...>",
        "scala.Predef.StringFormat.formatted",
        "scala.Predef.Ensuring.ensuring",
        "scala.Predef.ArrowAssoc.->",
        "scala.Predef.ArrowAssoc.→",
        "java.lang.Object.synchronized",
        "java.lang.Object.ne",
        "java.lang.Object.eq",
        "java.lang.Object.wait",
        "java.lang.Object.notifyAll",
        "java.lang.Object.notify",
        "java.lang.Object.clone",
        "java.lang.Object.finalize"
      )

      blacklist(s.showFullName) ||
      s.isOneOf(GivenOrImplicit) ||
      // Cache objects, which you should probably never need to
      // access directly, and apart from that have annoyingly long names
      "cache[a-f0-9]{32}".r.findPrefixMatchOf(s.name.decode.toString).isDefined ||
      // s.isDeprecated ||
      s.name.decode.toString == "<init>" ||
      s.name.decode.toString.contains('$')
    }
    def deepCompletions: Map[Name, Seq[SingleDenotation]] = {

      val blacklistedPackages = Set("shaded")

      def allMembers(s: Symbol) =
        try s.info.allMembers
        catch {
          case _: dotty.tools.dotc.core.TypeError => Nil
        }
      def rec(t: Symbol): Seq[Symbol] =
        if (blacklistedPackages(t.name.toString))
          Nil
        else {
          val children =
            if (t.is(Package) || t.is(PackageVal) || t.is(PackageClass)) {
              allMembers(t).map(_.symbol).filter(!blacklisted(_)).filter(_ != t).flatMap(rec)
            } else Nil

          t +: children.toSeq
        }

      val syms = for {
        member <- allMembers(defn.RootClass).map(_.symbol).filter(!blacklisted(_)).toList
        sym <- rec(member)
        if sym.name.toString.startsWith(prefix)
      } yield sym

      syms.map(sym => (sym.fullName, List(sym: SingleDenotation))).toMap
    }

}
