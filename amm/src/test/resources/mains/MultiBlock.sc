// MultiBlock.sc
// Several blocks: with --class-based, later blocks reference the wrappers of
// earlier ones, and those they don't use are null-ified

val a = 1
@
val b = 2
@
val c = b + 1
println(s"c = $c")

// The class-based wrapper of this block holds references to the wrappers of the
// earlier blocks, only those this block uses should be set
val usedWrappers = {
  val outer: AnyRef = MultiBlock_3.this
  outer.getClass.getDeclaredFields.toList
    .filter(_.getName.startsWith("MultiBlock"))
    .map { f =>
      f.setAccessible(true)
      f.getName + (if (f.get(outer) == null) " null" else " set")
    }
    .sorted
}
println(usedWrappers.mkString(", "))
