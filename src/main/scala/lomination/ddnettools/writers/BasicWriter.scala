package lomination.ddnettools.writers

import lomination.ddnettools.*

object BasicWriter {

  given Writable[Operator] with
    extension (o: Operator) def write: String = if (o == Operator.Equal) "INDEX" else "NOTINDEX"

  given Writable[Dir] with
    extension (d: Dir)
      def write: String = d match
        case Dir(Sign.+, Times.Zero)   => "NONE"
        case Dir(Sign.+, Times.One)    => "ROTATE"
        case Dir(Sign.+, Times.Two)    => "XFLIP YFLIP"
        case Dir(Sign.+, Times.Three)  => "XFLIP YFLIP ROTATE"
        case Dir(Sign.-, Times.Zero)  => "XFLIP"
        case Dir(Sign.-, Times.One)   => "YFLIP ROTATE"
        case Dir(Sign.-, Times.Two)   => "YFLIP"
        case Dir(Sign.-, Times.Three) => "XFLIP ROTATE"

  given Writable[Pos] with
    extension (p: Pos)
      def write: String = p match
        case Pos(x, y) => s"$x $y"

  given Writable[Cond] with
    extension (c: Cond)
      def write: String = c match
        case Cond(p, o, t: _*) => s"Pos ${p.write} ${o.write} ${t.map(_.write).mkString(" OR ")}"

  given Writable[TileMatcher] with
    extension (t: TileMatcher)
      def write: String = t match
        case TileMatcher(FullTile, dir)     => "FULL"
        case TileMatcher(0, AnyDir)         => "EMPTY"
        case TileMatcher(id: Int, AnyDir)   => s"$id"
        case TileMatcher(id: Int, dir: Dir) => s"$id ${dir.write}"

  given Writable[Tile] with
    extension (t: Tile)
      def write: String = t match
        case Tile(id, d) => s"$id ${d.write}"

  given Writable[Random] with
    extension (r: Random)
      def write: String =
        if (r.value >= 100f) ""
        else
          r.value.toString match
            case s if s.endsWith(".0") => s"Random ${s.dropRight(2)}%\n"
            case s                     => s"Random $s%\n"

  given Writable[Reset] with
    extension (r: Reset)
      def write: String = r match
        case Reset(t, r, a, false) =>
          (for d <- a yield s"Index ${t.rotate(d).write}\n${r.write}").mkString
        case Reset(t, r, a, true) =>
          (for d <- a yield s"Index ${t.rotate(d).write}\nNoDefaultRule\n${r.write}").mkString

  given Writable[Replace] with
    extension (r: Replace)
      def write: String = r match
        case Replace(t, conds, r, a, false) =>
          (
            for d <- a
            yield s"Index ${t.rotate(d).write}\n" + (
              for c <- conds
              yield s"Pos ${c.pos.write} ${c.op.write} ${c.tm.map(_.write).mkString(" OR ")}\n"
            ) + r.write
          ).mkString
        case Replace(t, conds, r, a, true) =>
          (
            for d <- a
            yield s"Index ${t.rotate(d).write}\nNoDefaultRule\n" + (
              for c <- conds
              yield s"Pos ${c.pos.write} ${c.op.write} ${c.tm.map(_.write).mkString(" OR ")}\n"
            ) + r.write
          ).mkString

  given Writable[Shadow] with
    extension (s: Shadow)
      def write: String = s match
        case Shadow(tiles, sD) => {
          val pos = Seq(
            Seq(),
            Seq(new Cond(Pos(1, 0), TileMatcher(FullTile)), new Cond(Pos(0, 1), TileMatcher(FullTile))),
            Seq("1 0" -> "FULL", "0 1" -> "FULL")
          )
          val posWithSD = Seq()
          (for t <- tiles
          yield s"Index s\n").mkString
        }

  given Writable[Rule] with
    extension (r: Rule)
      def write: String =
        val commandOutput = r.cmds
          .map {
            case r: Reset   => r.write
            case r: Replace => r.write
            case s: Shadow  => s.write
          }
          .mkString("\n")
        s"[${r.name}]\n\n$commandOutput"

  given Writable[Autorule] with
    extension (a: Autorule) def write: String = a.rules.map(_.write).mkString("\n")
}
