package lomination.ddnettools.writers

import lomination.ddnettools.*

object BasicWriter {

  given Writable[Operator] with
    extension (o: Operator) def write: String = if (o == Operator.Equal) "INDEX" else "NOTINDEX"

  given Writable[Dir] with
    extension (d: Dir)
      def write: String = d match
        case Dir(Sign.Plus, Times.Zero)   => "NONE"
        case Dir(Sign.Plus, Times.One)    => "ROTATE"
        case Dir(Sign.Plus, Times.Two)    => "XFLIP YFLIP"
        case Dir(Sign.Plus, Times.Three)  => "XFLIP YFLIP ROTATE"
        case Dir(Sign.Minus, Times.Zero)  => "XFLIP"
        case Dir(Sign.Minus, Times.One)   => "YFLIP ROTATE"
        case Dir(Sign.Minus, Times.Two)   => "YFLIP"
        case Dir(Sign.Minus, Times.Three) => "XFLIP ROTATE"

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
        case Tile(id, d) => s"$id $d"

  given Writable[Clear] with
    extension (c: Clear)
      def write: String = c match
        case Clear(t, r, a) =>
          (
            for d <- (a :+ Dir(Sign.Plus, Times.Zero))
            yield s"Index ${t.rotate(d).write}\nNoDefaultRule\n" + (if (r >= 100f) s"Random $r%\n" else "")
          ).mkString

  given Writable[Reset] with
    extension (r: Reset)
      def write: String = r match
        case Reset(t, r, a) =>
          (
            for d <- (a :+ Dir(Sign.Plus, Times.Zero))
            yield s"Index ${t.rotate(d).write}\n" + (if (r >= 100f) s"Random $r%\n" else "")
          ).mkString

  given Writable[Replace] with
    extension (r: Replace)
      def write: String = r match
        case Replace(t, conds, r, a) =>
          (
            for d <- (a :+ Dir(Sign.Plus, Times.Zero))
            yield s"Index ${t.rotate(d).write}\n" + (
              for c <- conds
              yield s"Pos ${c.pos.write} ${c.op.write} ${c.tm.map(_.write).mkString(" OR ")}\n"
            ) + (if (r >= 100f) s"Random $r%\n" else "")
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
}
