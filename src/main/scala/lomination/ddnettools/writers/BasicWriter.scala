package lomination.ddnettools.writers

import lomination.ddnettools.*

object BasicWriter {

  given Writable[Operator] with
    extension (o: Operator)
      def write(using DefaultTile): String =
        if (o == Operator.Equal) "INDEX" else "NOTINDEX"

  given Writable[Dir] with
    extension (d: Dir)
      def write(using DefaultTile): String = d match
        case Dir(Sign.+, Times.Zero)  => "NONE"
        case Dir(Sign.+, Times.One)   => "ROTATE"
        case Dir(Sign.+, Times.Two)   => "XFLIP YFLIP"
        case Dir(Sign.+, Times.Three) => "XFLIP YFLIP ROTATE"
        case Dir(Sign.-, Times.Zero)  => "XFLIP"
        case Dir(Sign.-, Times.One)   => "YFLIP ROTATE"
        case Dir(Sign.-, Times.Two)   => "YFLIP"
        case Dir(Sign.-, Times.Three) => "XFLIP ROTATE"

  given Writable[Pos] with
    extension (p: Pos) def write(using DefaultTile): String = s"${p.x} ${p.y}"

  given Writable[Cond] with
    extension (c: Cond)
      def write(using DefaultTile): String =
        s"Pos ${c.pos.write} ${c.matcher.write}\n"

  given Writable[Matcher] with
    extension (m: Matcher)
      def write(using DefaultTile): String = m match
        case FullMatcher              => "FULL"
        case EmptyMatcher             => "EMPTY"
        case GenericMatcher(op, tms*) => s"${op.write} ${tms.map(_.write).mkString(" OR ")}"

  given Writable[TileMatcher] with
    extension (tm: TileMatcher)
      def write(using DefaultTile): String = tm match
        case TileMatcher(id, AnyDir)   => s"$id"
        case TileMatcher(id, dir: Dir) => s"$id ${dir.write}"

  given Writable[Tile] with
    extension (t: Tile) def write(using DefaultTile): String = s"${t.id} ${t.dir.write}"

  given Writable[Random] with
    extension (r: Random)
      def write(using DefaultTile): String =
        if (r.value >= 100f) ""
        else
          r.value.toString match
            case s if s.endsWith(".0") => s"Random ${s.dropRight(2)}%\n"
            case s                     => s"Random $s%\n"

  given Writable[Replace] with
    extension (r: Replace)
      def write(using tmp: DefaultTile): String =
        val len = r.autorotate.length
        if (len == 1)
          s"Index ${r.tile.rotate(r.autorotate(0)).write}\n" +
            "NoDefaultRule\n" +
            r.conds.map(_.write).mkString +
            r.random.write
        else
          s"Index ${tmp.tile.write}\n" +
            "NoDefaultRule\n" +
            r.conds.map(_.write).mkString +
            r.random.write +
            Range(0, len).map { i =>
              s"Index ${r.tile.rotate(r.autorotate(i)).write}\n" +
                s"Pos 0 0 INDEX ${tmp.tm.write}\n" +
                (if (len - i > 1) s"Random ${len - i}\n" else "")
            }.mkString + "NewRun\n"

  given Writable[Shadow] with
    extension (s: Shadow)
      def write(using tmp: DefaultTile): String =
        val tm = tmp.tm
        val conds =
          if (s.softDiags)
            Seq()
          else
            Seq(
              Seq(Pos.zero is tm, Pos.n is tm, Pos.e is tm, Pos.s is tm, Pos.w is tm),
              Seq(Pos.zero is tm, Pos.n isnot tm, Pos.e is tm, Pos.s is tm, Pos.w isnot tm),
              Seq(Pos.zero is tm, Pos.n isnot tm, Pos.e isnot tm, Pos.s is tm, Pos.w isnot tm),
              Seq(),
              Seq(Pos.zero is tm, Pos.n isnot tm, Pos.s isnot tm, Pos.w isnot tm, Pos.e isnot tm),
              Seq(),
              Seq(Pos.zero isnot tm, Pos.n is tm, Pos.e isnot tm, Pos.s isnot tm, Pos.w is tm, Pos.nw is tm),
              Seq(Pos.zero isnot tm, Pos.n is tm, Pos.ne is tm, Pos.e is tm, Pos.s isnot tm, Pos.w isnot tm, Pos.nw isnot tm),
              Seq(Pos.zero isnot tm, Pos.n is tm, Pos.ne is tm, Pos.e is tm, Pos.se is tm, Pos.s is tm, Pos.sw isnot tm, Pos.w is tm, Pos.nw is tm),
              (Pos.zero isnot tm) +: Pos.distFrom1.map(_ is tm),
              Seq()
            )
        s"Index ${tmp.tile.write}\nNoDefaultRule\n" +
          s.conds.map(_.write).mkString + "NewRun\n" +
          s"Index ${s.tiles(0).write}\nNoDefaultRule\nPos 0 0 INDEX ${tm.write}\n" + (
            for {
              i   <- 1 until s.tiles.length
              dir <- Seq(Times.Zero, Times.One, Times.Two, Times.Three).map(Dir(Sign.+, _))
            } yield
              if (conds(i).length == 0) ""
              else s"Index ${s.tiles(i).rotate(dir).write}\nNoDefaultRule\n" +
                conds(i).map(c => (c.pos.rotate(dir) is c.matcher).write).mkString
          ).mkString + "NewRun\n"

  given Writable[Rule] with
    extension (r: Rule)
      def write(using DefaultTile): String =
        s"[${r.name}]\n\n" + r.cmds
          .map {
            case r: Replace => r.write
            case s: Shadow  => s.write
          }
          .mkString("\n")

  given Writable[Autorule] with
    extension (a: Autorule)
      def write(using DefaultTile): String =
        "# Generated with ddnettools (v0.1) by lomination\n" +
          "# https://github.com/lomination/ddnettools" + "\n\n\n\n" +
          a.rules.map(_.write).mkString("\n")
}
