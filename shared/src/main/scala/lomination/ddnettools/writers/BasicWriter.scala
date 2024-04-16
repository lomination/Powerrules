package lomination.ddnettools.writers

import lomination.ddnettools.*
import lomination.ddnettools.Pos.aroundExept
import lomination.ddnettools.build.BuildInfo

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
    extension (p: Pos)
      def write(using DefaultTile): String = s"${p.x} ${p.y}"

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
        if (r.percent >= 100f) ""
        else s"Random ${r.percent.toString().replaceAll("\\.0+", "")}%\n"

  given Writable[Replace] with
    extension (r: Replace)
      def write(using defTile: DefaultTile): String =
        if (r.tiles.sizeIs == 1 && r.rotations.sizeIs == 1)
          s"Index ${r.tiles(0).rotate(r.rotations(0)).write}\n" +
            "NoDefaultRule\n" +
            r.conds.map(_.write).mkString +
            r.random.write +
            "NewRun\n"
        else
          val tLen = r.tiles.length
          val rLen = r.rotations.length
          val tmp = s"Index ${defTile.tile.write}\n" +
            "NoDefaultRule\n" +
            r.conds.map(_.write).mkString +
            r.random.write +
            "NewRun\n"
          val core = (
            for {
              i <- 0 until tLen
              j <- 0 until rLen
            } yield
              val chance = tLen * rLen - i * rLen - j
              s"Index ${r.tiles(i).rotate(r.rotations(j)).write}\n" +
                s"Pos 0 0 INDEX ${defTile.tm.write}\n" +
                (if (chance > 1) s"Random $chance\nNewRun\n" else "")
          ).mkString + "NewRun\n"
          tmp + core

  given Writable[Shadow] with
    extension (sd: Shadow)
      def write(using defTile: DefaultTile): String =
        val tm = defTile.tm
        val conds = sd.shadowType match
          case ShadowType(extCorner, intCorner, false) =>
            import lomination.ddnettools.Pos.{zero as o, n, ne as nE, e, se, s, sw, w, nw, around, adjacent}
            Seq(
              (o is tm) & adjacent.map( _ is tm ),    // square
              (o is tm) & (n isnot tm) & (e is tm)    & (s is tm)    & (w is tm),    // T
              (o is tm) & (n isnot tm) & (e is tm)    & (s isnot tm) & (w is tm),    // -
              (o is tm) & (n isnot tm) & (e is tm)    & (s is tm)    & (w isnot tm), // corner down right "r"
              (o is tm) & (n isnot tm) & (e isnot tm) & (s is tm)    & (w isnot tm), // end of a bar (up i) connection down
              (o is tm) & adjacent.map( _ isnot tm ), // • no connections
            ) ++ (
              if (extCorner) Seq(
                (o isnot tm) & (n is tm) & (e isnot tm) & (s isnot tm) & (w is tm)  & (nw is tm), // corner in top-left corner of the tile
                (o isnot tm) & (n is tm) &  (e is tm)  & (s isnot tm)  & (w is tm)  & (nE is tm)   & (nw is tm), // double corners top-left and top-right                                
                (o isnot tm) & around.map( _ is tm ) // four corners
              )
              else Seq()
            ) ++ (
              if (intCorner) Seq(
                (o is tm) & adjacent.map( _ is tm ) & (nE is tm) & (se is tm) & (sw is tm) & (nw isnot tm), // corner in top-left corner of the tile
                (o is tm) & adjacent.map( _ is tm ) & (nE isnot tm) & (se is tm) & (sw is tm) & (nw isnot tm), // corner in top-left and top-right corner of the tile
                (o is tm) & adjacent.map( _ is tm ) & (nE is tm) & (se isnot tm) & (sw is tm) & (nw isnot tm), // corner in top-left and bottom-right corner of the tile
                (o is tm) & adjacent.map( _ is tm ) & (nE isnot tm) & (se isnot tm) & (sw is tm) & (nw isnot tm), // corner in top-left, top-right and bottom-right corner of the tile
                (o is tm) & adjacent.map( _ is tm ) & (nE isnot tm) & (se isnot tm) & (sw isnot tm) & (nw isnot tm) // corner in top-left, top-right and bottom-right corner of the tile
              )
              else Seq()
            )
          case ShadowType(extCorner, intCorner, true) => ???
        // replace with default tile
        val tmp = s"Index ${defTile.tile.write}\nNoDefaultRule\n" +
          sd.conds.map(_.write).mkString + "NewRun\n"
        // compute shaodw
        val core = s"Index ${sd.tiles(0).write}\nNoDefaultRule\nPos 0 0 INDEX ${tm.write}\n" + (
          for {
            i   <- 1 until sd.tiles.length
            dir <- Seq(Times.Zero, Times.One, Times.Two, Times.Three).map(Dir(Sign.+, _))
          } yield s"Index ${sd.tiles(i).rotate(dir).write}\nNoDefaultRule\n" +
            conds(i).map(_.rotate(dir).write).mkString
        ).mkString + "NewRun\n"
        // return
        tmp + core

  given Writable[Comment] with
    extension (c: Comment)
      def write(using DefaultTile): String =
        c.str + "\n"

  given Writable[Rule] with
    extension (r: Rule)
      def write(using DefaultTile): String =
        s"[${r.name}]\n\n" + r.cmds
          .map {
            case r: Replace => r.write
            case s: Shadow  => s.write
            case c: Comment => c.write
          }
          .mkString("\n")

  given Writable[RuleFile] with
    extension (a: RuleFile)
      def write(using DefaultTile): String =
        s"# Generated with ddnettools (v${BuildInfo.gitDescription}) by lomination\n" +
          "# https://github.com/lomination/ddnettools" + "\n\n\n\n" +
          a.rules.map(_.write).mkString("\n")
}
