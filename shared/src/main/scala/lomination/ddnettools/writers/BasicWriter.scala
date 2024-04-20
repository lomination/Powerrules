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
        val defConds =
          import lomination.ddnettools.Pos.{zero as o, n, ne as nE, e, se, s, sw, w, nw, around, adjacent}
          import lomination.ddnettools.Dir.{p0, p1, p2, p3, m0, m1, m2, m3}
          Seq(
            (Seq(p0), (o is tm)             & adjacent.map(_ is tm)), // square
            (Seq(p0, p1, p2, p3), (o is tm) & (n isnot tm) & (e is tm)    & (s is tm)    & (w is tm)),    // T
            (Seq(p0, p1), (o is tm)         & (n isnot tm) & (e is tm)    & (s isnot tm) & (w is tm)),    // -
            (Seq(p0, p1, p2, p3), (o is tm) & (n isnot tm) & (e is tm)    & (s is tm)    & (w isnot tm)), // corner down right "r"
            (Seq(p0, p1, p2, p3), (o is tm) & (n isnot tm) & (e isnot tm) & (s is tm)    & (w isnot tm)), // end of a bar (up i) connection down
            (Seq(p0), (o is tm)             & adjacent.map(_ isnot tm)) // • no connections
          )
        val extConds =
          import lomination.ddnettools.Pos.{zero as o, n, ne as nE, e, se, s, sw, w, nw, around, adjacent}
          import lomination.ddnettools.Dir.{p0, p1, p2, p3, m0, m1, m2, m3}
          Seq(
            (Seq(p0, p1, p2, p3), (o isnot tm) & (n is tm) & (e isnot tm) & (s isnot tm) & (w is tm) & (nw is tm)), // corner in top-left corner of the tile
            (Seq(p0, p1, p2, p3), (o isnot tm) & (n is tm) & (e is tm)    & (s isnot tm) & (w is tm) & (nE is tm) & (nw is tm)), // double corners top-left and top-right
            (Seq(p0), (o isnot tm)             & around.map(_ is tm)) // four corners
          )
        val intConds =
          import lomination.ddnettools.Pos.{zero as o, n, ne as nE, e, se, s, sw, w, nw, around, adjacent}
          import lomination.ddnettools.Dir.{p0, p1, p2, p3, m0, m1, m2, m3}
          Seq(
            (Seq(p0, p1, p2, p3), (o is tm)                 & adjacent.map(_ is tm) & (nE is tm)    & (se is tm)    & (sw is tm)    & (nw isnot tm)), // corner in top-left corner of the tile
            (Seq(p0, p1, p2, p3), (o is tm)                 & adjacent.map(_ is tm) & (nE isnot tm) & (se is tm)    & (sw is tm)    & (nw isnot tm)), // corner in top-left and top-right corner of the tile
            (Seq(p0, p1), (o is tm)                         & adjacent.map(_ is tm) & (nE is tm)    & (se isnot tm) & (sw is tm)    & (nw isnot tm)), // corner in top-left and bottom-right corner of the tile
            (Seq(p0, p1, p2, p3), (o is tm)                 & adjacent.map(_ is tm) & (nE isnot tm) & (se isnot tm) & (sw is tm)    & (nw isnot tm)), // corner in top-left top-right and bottom-right corner of the tile
            (Seq(p0), (o is tm)                             & adjacent.map(_ is tm) & (nE isnot tm) & (se isnot tm) & (sw isnot tm) & (nw isnot tm)), // 4 corners of the tile
            (Seq(p0, p1, p2, p3), (o is tm)                 & (n isnot tm)          & (e is tm)     & (s is tm)     & (w isnot tm)  & (se isnot tm)), // tunnel corner
            (Seq(p0, p1, p2, p3, m0, m1, m2, m3), (o is tm) & (n isnot tm)          & (e is tm)     & (s is tm)     & (w is tm)     & (se is tm)    & (sw isnot tm)), // T only one bottom left corner
            (Seq(p0, p1, p2, p3), (o is tm)                 & (n isnot tm)          & (e is tm)     & (s is tm)     & (w is tm)     & (se isnot tm) & (sw isnot tm))  // T both
          )
        val allConds = defConds
          ++ (if (sd.shadowType.extCorner) extConds else Seq())
          ++ (if (sd.shadowType.intCorner) intConds else Seq())

        s"Index ${defTile.tile.write}\nNoDefaultRule\n"
          + sd.conds.map(_.write).mkString + "NewRun\n"
          + (
            for {
              (tile, (dirs, conds)) <- (sd.tiles zip allConds)
              d                     <- dirs
            } yield s"Index ${tile.rotate(d).write}\nNoDefaultRule\n"
              + conds.map(_.rotate(d).write).mkString
          ).mkString + "NewRun\n"

  given Writable[Shape] with
    extension (sp: Shape)
      def write(using defTile: DefaultTile): String = ???
      // val tmp = s"Index ${defTile.tile.write}\nNoDefaultRule\n" +
      //   (
      //     for {
      //       x <- 0 until sp.oldPattern.xSize
      //       y <- 0 until sp.oldPattern.ySize
      //     } yield (Pos(x, y) is sp.oldPattern(x, y)).write
      //   ).mkString +
      //   "NewRun\n"
      // val noOverlaps = "Index 0\n" + (
      //   for {
      //     x <- 1 until sp.oldPattern.xSize
      //     y <- 1 until sp.oldPattern.ySize
      //   } yield (Pos(0, 0) is defTile.tm).write +
      //     (Pos(-x, -y) is defTile.tm).write
      // )
      // val core = ???
      // tmp + noOverlaps + core

  given Writable[Comment] with
    extension (c: Comment)
      def write(using DefaultTile): String =
        c.str + "\n"

  given Writable[Rule] with
    extension (r: Rule)
      def write(using DefaultTile): String =
        s"[${r.name}]\n\n" + r.cmds
          .map {
            case re: Replace => re.write
            case sd: Shadow  => sd.write
            case sp: Shape   => sp.write
            case co: Comment => co.write
          }
          .mkString("\n")

  given Writable[RuleFile] with
    extension (rf: RuleFile)
      def write(using DefaultTile): String =
        s"# Generated with ddnettools (${BuildInfo.version}) by lomination\n" +
          "# https://github.com/lomination/ddnettools" + "\n\n\n\n" +
          rf.rules.map(_.write).mkString("\n")
}
