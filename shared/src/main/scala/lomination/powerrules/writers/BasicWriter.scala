package lomination.powerrules.writers

import lomination.powerrules.*
import lomination.powerrules.build.BuildInfo

object BasicWriter {

  given Writable[Op] with
    extension (o: Op)
      def write(using DefaultTile): String =
        if (o == Op.Is) "INDEX" else "NOTINDEX"

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
        c match
          case Cond(pos, FullMatcher(op))          => s"Pos ${pos.write} ${if (op == Op.Is) "FULL" else "EMPTY"}\n"
          case Cond(pos, NotEdgeMatcher)           => pos.adjacent.map(p => s"Pos ${p.write} NOTINDEX -1\n").mkString
          case Cond(pos, GenericMatcher(op, tms*)) => s"Pos ${pos.write} ${op.write} ${tms.map(_.write).mkString(" OR ")}\n"

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
          val tmp = s"Index ${defTile.toTile.write}\n" +
            "NoDefaultRule\n" +
            r.conds.map(_.write).mkString +
            r.random.write +
            "NewRun\n"
          val core =
            (
              for {
                i <- 0 until tLen
                j <- 0 until rLen
                chance = tLen * rLen - i * rLen - j
              } yield s"Index ${r.tiles(i).rotate(r.rotations(j)).write}\n" +
                s"Pos 0 0 INDEX ${defTile.toTm.write}\n" +
                (if (chance > 1) s"Random $chance\n" else "") +
                "NewRun\n"
            ).mkString
          tmp + core

  given Writable[Shadow] with
    extension (sd: Shadow)
      def write(using defTile: DefaultTile): String =
        val tm  = defTile.toTm
        val tmB = GenericMatcher(Op.Is, defTile.toTm, TileMatcher(-1))
        val defConds =
          import lomination.powerrules.Pos.{zero as o, n, ne as nE, e, se, s, sw, w, nw, around, adjacent}
          import lomination.powerrules.Dir.{p0, p1, p2, p3, m0, m1, m2, m3}
          Seq(
            (Seq(p0), (o is tmB)             & adjacent.map(_ is tmB)), // d1
            (Seq(p0, p1, p2, p3), (o is tmB) & (n isnot tmB) & (e is tmB)    & (s is tmB)    & (w is tmB)),    // d2
            (Seq(p0, p1, p2, p3), (o is tmB) & (n isnot tmB) & (e is tmB)    & (s is tmB)    & (w isnot tmB)), // d3
            (Seq(p0, p1), (o is tmB)         & (n isnot tmB) & (e is tmB)    & (s isnot tmB) & (w is tmB)),    // d4
            (Seq(p0, p1, p2, p3), (o is tmB) & (n isnot tmB) & (e isnot tmB) & (s is tmB)    & (w isnot tmB)), // d5
            (Seq(p0), (o is tmB)             & adjacent.map(_ isnot tmB)) // d6
          )
        val extConds =
          import lomination.powerrules.Pos.{zero as o, n, ne as nE, e, se, s, sw, w, nw, around, adjacent}
          import lomination.powerrules.Dir.{p0, p1, p2, p3, m0, m1, m2, m3}
          Seq(
            (Seq(p0, p1, p2, p3), (o isnot tm) & (n is tm) & (e isnot tm) & (s isnot tm) & (w is tm) & (nw is tm)), // e1
            (Seq(p0, p1, p2, p3), (o isnot tm) & (n is tm) & (e is tm)    & (s isnot tm) & (w is tm) & (nE is tm) & (nw is tm)), // e2
            (Seq(p0), (o isnot tm)             & around.map(_ is tm)) // e3
          )
        val intConds =
          import lomination.powerrules.Pos.{zero as o, n, ne as nE, e, se, s, sw, w, nw, around, adjacent}
          import lomination.powerrules.Dir.{p0, p1, p2, p3, m0, m1, m2, m3}
          Seq(
            (Seq(p0, p1, p2, p3), (o is tmB)                 & adjacent.map(_ is tmB) & (nE is tmB)    & (se is tmB)    & (sw is tmB)    & (nw isnot tmB)), // i1
            (Seq(p0, p1, p2, p3), (o is tmB)                 & adjacent.map(_ is tmB) & (nE isnot tmB) & (se is tmB)    & (sw is tmB)    & (nw isnot tmB)), // i2
            (Seq(p0, p1), (o is tmB)                         & adjacent.map(_ is tmB) & (nE is tmB)    & (se isnot tmB) & (sw is tmB)    & (nw isnot tmB)), // i3
            (Seq(p0, p1, p2, p3), (o is tmB)                 & adjacent.map(_ is tmB) & (nE isnot tmB) & (se isnot tmB) & (sw is tmB)    & (nw isnot tmB)), // i4
            (Seq(p0), (o is tmB)                             & adjacent.map(_ is tmB) & (nE isnot tmB) & (se isnot tmB) & (sw isnot tmB) & (nw isnot tmB)), // i5
            (Seq(p0, p1, p2, p3), (o is tmB)                 & (n isnot tmB)          & (e is tmB)     & (s is tmB)     & (w isnot tmB)  & (se isnot tmB)), // i6
            (Seq(p0, p1, p2, p3, m0, m1, m2, m3), (o is tmB) & (n isnot tmB)          & (e is tmB)     & (s is tmB)     & (w is tmB)     & (se is tmB)    & (sw isnot tmB)), // i7
            (Seq(p0, p1, p2, p3), (o is tmB)                 & (n isnot tmB)          & (e is tmB)     & (s is tmB)     & (w is tmB)     & (se isnot tmB) & (sw isnot tmB))  // i8
          )
        val allConds = defConds
          ++ (if (sd.shadowType.extCorner) extConds else Seq())
          ++ (if (sd.shadowType.intCorner) intConds else Seq())

        s"Index ${defTile.toTile.write}\n" +
          "NoDefaultRule\n" +
          sd.conds.map(_.write).mkString +
          "NewRun\n" +
          (
            for {
              (tile, (dirs, conds)) <- (sd.tiles zip allConds)
              d                     <- dirs
            } yield s"Index ${tile.rotate(d).write}\n" +
              conds.map(_.rotate(d).write).mkString
          ).mkString + "NewRun\n"

  given Writable[Shape] with
    extension (sp: Shape)
      def write(using defTile: DefaultTile): String =
        val tmp =
          s"Index ${defTile.toTile.rotate(sp.rotations.last).write}\n" +
            "NoDefaultRule\n" +
            (Pos(-1, -1) isnot TileMatcher(-1)).write +
            (Pos(sp.applyPat.xSize, sp.applyPat.ySize) isnot TileMatcher(-1)).write +
            (
              for {
                x <- 0 until sp.onPat.xSize
                y <- 0 until sp.onPat.ySize
                t <- sp.onPat(x, y)
              } yield (Pos(x, y) is t).write
            ).mkString +
            sp.random.write +
            "NewRun\n"
        val noOverlaps =
          (
            for {
              x <- (-sp.onPat.xSize + 1) until sp.onPat.xSize
              y <- (-sp.onPat.ySize + 1) until sp.onPat.ySize
              if (!(x >= 0 && y >= 0))
            } yield s"Index ${sp.neutral.write}\n" +
              (Pos(0, 0) is defTile.toTm.rotate(sp.rotations.last)).write +
              (Pos(-x, -y) is defTile.toTm.rotate(sp.rotations.last)).write +
              "NewRun\n"
          ).mkString
        val newTmp =
          (
            for {
              i <- 0 until sp.rotations.length - 1
            } yield s"Index ${defTile.toTile.rotate(sp.rotations(i)).write}\n" +
              (Pos(0, 0) is defTile.toTm.rotate(sp.rotations.last)).write +
              s"Random ${sp.rotations.length - i}\n" +
              "NewRun\n"
          ).mkString
        val core =
          (
            for {
              dir <- sp.rotations
              pattern = sp.applyPat.rotate(dir)
              x <- 0 until pattern.xSize
              y <- 0 until pattern.ySize
              t <- pattern(x, y)
            } yield s"Index ${t.rotate(dir).write}\n" +
              (Pos(-x, -y) is defTile.toTm.rotate(dir)).write
          ).mkString
        tmp + noOverlaps + (if (sp.rotations.sizeIs > 1) newTmp else "") + core

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
        s"# Generated with Powerrules (version ${BuildInfo.version}) by lomination\n" +
          "# https://github.com/lomination/Powerrules" + "\n\n\n\n" +
          rf.rules.map(_.write).mkString("\n")
}
