package lomination.powerrules.writing

import lomination.powerrules.{AnyDir, Comment, Cond, Dir, FullMatcher, GenericMatcher, NotEdgeMatcher, Op, Pos, Random, Replace, Rule, RuleFile, Shadow, Shape, Sign, Tile, TileMatcher, Times, TmpTile}
import lomination.powerrules.build.BuildInfo

object BasicWriter {

  val logger = org.log4s.getLogger

  given Writable[Op] with
    extension (o: Op)
      def write(using TmpTile): String =
        logger.trace("writing operator")
        if (o == Op.Is) "INDEX" else "NOTINDEX"

  given Writable[Dir] with
    extension (d: Dir)
      def write(using TmpTile): String =
        logger.trace("writing direction")
        d match
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
      def write(using TmpTile): String =
        logger.trace("writing position")
        s"${p.x} ${p.y}"

  given Writable[Cond] with
    extension (c: Cond)
      def write(using TmpTile): String =
        logger.trace("writing condition")
        c match
          case Cond(pos, NotEdgeMatcher)           => pos.adjacent.map(p => s"Pos ${p.w} NOTINDEX -1\n").mkString
          case Cond(pos, FullMatcher(op))          => s"Pos ${pos.w} ${if (op == Op.Is) "FULL" else "EMPTY"}\n"
          case Cond(pos, GenericMatcher(op, tms*)) => s"Pos ${pos.w} ${op.w} ${tms.map(_.w).mkString(" OR ")}\n"

  given Writable[TileMatcher] with
    extension (tm: TileMatcher)
      def write(using TmpTile): String =
        logger.trace("writing tilematcher")
        tm match
          case TileMatcher(id, AnyDir)   => s"$id"
          case TileMatcher(id, dir: Dir) => s"$id ${dir.w}"

  given Writable[Tile] with
    extension (t: Tile)
      def write(using TmpTile): String =
        logger.trace("writing tile")
        s"${t.id} ${t.dir.w}"

  given Writable[Random] with
    extension (r: Random)
      def write(using TmpTile): String =
        logger.trace("writing random")
        if (r.percent >= 100f) ""
        else s"Random ${r.percent.toString().replaceAll("\\.0+", "")}%\n"

  given Writable[Replace] with
    extension (re: Replace)
      def write(using tmpTile: TmpTile): String =
        logger.trace("writing replace")
        if (re.tiles.sizeIs == 1)
          // no need of a temporary tile
          s"Index ${re.tiles(0).write}\n" +
            noDefaultRule(re.conds) +
            re.conds.map(_.write).mkString +
            re.random.write +
            "NewRun\n"
        else
          // use a temporary tile
          val len = re.tiles.length
          val tmp =
            s"Index ${tmpTile.toTile.write}\n" +
              noDefaultRule(re.conds) +
              re.conds.map(_.write).mkString +
              re.random.write +
              "NewRun\n"
          val core =
            (for {
              i <- 0 until len
              tile = re.tiles(i)
            } yield s"Index ${tile.write}\n" +
              s"Pos 0 0 INDEX ${tmpTile.toTm.write}\n" +
              intRandom(len - i) + // write random as integer (n => 1/n probability)
              "NewRun\n").mkString
          tmp + core

  given Writable[Shadow] with
    extension (sd: Shadow)
      def write(using tmpTile: TmpTile): String =
        logger.trace("writing shadow")
        import lomination.powerrules.writing.{defaultTilesConds, externalTilesConds, internalTilesConds, toConds}
        val tmpT =
          // place the temporary tile where the shadow command should be applied
          s"Index ${tmpTile.toTile.write}\n" +
            noDefaultRule(sd.conds) +
            sd.conds.map(_.write).mkString +
            "NewRun\n"
        val defT =
          // place the default tiles (see the Shadow page of the wiki)
          logger.trace("writing shadow's default tiles")
          (
            for {
              (tile, (name, dirs, conds)) <- (sd.defTiles zip defaultTilesConds(sd.softMode)(GenericMatcher(Op.Is, tmpTile.toTm, TileMatcher(-1)))) // consider ouside tiles (-1) as full tiles
              dir                         <- dirs
            } yield
              logger.trace(s"writing shadow's default tile $name")
              s"Index ${tile.rotate(dir).w}\n" +
                conds.map(_.rotatePos(dir).w).mkString
          ).mkString
        val extT =
          // place the external tiles (see the Shadow page of the wiki)
          logger.trace("writing shadow's external tiles")
          (
            for {
              (tile, (name, dirs, conds)) <- (sd.extTiles zip externalTilesConds(sd.softMode)(tmpTile.toGm))
              dir                         <- dirs
            } yield
              logger.trace(s"writing shadow's external tile $name")
              s"Index ${tile.rotate(dir).write}\n" +
                "NoDefaultRule\n" +
                conds.map(_.rotatePos(dir).write).mkString
          ).mkString
        val intT =
          // place the internal tiles (see the Shadow page of the wiki)
          logger.trace("writing shadow's internal tiles")
          (
            for {
              (tile, (name, dirs, conds)) <- (sd.intTiles zip internalTilesConds(sd.softMode)(GenericMatcher(Op.Is, tmpTile.toTm, TileMatcher(-1))))
              dir                         <- dirs
            } yield
              logger.trace(s"writing shadow's internal tile $name")
              s"Index ${tile.rotate(dir).w}\n" +
                conds.map(_.rotatePos(dir).w).mkString
          ).mkString
        // run
        tmpT + defT + extT + intT + "NewRun\n"

  given Writable[Shape] with
    extension (sp: Shape)
      def write(using tmpTile: TmpTile): String =
        logger.trace("writing shape")
        val tmp =
          "NoLayerCopy\n" +
            s"Index ${tmpTile.toTile.w}\n" +
            "NoDefaultRule\n" +
            (Pos(-1, -1) isnot TileMatcher(-1)).w +
            (Pos(sp.applyPat.xSize, sp.applyPat.ySize) isnot TileMatcher(-1)).w +
            (
              for {
                x <- 0 until sp.onPat.xSize
                y <- 0 until sp.onPat.ySize
                m <- sp.onPat(x, y)
              } yield (Pos(x, y) is m).w
            ).mkString +
            sp.random.w +
            "NewRun\n"
        val core =
          (
            for {
              x <- 0 until sp.applyPat.xSize
              y <- 0 until sp.applyPat.ySize
              t <- sp.applyPat(x, y)
            } yield s"Index ${t.w}\n" +
              (Pos(-x, -y) is tmpTile.toTm).w
          ).mkString
        tmp + core

  given Writable[Comment] with
    extension (c: Comment)
      def write(using TmpTile): String =
        logger.trace("writing comment")
        c.str.trim + "\n"

  given Writable[Rule] with
    extension (r: Rule)
      def write(using TmpTile): String =
        logger.trace("writing rule")
        s"[${r.name}]\n\n" + r.cmds
          .map {
            case re: Replace => re.w
            case sd: Shadow  => sd.w
            case sp: Shape   => sp.w
            case co: Comment => co.w
          }
          .mkString("\n")

  given Writable[RuleFile] with
    extension (rf: RuleFile)
      def write(using TmpTile): String =
        logger.trace("writing rulefile")
        s"# Generated with Powerrules (version ${BuildInfo.version}) by lomination\n" +
          "# https://github.com/lomination/Powerrules" + "\n\n\n\n" +
          rf.rules.map(_.write).mkString("\n")

  /** Writes a `NoDefaultRule` keyword if one is needed. It can be determined using the given conditions */
  private def noDefaultRule(conds: Seq[Cond]): String =
    val needDefaultRule = conds.forall {
      _ match
        case Cond(Pos.zero, GenericMatcher(Op.Is, _)) => false // no need a default rule
        case Cond(Pos.zero, FullMatcher(Op.Is))       => false // no need a default rule
        case _                                        => true  // could need a default rule
    }
    if (needDefaultRule) "NoDefaultRule\n" else ""

  private def intRandom(chance: Int): String =
    if (chance > 1) s"Random $chance\n"
    else ""
}
