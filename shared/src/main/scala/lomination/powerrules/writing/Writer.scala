package lomination.powerrules.writing

import lomination.powerrules.ast._
import lomination.powerrules.build.BuildInfo
import lomination.powerrules.config.Config

object Writer {

  val logger = org.log4s.getLogger

  def apply(ruleFile: RuleFile)(using config: Config): String =
    ruleFile.w

  def usingDefaultRule(conds: Seq[Cond])(using Config): String =
    def isDefault(cond: Cond): Boolean =
      cond match
        case Cond(Pos.zero, Op.Is, FullMatcher)     => true
        case Cond(Pos.zero, Op.IsNot, EmptyMatcher) => true
        case _                                      => false
    def isZeroPositive(cond: Cond): Boolean =
      cond match
        case Cond(Pos.zero, Op.Is, _)               => true
        case Cond(Pos.zero, Op.IsNot, EmptyMatcher) => true
        case _                                      => false
    if (conds.exists(!isZeroPositive(_)))
      "NoDefaultRule\n" + conds.map(_.w).mkString
    else if (conds.exists(!isDefault(_)))
      conds.map(_.w).mkString
    else
      ""

  private def intRandom(chance: Int): String =
    if (chance > 1) s"Random $chance\n"
    else ""

  // ---------- Ast extensions ---------- //

  given Writable[RuleFile] with
    extension (rf: RuleFile)
      def write(using Config): String =
        logger.trace("writing rulefile")
        s"# Generated with Powerrules (version ${BuildInfo.version}) by lomination\n" +
          "# https://github.com/lomination/Powerrules" + "\n\n\n\n" +
          rf.rules.map(_.w).mkString("\n")

  given Writable[Rule] with
    extension (r: Rule)
      def write(using Config): String =
        logger.trace("writing rule")
        s"[${r.name}]\n\n" + r.cmds
          .map {
            case re: Replace => re.w
            case sd: Shadow  => sd.w
            case sp: Shape   => sp.w
            case co: Comment => co.w
          }
          .mkString("\n")

  given Writable[Replace] with
    extension (re: Replace)
      def write(using config: Config): String =
        logger.trace("writing replace")
        if (re.tiles.sizeIs == 1)
          // no need of a temporary tile
          s"Index ${re.tiles(0).w}\n" +
            usingDefaultRule(re.conds) +
            re.random.w +
            "NewRun\n"
        else
          // use a temporary tile
          val tmpTile = config.wTemporaryTile
          val len     = re.tiles.length
          val tmp =
            s"Index ${tmpTile.toTile.w}\n" +
              usingDefaultRule(re.conds) +
              re.random.w +
              "NewRun\n"
          val core =
            (for {
              i <- 0 until len
              tile = re.tiles(i)
            } yield s"Index ${tile.w}\n" +
              s"Pos 0 0 INDEX ${tmpTile.toTm.w}\n" +
              intRandom(len - i) + // write random as integer (n => 1/n probability)
              "NewRun\n").mkString
          tmp + core

  given Writable[Shadow] with
    extension (sd: Shadow)
      def write(using config: Config): String =
        logger.trace("writing shadow")
        import lomination.powerrules.writing.{defaultTilesConds, externalTilesConds, internalTilesConds}
        val tmpTile = config.wTemporaryTile
        val tmpT =
          // place the temporary tile where the shadow command should be applied
          s"Index ${tmpTile.toTile.w}\n" +
            usingDefaultRule(sd.conds) +
            "NewRun\n"
        val defT =
          // place the default tiles (see the Shadow page of the wiki)
          logger.trace("writing shadow's default tiles")
          (
            for {
              (tile, (name, dirs, conds)) <- sd.defTiles zip defaultTilesConds(sd.softMode)(GenericMatcher(tmpTile.toTm, TileMatcher(-1)))
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
              (tile, (name, dirs, conds)) <- sd.extTiles zip externalTilesConds(sd.softMode)(tmpTile.toGm)
              dir                         <- dirs
            } yield
              logger.trace(s"writing shadow's external tile $name")
              s"Index ${tile.rotate(dir).w}\n" +
                "NoDefaultRule\n" +
                conds.map(_.rotatePos(dir).w).mkString
          ).mkString
        val intT =
          // place the internal tiles (see the Shadow page of the wiki)
          logger.trace("writing shadow's internal tiles")
          (
            for {
              (tile, (name, dirs, conds)) <- sd.intTiles zip internalTilesConds(sd.softMode)(GenericMatcher(tmpTile.toTm, TileMatcher(-1)))
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
      def write(using config: Config): String =
        logger.trace("writing shape")
        val tmpTile = config.wTemporaryTile
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
                c <- sp.onPat(x, y)
              } yield c(Pos(x, y)).w
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
      def write(using Config): String =
        logger.trace("writing comment")
        s"#${c.str.stripTrailing}\n"

  given Writable[Cond] with
    extension (c: Cond)
      def write(using Config): String =
        logger.trace("writing condition")
        c match
          case Cond(pos, Op.IsNot, EdgeMatcher)    => pos.adjacent.map(p => s"Pos ${p.w} NOTINDEX -1\n").mkString
          case Cond(pos, _, EdgeMatcher)           => throw Exception("Cannot write \"is edge\"")
          case Cond(pos, op, FullMatcher)          => s"Pos ${pos.w} ${if (op == Op.Is) "FULL" else "EMPTY"}\n"
          case Cond(pos, op, EmptyMatcher)         => s"Pos ${pos.w} ${if (op == Op.Is) "EMPTY" else "FULL"}\n"
          case Cond(pos, op, GenericMatcher(tms*)) => s"Pos ${pos.w} ${op.w} ${tms.map(_.w).mkString(" OR ")}\n"

  given Writable[Pos] with
    extension (p: Pos)
      def write(using Config): String =
        logger.trace("writing position")
        s"${p.x} ${p.y}"

  given Writable[TileMatcher] with
    extension (tm: TileMatcher)
      def write(using Config): String =
        logger.trace("writing tilematcher")
        tm match
          case TileMatcher(id, AnyDir)   => s"$id"
          case TileMatcher(id, dir: Dir) => s"$id ${dir.w}"

  given Writable[Op] with
    extension (o: Op)
      def write(using Config): String =
        logger.trace("writing operator")
        if (o == Op.Is) "INDEX" else "NOTINDEX"

  given Writable[Tile] with
    extension (t: Tile)
      def write(using Config): String =
        logger.trace("writing tile")
        s"${t.id} ${t.dir.w}"

  given Writable[Dir] with
    extension (d: Dir)
      def write(using Config): String =
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

  given Writable[Random] with
    extension (r: Random)
      def write(using Config): String =
        logger.trace("writing random")
        if (r.percent >= 100f) ""
        else s"Random ${r.percent.toString().replaceAll("\\.0+", "")}%\n"

}
