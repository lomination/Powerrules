package lomination.powerrules.writer

import lomination.powerrules.{AnyDir,Comment,Cond,Dir,FullMatcher,GenericMatcher,NotEdgeMatcher,Op,Pos,Replace,Random,Rule,RuleFile,Shadow,Shape,Sign,Tile,TileMatcher,Times,TmpTile}
import lomination.powerrules.build.BuildInfo
import lomination.powerrules.parser.RuleFileParser.rotateStm

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
          case Cond(pos, NotEdgeMatcher)           => pos.adjacent.map(p => s"Pos ${p.write} NOTINDEX -1\n").mkString
          case Cond(pos, FullMatcher(op))          => s"Pos ${pos.write} ${if (op == Op.Is) "FULL" else "EMPTY"}\n"
          case Cond(pos, GenericMatcher(op, tms*)) => s"Pos ${pos.write} ${op.write} ${tms.map(_.write).mkString(" OR ")}\n"

  given Writable[TileMatcher] with
    extension (tm: TileMatcher)
      def write(using TmpTile): String =
        logger.trace("writing tilematcher")
        tm match
          case TileMatcher(id, AnyDir)   => s"$id"
          case TileMatcher(id, dir: Dir) => s"$id ${dir.write}"

  given Writable[Tile] with
    extension (t: Tile)
      def write(using TmpTile): String =
        logger.trace("writing tile")
        s"${t.id} ${t.dir.write}"

  given Writable[Random] with
    extension (r: Random)
      def write(using TmpTile): String =
        logger.trace("writing random")
        if (r.percent >= 100f) ""
        else s"Random ${r.percent.toString().replaceAll("\\.0+", "")}%\n"

  given Writable[Replace] with
    extension (r: Replace)
      def write(using tmpTile: TmpTile): String =
        logger.trace("writing replace")
        if (r.tiles.sizeIs == 1 && r.rotations.sizeIs == 1)
          s"Index ${r.tiles(0).rotate(r.rotations(0)).write}\n" +
            "NoDefaultRule\n" +
            r.conds.map(_.write).mkString +
            r.random.write +
            "NewRun\n"
        else
          val tLen = r.tiles.length
          val rLen = r.rotations.length
          val tmp = s"Index ${tmpTile.toTile.write}\n" +
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
                s"Pos 0 0 INDEX ${tmpTile.toTm.write}\n" +
                (if (chance > 1) s"Random $chance\n" else "") +
                "NewRun\n"
            ).mkString
          tmp + core

  given Writable[Shadow] with
    extension (sd: Shadow)
      def write(using tmpTile: TmpTile): String =
        logger.trace("writing shadow")
        import lomination.powerrules.writer.{defaultTilesConds, externalTilesConds, internalTilesConds, toConds}
        val tmp =
          s"Index ${tmpTile.toTile.write}\n" +
            "NoDefaultRule\n" +
            sd.conds.map(_.write).mkString +
            "NewRun\n"
        val defT =
          logger.trace("writing shadow's default tiles")
          (
            for {
              (tile, (name, dirs, conds)) <- (sd.defTiles zip defaultTilesConds(sd.softMode)(GenericMatcher(Op.Is, tmpTile.toTm, TileMatcher(-1))))
              dir                         <- dirs
            } yield
              logger.trace(s"writing shadow's default tile $name")
              s"Index ${tile.rotate(dir).write}\n" +
              conds.map(_.rotatePos(dir).write).mkString
          ).mkString
        val extT =
          logger.trace("writing shadow's external tiles")
          (
            for {
              (tile, (name, dirs, conds)) <- (sd.extTiles zip externalTilesConds(sd.softMode)(tmpTile.toGm))
              dir                         <- dirs
            } yield
              logger.trace(s"writing shadow's external tile $name")
              s"Index ${tile.rotate(dir).write}\n" +
              conds.map(_.rotatePos(dir).write).mkString
          ).mkString
        val intT =
          logger.trace("writing shadow's internal tiles")
          (
            for {
              (tile, (name, dirs, conds)) <- (sd.intTiles zip internalTilesConds(sd.softMode)(GenericMatcher(Op.Is, tmpTile.toTm, TileMatcher(-1))))
              dir                         <- dirs
            } yield
              logger.trace(s"writing shadow's internal tile $name")
              s"Index ${tile.rotate(dir).write}\n" +
              conds.map(_.rotatePos(dir).write).mkString
          ).mkString
        tmp + defT + extT + intT + "NewRun\n"

  given Writable[Shape] with
    extension (sp: Shape)
      // def writeBis(using tmpTile: TmpTile): String =
      //   "NewRun\n" +
      //   "NoLayerCopy\n" +
      //   s"Index ${tmpTile.toTile.rotate(sp.rotations.last).write}" +
      //   "NoDefaultRule\n" +
      //   (Pos(-1, -1) isnot TileMatcher(-1)).write +
      //   (Pos(sp.applyPat.xSize, sp.applyPat.ySize) isnot TileMatcher(-1)).write +
      //   (
      //     for {
      //       x <- 0 until sp.onPat.rotate(sp.rotations.last).xSize
      //       y <- 0 until sp.onPat.rotate(sp.rotations.last).ySize
      //       t <- sp.onPat.rotate(sp.rotations.last)(x, y)
      //     } yield (Pos(x, y) is t).write
      //   ).mkString +
      //   sp.random.write +
      //   "NewRun\n"
        


      def write(using tmpTile: TmpTile): String =
        logger.trace("writing shape")
        val tmp =
          s"Index ${tmpTile.toTile.rotate(sp.rotations.last).write}\n" +
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
              (Pos(0, 0) is tmpTile.toTm.rotate(sp.rotations.last)).write +
              (Pos(-x, -y) is tmpTile.toTm.rotate(sp.rotations.last)).write +
              "NewRun\n"
          ).mkString
        val newTmp =
          (
            for {
              i <- 0 until sp.rotations.length - 1
            } yield s"Index ${tmpTile.toTile.rotate(sp.rotations(i)).write}\n" +
              (Pos(0, 0) is tmpTile.toTm.rotate(sp.rotations.last)).write +
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
              (Pos(-x, -y) is tmpTile.toTm.rotate(dir)).write
          ).mkString
        tmp + noOverlaps + (if (sp.rotations.sizeIs > 1) newTmp else "") + core

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
            case re: Replace => re.write
            case sd: Shadow  => sd.write
            case sp: Shape   => sp.write
            case co: Comment => co.write
          }
          .mkString("\n")

  given Writable[RuleFile] with
    extension (rf: RuleFile)
      def write(using TmpTile): String =
        logger.trace("writing rulefile")
        s"# Generated with Powerrules (version ${BuildInfo.version}) by lomination\n" +
          "# https://github.com/lomination/Powerrules" + "\n\n\n\n" +
          rf.rules.map(_.write).mkString("\n")
}
