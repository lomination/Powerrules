package lomination.ddnettools.parser

import lomination.ddnettools.{Tile, Cond, Random, Dir, ShadowType, Grid, Matcher}

trait Statements

case class WithStm(tiles: Seq[Tile])                extends Statements
case class IfStm(conds: Seq[Cond])                  extends Statements
case class RandomStm(chance: Random)                extends Statements
case class RotateStm(rotations: Seq[Dir])           extends Statements
case class TypeStm(sdType: ShadowType)              extends Statements
case class ApplyStm(chars: Grid[Char])              extends Statements
case class OnStm(chars: Grid[Char])                 extends Statements
case class UsingStm(map: Map[Char, Tile | Matcher]) extends Statements
case class NeutralStm(tile: Tile)                   extends Statements
