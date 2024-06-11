package lomination.powerrules.parser

import lomination.powerrules.{Cond, Dir, GenericMatcher, Grid, Random, Tile}

trait Statement

case class WithStm(tiles: Seq[Tile])                       extends Statement
case class WithExternalStm(tiles: Seq[Tile])               extends Statement
case class WithInternalStm(tiles: Seq[Tile])               extends Statement
case class IfStm(conds: Seq[Cond])                         extends Statement
case class RandomStm(chance: Random)                       extends Statement
case class RotateStm(rotations: Seq[Dir])                  extends Statement
case class ModeStm(softMode: Boolean)                      extends Statement
case class ApplyStm(chars: Grid[Char])                     extends Statement
case class OnStm(chars: Grid[Char])                        extends Statement
case class UsingStm(map: Map[Char, Tile | GenericMatcher]) extends Statement
case class NeutralStm(tile: Tile)                          extends Statement
