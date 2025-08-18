package lomination.powerrules.parsing

import lomination.powerrules.ast.{Cond, Grid, Pos, Random, Tile}

sealed trait Statement

case class WithStm(tiles: Seq[Tile])                      extends Statement
case class WithExternalStm(tiles: Seq[Tile])              extends Statement
case class WithInternalStm(tiles: Seq[Tile])              extends Statement
case class IfStm(conds: Seq[Cond])                        extends Statement
case class RandomStm(chance: Random)                      extends Statement
case class ModeStm(softMode: Boolean)                     extends Statement
case class ApplyStm(chars: Grid[Char])                    extends Statement
case class OnStm(chars: Grid[Char])                       extends Statement
case class UsingStm(map: Map[Char, Tile | (Pos => Cond)]) extends Statement
