package lomination.powerrules.config

import lomination.powerrules.ast

case class Config(
    pIndentation: Int = 4,
    pUseTabs: Boolean = false,
    wTemporaryTile: TmpTile = TmpTile(0xff),
    wSpacing: Level = Level.Normal,
    wVerbose: Level = Level.Least
):

  /** Returns a new Config instance with the given `pIndentation` setting as value */
  def setPIndentation(pIndentation: Int): Config =
    Config(pIndentation, pUseTabs, wTemporaryTile, wSpacing, wVerbose)

  def setPUseTabs(pUseTabs: Boolean): Config =
    Config(pIndentation, pUseTabs, wTemporaryTile, wSpacing, wVerbose)

  def setWTemporaryTile(wTemporaryTile: TmpTile): Config =
    Config(pIndentation, pUseTabs, wTemporaryTile, wSpacing, wVerbose)

  def setWSpacing(wSpacing: Level): Config =
    Config(pIndentation, pUseTabs, wTemporaryTile, wSpacing, wVerbose)

  def setWVerbose(wVerbose: Level): Config =
    Config(pIndentation, pUseTabs, wTemporaryTile, wSpacing, wVerbose)

object Config:
  val default = Config()

/** A temporary tile used in the two-stage processes during command writing */
case class TmpTile(id: Int):

  /** Converts this temporary tile to a regular tile */
  def toTile(dir: ast.Dir = ast.Dir.p0): ast.Tile = ast.Tile(id, dir)

  /** Converts this temporary tile to a regular tile */
  @inline def toTile: ast.Tile = toTile(ast.Dir.p0)

  /** Convert this temporary tile to a tile matcher */
  def toTm(dir: ast.Dir = ast.Dir.p0): ast.TileMatcher = ast.TileMatcher(id, dir)

  /** Convert this temporary tile to a tile matcher */
  @inline def toTm: ast.TileMatcher = toTm(ast.Dir.p0)

  /** Converts this temporary tile to a generic matcher */
  def toGm(dir: ast.Dir = ast.Dir.p0): ast.GenericMatcher = ast.GenericMatcher(ast.TileMatcher(id, dir))

  /** Converts this temporary tile to a generic matcher */
  @inline def toGm: ast.GenericMatcher = ast.GenericMatcher(ast.TileMatcher(id, ast.Dir.p0))

enum Level:
  case Least, Less, Normal, More, Most
