package lomination.powerrules.ast

// For more information about the following classes, please refer to the wiki

// ---------- General ---------- //

/** A powerrules file
  *
  * @param tmpTile
  *   the tile used as temporary tile
  * @param rules
  *   a not empty sequence of rules
  */
case class RuleFile(rules: Seq[Rule])

/** A powerrules rule, equivalent to a DDNet rule
  *
  * @param name
  *   the name of the rule
  * @param cmds
  *   the commands it contains
  */
case class Rule(name: String, cmds: Seq[Command])

// ---------- Commands ---------- //

/** Commands trait */
sealed trait Command

/** A replace command
  *
  * @param tiles
  *   a non empty sequence of tiles (`Seq[Tile]`) that corresponds to the 'with' statement
  * @param conds
  *   a sequence of condition (`Seq[Cond]`) that corresponds to the 'if' statement. By default set to empty sequence `Seq()`
  * @param random
  *   a percent (`Random`) that corresponds to the 'random' statement. By default set to `Random(100)`
  * @param rotations
  *   a non empty sequence of direction (`Seq[Dir]`) that corresponds to the 'rotate' statement. By default set to `Seq(Dir.p0)`
  */
case class Replace(
    tiles: Seq[Tile],
    conds: Seq[Cond] = Seq(),
    random: Random = Random.always
) extends Command

/** A shadow command
  *
  * @param defTiles
  *   a non empty sequence of tiles (`Seq[Tile]`) that correponds to the 'with' statement
  * @param extTiles
  *   a sequence of tiles (`Seq[Tile]`) that correponds to the 'withexternal' statement. By default set to empty sequence `Seq()`
  * @param intTiles
  *   a sequence of tiles (`Seq[Tile]`) that correponds to the 'withinternal' statement. By default set to empty sequence `Seq()`
  * @param conds
  *   a sequence of conditions (`Seq[Cond]`) that correponds to the 'if' statement. By default set to empty sequence `Seq()`
  * @param softMode
  *   a boolean (`Boolean`) that correponds to the 'mode' statement. By default set to `false`
  */
case class Shadow(
    defTiles: Seq[Tile],
    extTiles: Seq[Tile] = Seq(),
    intTiles: Seq[Tile] = Seq(),
    conds: Seq[Cond] = Seq(),
    softMode: Boolean = false
) extends Command

/** A shape command
  *
  * @param applyPat
  *   a pattern of options of tile (`Seq[Option[Tile]])` that corresponds to the `apply` statement. In the case of `Some(Tile)`, the command will
  *   place the given tile, else (`None`) it won't change anything at the position.
  * @param onPat
  *   a pattern of options of tile matchers (`Seq[Option[TileMatcher]])` that corresponds to the `on` statement. In the case of `Some(TileMatcher)`,
  *   the command will test the given tile matcher, else (`None`) it won't test anything at the position.
  * @param neutral
  *   a tile used to clear shapes' overlaps
  * @param random
  *   a percent (`Random`) that corresponds to the 'random' statement. By default set to `Random(100)`
  * @param rotations
  *   non empty sequence of direction (`Seq[Dir]`) that corresponds to the 'rotate' statement. By default set to `Seq(Dir.p0)`
  */
case class Shape(
    applyPat: Grid[Option[Tile]],
    onPat: Grid[Option[Pos => Cond]],
    random: Random = Random.always
) extends Command

case class Comment(str: String) extends Command

// ---------- Conditions ---------- //

/** Represents a test required for a command to be executed
  *
  * @param pos
  *   the relative position that must match
  * @param matcher
  *   the matcher the given position must match
  */
case class Cond(pos: Pos, op: Op, matcher: Matcher):
  /** Rotates this condition's position and matcher */
  def rotate(dir: Dir): Cond = Cond(pos.rotate(dir), op, matcher.rotate(dir))

  /** Rotates this condition's position */
  def rotatePos(dir: Dir): Cond = Cond(pos.rotate(dir), op, matcher)

  /** Returns a sequence of conditions containing this and that */
  def &(that: Cond): Seq[Cond] = Seq(this, that)

  /** Returns the given sequence of conditions preceded by this */
  def &(that: Seq[Cond]): Seq[Cond] = this +: that

extension (seq: Seq[Cond])
  /** Appends the given condition to this */
  def &(that: Cond): Seq[Cond] = seq :+ that

  /** Appends the given conditions to this */
  def &(that: Seq[Cond]): Seq[Cond] = seq ++ that

/** A position with coordinates */
case class Pos(x: Int, y: Int):
  /** Adds the x of this and that, and the y of this and that */
  def +(that: Pos): Pos = Pos(x + that.x, y + that.y)

  /** Rotates this position around `Pos(0 0)` by the given direction */
  def rotate(dir: Dir): Pos = dir match
    case Dir(Sign.+, n) => this.clockwise(n.ordinal)
    case Dir(Sign.-, n) => Pos(-x, y).anticlockwise(n.ordinal)

  /** Rotates this position clockwise by 90 degrees `n` times */
  def clockwise(n: Int): Pos =
    if (n <= 0) this
    else Pos(-y, x).clockwise(n - 1)

  /** Rotates this position anticlockwise by 90 degrees `n` times */
  def anticlockwise(n: Int): Pos =
    if (n <= 0) this
    else Pos(y, -x).anticlockwise(n - 1)

  /** Returns a condition containing this position and the given matcher */
  def is(matcher: Matcher): Cond = Cond(this, Op.Is, matcher)

  /** Returns a condition containing this position and the given tile matcher converted into a generic matcher */
  def is(tm: TileMatcher): Cond = Cond(this, Op.Is, GenericMatcher(tm))

  /** Returns a condition containing this position and the opposite of the given matcher */
  def isnot(matcher: Matcher): Cond = Cond(this, Op.IsNot, matcher)

  /** Returns a condition containing this position and the opposite of the given tile matcher converted into a generic matcher */
  def isnot(tm: TileMatcher): Cond = Cond(this, Op.IsNot, GenericMatcher(tm))

  /** Returns a sequence of all adjacent positions to this one (seq lenth = 4) */
  def adjacent: Seq[Pos] = Pos.adjacent.map(this + _)

  /** Returns a sequence of all around positions to this one (seq lenth = 8) */
  def around: Seq[Pos] = Pos.around.map(this + _)

case object Pos:
  /** Returns `Pos(0, 0)` */
  val zero: Pos = Pos(0, 0)

  /** Returns `Pos(0, -1)` */
  val n: Pos = Pos(0, -1)

  /** Returns `Pos(1, -1)` */
  val ne: Pos = Pos(1, -1)

  /** Returns `Pos(1, 0)` */
  val e: Pos = Pos(1, 0)

  /** Returns `Pos(1, 1)` */
  val se: Pos = Pos(1, 1)

  /** Returns `Pos(0, 1)` */
  val s: Pos = Pos(0, 1)

  /** Returns `Pos(-1, 1)` */
  val sw: Pos = Pos(-1, 1)

  /** Returns `Pos(-1, 0)` */
  val w: Pos = Pos(-1, 0)

  /** Returns `Pos(-1, -1)` */
  val nw: Pos = Pos(-1, -1)

  /** Returns a sequence of all adjacent positions to the `Pos(0, 0)` (seq length = 4) */
  def adjacent: Seq[Pos] = Seq(n, e, s, w)

  /** Returns a sequence of all around positions to the `Pos(0, 0)` (seq length = 8) */
  def around: Seq[Pos] = Seq(n, ne, e, se, s, sw, w, nw)

/** Matchers can test whether given positions contain a certain tile or not */
sealed trait Matcher:
  /** Rotates the matcher by the given direction */
  def rotate(dir: Dir): Matcher

/** Tests whether the given position contains a tile with an index diferent or equal to 0
  * @param op
  *   defines whether the position should (`Op.Is`) or should not (`Op.IsNot`) contain a tile with an index diferent from 0. In other words: `Op.Is`
  *   \=> full matcher (matches index != 0); `Op.IsNot` => empty matcher (matches index == 0)
  */
object FullMatcher extends Matcher:
  /** Returns this */
  def rotate(dir: Dir): FullMatcher.type = this

object EmptyMatcher extends Matcher:
  /** Returns this */
  def rotate(dir: Dir): EmptyMatcher.type = this

/** Maches when the given position is not on the edge of the tile layer
  *
  * @note
  *   It is negative, so use `<pos> is NotEdgeMatcher`. Using `<pos> isnot NotEdgeMatcher` will throw an UnsupportedOperationException
  */
case object EdgeMatcher extends Matcher:
  /** Returns this */
  def rotate(dir: Dir): EdgeMatcher.type = this

/** Tests whether the given position matches one of the given tile matchers
  * @param op
  *   defines whether the position should (`Op.Is`) or should not (`Op.IsNot`) match one of the given tile matchers
  *
  * @param tms
  *   tile matchers to match
  */
case class GenericMatcher(tms: TileMatcher*) extends Matcher:
  /** Rotates the given tile matchers */
  def rotate(dir: Dir): GenericMatcher = GenericMatcher(tms.map(_.rotate(dir))*)

/** Matches a tile with a certain index and a certain direction
  *
  * @param id
  *   the index of the tile to match. From -1 to 255
  * @param dir
  *   the direction og the tile to match or `AnyDir` if the direction should not be tested
  */
case class TileMatcher(id: Int, dir: Dir | AnyDir.type = AnyDir):
  /** Rotates the direction of this tile matcher
    *
    * @param dir
    *   the direction to rotate this tile matcher
    * @return
    *   the new tile matcher with a rotated direction. In case in the case of `AnyDir`, return this
    */
  def rotate(dir: Dir): TileMatcher = this.dir match
    case AnyDir => TileMatcher(id, AnyDir)
    case d: Dir => TileMatcher(id, dir rotate d)

// ---------- Others ---------- //

/** A two dimensional generic sequence
  *
  * @param rows
  *   a sequence of sequence of objects which corresponds to a sequence of rows (not columns)
  */
case class Grid[A](rows: Seq[Seq[A]]):
  val logger     = org.log4s.getLogger
  val ySize: Int = rows.length
  val xSize: Int = rows(0).length
  check

  /** Checks if the grid is rectangular */
  private def check: Unit =
    for (i <- 1 until ySize) yield
      if (rows(i).sizeIs > xSize)
        logger.warn(
          s"Grid has line $i longer than its first one ($xSize) but should be rectangular. The remaining elements won't be taken into consideration"
        )
      else if (rows(i).sizeIs < xSize)
        val msg       = s"Grid has line $i shorter than its first one ($xSize) but must be rectangular"
        val exception = IndexOutOfBoundsException(msg)
        logger.error(exception)(msg)
        throw exception

  /** Gets the element at the given coordinates
    * @return
    *   `rows(y)(x)`
    */
  def apply(x: Int, y: Int) = rows(y)(x)

  /** Rotates this grid by the given direction */
  def rotate(dir: Dir): Grid[A] =
    dir match
      case Dir(Sign.+, Times.Zero)  => this
      case Dir(Sign.+, Times.One)   => Grid(rows.transpose.map(_.reverse))
      case Dir(Sign.+, Times.Two)   => Grid(rows.map(_.reverse).reverse)
      case Dir(Sign.+, Times.Three) => Grid(rows.map(_.reverse).transpose)
      case Dir(Sign.-, Times.Zero)  => Grid(rows.map(_.reverse))
      case Dir(Sign.-, Times.One)   => Grid(rows.transpose)
      case Dir(Sign.-, Times.Two)   => Grid(rows.reverse)
      case Dir(Sign.-, Times.Three) => Grid(rows.transpose.map(_.reverse).reverse)

  /** Maps `f` on this grid's element as a non-nested sequence */
  def map[B](f: A => B): Grid[B] =
    Grid(rows.map(_.map(f)))

/** A tile
  *
  * @param id
  *   this tile's index (between 0 and 255)
  * @param dir
  *   this tile's direction
  */
case class Tile(id: Int, dir: Dir = Dir.p0):
  /** Rotates this tile's direction */
  def rotate(d: Dir): Tile = Tile(id, d rotate dir)

  /** Converts this tile to a tile matcher */
  def toTileMatcher: TileMatcher = TileMatcher(id, dir)

/** A direction */
case class Dir(sign: Sign, n: Times):
  /** Rotates this direction by the given direction. This method is associative and commutatives */
  def rotate(dir: Dir): Dir = Dir(
    if (sign == dir.sign) Sign.+ else Sign.-,
    Times.fromOrdinal((n.ordinal + dir.n.ordinal) % 4)
  )

case object Dir:
  /** Plus zero direction
    * @return
    *   `Dir(Sign.+, Times.Zero)`
    */
  val p0: Dir = Dir(Sign.+, Times.Zero)

  /** Plus one direction
    * @return
    *   `Dir(Sign.+, Times.One)`
    */
  val p1: Dir = Dir(Sign.+, Times.One)

  /** Plus two direction
    * @return
    *   `Dir(Sign.+, Times.Two)`
    */
  val p2: Dir = Dir(Sign.+, Times.Two)

  /** Plus three direction
    * @return
    *   `Dir(Sign.+, Times.Three)`
    */
  val p3: Dir = Dir(Sign.+, Times.Three)

  /** Minus zero direction
    * @return
    *   `Dir(Sign.-, Times.Zero)`
    */
  val m0: Dir = Dir(Sign.-, Times.Zero)

  /** Minus one direction
    * @return
    *   `Dir(Sign.-, Times.One)`
    */
  val m1: Dir = Dir(Sign.-, Times.One)

  /** Minus two direction
    * @return
    *   `Dir(Sign.-, Times.Two)`
    */
  val m2: Dir = Dir(Sign.-, Times.Two)

  /** Minus three direction
    * @return
    *   `Dir(Sign.-, Times.Three)`
    */
  val m3: Dir = Dir(Sign.-, Times.Three)

  /** Returns a sequence of all positive directions */
  def positive: Seq[Dir] = Seq(p0, p1, p2, p3)

  /** Returns a sequence of all negative directions */
  def negative: Seq[Dir] = Seq(m0, m1, m2, m3)

  /** Returns a sequence of all directions */
  def all: Seq[Dir] = Seq(p0, p1, p2, p3, m0, m1, m2, m3)

/** Any direction object */
case object AnyDir

/** Random chance in percent */
case class Random(percent: Float) extends AnyVal

case object Random:
  /** The chance corresponding to 100% */
  val always = Random(100f)

// ---------- Enums ---------- //

/** Direction sign */
enum Sign:
  case +, -

/** Direction times */
enum Times:
  case Zero, One, Two, Three

/** Operator */
enum Op:
  case Is, IsNot

  /** Returns the opposite of this operator */
  def not: Op = if (this == Op.Is) Op.IsNot else Op.Is
