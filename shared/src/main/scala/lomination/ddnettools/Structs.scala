package lomination.ddnettools

// general
case class RuleFile(defTile: DefaultTile, rules: Seq[Rule])

case class Rule(name: String, cmds: Seq[Command])

// commands
sealed trait Command

case class Replace(
    tiles: Seq[Tile],
    conds: Seq[Cond] = Seq(),
    random: Random = Random.always,
    rotations: Seq[Dir] = Seq(Dir.p0)
) extends Command

case class Shadow(
    tiles: Seq[Tile],
    conds: Seq[Cond] = Seq(),
    shadowType: ShadowType = ShadowType.default
) extends Command

case class Shape(
    applyPat: Grid[Option[Tile]],
    onPat: Grid[Option[Matcher]],
    neutral: Tile,
    random: Random = Random.always,
    rotations: Seq[Dir] = Seq(Dir.p0)
) extends Command

case class Comment(str: String) extends Command

// matchers
sealed trait Matcher:
  def not: Matcher
  def rotate(dir: Dir): Matcher

case class FullMatcher(op: Op) extends Matcher:
  def not: FullMatcher              = FullMatcher(op.not)
  def rotate(dir: Dir): FullMatcher = this

case class GenericMatcher(op: Op, tms: TileMatcher*) extends Matcher:
  def this(tms: TileMatcher*) = this(Op.Is, tms*)
  def not: GenericMatcher              = GenericMatcher(op.not, tms*)
  def rotate(dir: Dir): GenericMatcher = GenericMatcher(op, tms.map(_.rotate(dir))*)

case class TileMatcher(id: Int, dir: Dir | AnyDir.type = AnyDir):
  def rotate(dir: Dir): TileMatcher = this.dir match
    case AnyDir => TileMatcher(id, AnyDir)
    case d: Dir => TileMatcher(id, dir rotate d)

// others
case class Grid[A](rows: Seq[Seq[A]]):
  def apply(x: Int, y: Int) = rows(y)(x)
  def toSeq: Seq[Seq[A]]    = rows
  def sizeX: Int            = rows(0).length
  def sizeY: Int            = rows.length
  def size: (Int, Int)      = (sizeX, sizeY)
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
  def map[B](f: A => B): Grid[B] =
    Grid(rows.map(_.map(f)))

case class Tile(id: Int, dir: Dir = Dir.p0):
  def this(id: Int) = this(id, Dir.p0)
  def rotate(d: Dir): Tile       = Tile(id, d rotate dir)
  def toTileMatcher: TileMatcher = TileMatcher(id, dir)

case class DefaultTile(id: Int, dir: Dir = Dir.p0):
  def toTile: Tile      = Tile(id, dir)
  def toTm: TileMatcher = TileMatcher(id, dir)

case class Cond(pos: Pos, matcher: Matcher):
  /** Does not rotate the matcher! */
  def rotate(dir: Dir): Cond         = Cond(pos.rotate(dir), matcher)
  def &(cond: Cond): Seq[Cond]       = Seq(this, cond)
  def &(conds: Seq[Cond]): Seq[Cond] = this +: conds

extension (seq: Seq[Cond])
  def &(cond: Cond): Seq[Cond]       = seq :+ cond
  def &(conds: Seq[Cond]): Seq[Cond] = seq ++ conds

case class Pos(x: Int, y: Int):
  def +(pos: Pos): Pos = Pos(x + pos.x, y + pos.y)
  def rotate(dir: Dir): Pos = dir match
    case Dir(Sign.+, n) => this.clockwise(n.ordinal)
    case Dir(Sign.-, n) => Pos(-x, y).anticlockwise(n.ordinal)
  def clockwise(n: Int): Pos =
    if (n <= 0) this
    else Pos(-y, x).clockwise(n - 1)
  def anticlockwise(n: Int): Pos =
    if (n <= 0) this
    else Pos(y, -x).anticlockwise(n - 1)
  def is(matcher: Matcher): Cond    = Cond(this, matcher)
  def is(tm: TileMatcher): Cond     = Cond(this, GenericMatcher(Op.Is, tm))
  def isnot(matcher: Matcher): Cond = Cond(this, matcher.not)
  def isnot(tm: TileMatcher): Cond  = Cond(this, GenericMatcher(Op.Isnot, tm))
  def adjacent: Seq[Pos]            = Pos.adjacent.map(this + _)
  def around: Seq[Pos]              = Pos.around.map(this + _)

object Pos:
  val zero: Pos = Pos(0, 0)
  val n: Pos    = Pos(0, -1)
  val ne: Pos   = Pos(1, -1)
  val e: Pos    = Pos(1, 0)
  val se: Pos   = Pos(1, 1)
  val s: Pos    = Pos(0, 1)
  val sw: Pos   = Pos(-1, 1)
  val w: Pos    = Pos(-1, 0)
  val nw: Pos   = Pos(-1, -1)
  ////
  def adjacent: Seq[Pos]               = Seq(n, e, s, w)
  def adjacentExept(pos: Pos*)         = adjacent.filterNot(pos.contains(_))
  def around: Seq[Pos]                 = Seq(n, ne, e, se, s, sw, w, nw)
  def aroundExept(pos: Pos*): Seq[Pos] = around.filterNot(pos.contains(_))

case class Dir(sign: Sign, n: Times):
  def this(i: Int, n: Int) =
    this(if (i >= 0) Sign.+ else Sign.-, Times.fromOrdinal(n % 4))
  def rotate(dir: Dir): Dir = Dir(
    if (sign == dir.sign) Sign.+ else Sign.-,
    Times.fromOrdinal((n.ordinal + dir.n.ordinal) % 4)
  )

object Dir:
  val p0: Dir = Dir(Sign.+, Times.Zero)
  val p1: Dir = Dir(Sign.+, Times.One)
  val p2: Dir = Dir(Sign.+, Times.Two)
  val p3: Dir = Dir(Sign.+, Times.Three)
  val m0: Dir = Dir(Sign.-, Times.Zero)
  val m1: Dir = Dir(Sign.-, Times.One)
  val m2: Dir = Dir(Sign.-, Times.Two)
  val m3: Dir = Dir(Sign.-, Times.Three)

object AnyDir

case class Random(percent: Float) extends AnyVal

object Random:
  val always = Random(100f)

case class ShadowType(extCorner: Boolean, intCorner: Boolean)

object ShadowType:
  val default = ShadowType(false, false)

// enums
enum Sign  { case +, -                  }
enum Times { case Zero, One, Two, Three }
enum Op:
  case Is, Isnot
  def not: Op = if (this == Op.Is) Op.Isnot else Op.Is
