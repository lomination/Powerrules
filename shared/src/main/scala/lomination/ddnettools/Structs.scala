package lomination.ddnettools

// general
case class Autorule(tmp: DefaultTile, rules: Seq[Rule])

case class Rule(name: String, cmds: Seq[Command])

// commands
sealed trait Command

case class Replace(tile: Tile, conds: Seq[Cond] = Seq(), random: Random = Random.always, autorotate: Seq[Dir] = Seq(Dir.default)) extends Command

case class Shadow(tiles: Seq[Tile], conds: Seq[Cond] = Seq(), shadowType: ShadowType = ShadowType.Default) extends Command

case class Comment(str: String) extends Command

// matchers
sealed trait Matcher:
  def not: Matcher
  def rotate(dir: Dir): Matcher

object FullMatcher extends Matcher:
  def not: Matcher                       = EmptyMatcher
  def rotate(dir: Dir): FullMatcher.type = this

object EmptyMatcher extends Matcher:
  def not: Matcher                        = FullMatcher
  def rotate(dir: Dir): EmptyMatcher.type = this

case class GenericMatcher(op: Operator, tms: TileMatcher*) extends Matcher:
  def this(tms: TileMatcher*) = this(Operator.Equal, tms*)
  def not: Matcher                     = GenericMatcher(Operator.fromOrdinal((op.ordinal + 1) % 2))
  def rotate(dir: Dir): GenericMatcher = GenericMatcher(op, tms.map(_.rotate(dir))*)

case class TileMatcher(id: Int, dir: Dir | AnyDir.type = Dir.default):
  def rotate(dir: Dir): TileMatcher = this.dir match
    case AnyDir => TileMatcher(id, AnyDir)
    case d: Dir => TileMatcher(id, dir rotate d)

// others
case class Tile(id: Int, dir: Dir = Dir.default):
  def this(id: Int) = this(id, Dir.default)
  def rotate(d: Dir): Tile = Tile(id, d rotate dir)

case class DefaultTile(id: Int, dir: Dir = Dir.default):
  val tile: Tile      = Tile(id, dir)
  val tm: TileMatcher = TileMatcher(id, dir)

case class Cond(pos: Pos, matcher: Matcher):
  /** `rotate` does not rotate the macher! */
  def rotate(dir: Dir): Cond = Cond(pos.rotate(dir), matcher)
  def &(that: Cond): Seq[Cond] = Seq(this, that)

extension (conds: Seq[Cond]) def &(c: Cond): Seq[Cond] = conds :+ c

case class Pos(x: Int, y: Int):
  def rotate(dir: Dir): Pos = dir match
    case Dir(Sign.+, n) => this.clockwise(n.ordinal)
    case Dir(Sign.-, n) => this.anticlockwise(n.ordinal)
  def clockwise(n: Int): Pos =
    if (n <= 0) this
    else Pos(-y, x).clockwise(n - 1)
  def anticlockwise(n: Int): Pos =
    if (n <= 0) this
    else Pos(y, -x).anticlockwise(n - 1)
  def is(matcher: Matcher): Cond =
    Cond(this, matcher)
  def is(tm: TileMatcher): Cond =
    Cond(this, GenericMatcher(Operator.Equal, tm))
  def isnot(matcher: Matcher): Cond =
    Cond(this, matcher.not)
  def isnot(tm: TileMatcher): Cond =
    Cond(this, GenericMatcher(Operator.NotEqual, tm))

object Pos {
  val zero: Pos           = Pos(0, 0)
  val n: Pos              = Pos(0, -1)
  val ne: Pos             = Pos(1, -1)
  val e: Pos              = Pos(1, 0)
  val se: Pos             = Pos(1, 1)
  val s: Pos              = Pos(0, 1)
  val sw: Pos             = Pos(-1, 1)
  val w: Pos              = Pos(-1, 0)
  val nw: Pos             = Pos(-1, -1)
  val adjacent: Seq[Pos]  = Seq(n, e, s, w)
  val distFrom1: Seq[Pos] = Seq(n, ne, e, se, s, sw, w, nw)
}

case class Dir(sign: Sign, n: Times):
  def this(i: Int, n: Int) =
    this(if (i >= 0) Sign.+ else Sign.-, Times.fromOrdinal(n % 4))
  def rotate(dir: Dir): Dir = Dir(
    if (sign == dir.sign) Sign.+ else Sign.-,
    Times.fromOrdinal((n.ordinal + dir.n.ordinal) % 4)
  )

object Dir {
  val default: Dir = Dir(Sign.+, Times.Zero)
  val p0: Dir      = Dir(Sign.+, Times.Zero)
  val p1: Dir      = Dir(Sign.+, Times.One)
  val p2: Dir      = Dir(Sign.+, Times.Two)
  val p3: Dir      = Dir(Sign.+, Times.Three)
  val m0: Dir      = Dir(Sign.-, Times.Zero)
  val m1: Dir      = Dir(Sign.-, Times.One)
  val m2: Dir      = Dir(Sign.-, Times.Two)
  val m3: Dir      = Dir(Sign.-, Times.Three)
}

object AnyDir

case class Random(value: Float) extends AnyVal

object Random { val always = Random(100f) }

// enums
enum Sign       { case +, -                                   }
enum Times      { case Zero, One, Two, Three                  }
enum Operator   { case Equal, NotEqual                        }
enum ShadowType { case NoOutsideCorner, Default, SoftDigonals }
