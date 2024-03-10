package lomination.ddnettools

// general
case class Autorule(rules: Seq[Rule])
case class Rule(name: String, cmds: Seq[Command])

// commands
trait Command
case class Random(value: Float) extends AnyVal
object Random { val always = Random(100f) }
case class Clear(tile: Tile, random: Random = Random.always, autorotate: Seq[Dir] = Seq())                     extends Command
case class Reset(tile: Tile, random: Random = Random.always, autorotate: Seq[Dir] = Seq())                     extends Command
case class Replace(tile: Tile, conds: Seq[Cond], random: Random = Random.always, autorotate: Seq[Dir] = Seq()) extends Command
case class Shadow(tiles: Seq[Tile], softdiags: Boolean = false)                                                extends Command
/*case class Shape(scheme: Scheme, matchScheme: Scheme, map: Map[Char, Seq[Tile]], random: Random = Random.always, autorotate: Seq[Dir] = Seq()) extends Command(random, autorotate)*/

// other classes
case class TileMatcher(id: Int | FullTile.type, dir: Dir | AnyDir.type = AnyDir):
  def rotate(dir: Dir): TileMatcher = this.dir match
    case AnyDir => TileMatcher(id, AnyDir)
    case d: Dir => TileMatcher(id, dir & d)

case class Tile(id: Int, dir: Dir):
  def rotate(d: Dir): Tile = Tile(id, d & dir)

case class Cond(pos: Pos, op: Operator, tm: TileMatcher*):
  def this(pos: Pos, tm: TileMatcher*) = this(pos, Operator.Equal, tm: _*)

case class Pos(x: Int, y: Int)

case class Dir(sign: Sign, n: Times):
  def &(dir: Dir): Dir = Dir(
    if (sign == dir.sign) Sign.Plus else Sign.Minus,
    Times.fromOrdinal((n.ordinal + dir.n.ordinal) % 4)
  )

case class Scheme(lines: Seq[Char])

// enums
enum Sign     { case Plus, Minus           }
enum Times    { case Zero, One, Two, Three }
enum Operator { case Equal, NotEqual       }

// objects
object AnyDir
object FullTile
