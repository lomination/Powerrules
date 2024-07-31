// package lomination.powerrules.parser

// package lomination.powerrules.parser

// import scala.util.Try
// import scala.util.parsing.combinator.*
// import scala.util.parsing.input.Position

// abstract class Token { val pos: Position }

// case class NEWLINE(pos: Position, indents: Int) extends Token
// case class SINGLECOMMENT(pos: Position, value: String) extends Token
// case class MULTICOMMENT(pos: Position, value: String) extends Token
// case class DEF(pos: Position) extends Token
// case class WORD(pos: Position, content: String) extends Token // Contains alpha numeric or _ values
// case class LEFTPARENTHESE(pos: Position) extends Token
// case class RIGHTPARENTHESE(pos: Position) extends Token
// case class COMMA(pos: Position) extends Token
// case class EQUAL(pos: Position) extends Token
// case class DOUBLEQUOTE(pos: Position) extends Token
// case class LEFTCHEVRON(pos: Position) extends Token
// case class RIGHTCHEVRON(pos: Position) extends Token
// case class RULENAME(pos: Position, name: String) extends Token
// case class REPLACE(pos: Position) extends Token
// case class SHADOW(pos: Position) extends Token
// case class SHAPE(pos: Position) extends Token
// case class COMMENT(pos: Position, content: String) extends Token
// case class WITH(pos: Position) extends Token
// case class WITHEXTERNAL(pos: Position) extends Token
// case class WITHINTERNAL(pos: Position) extends Token
// case class IF(pos: Position) extends Token
// case class RANDOM(pos: Position) extends Token
// case class ROTATE(pos: Position) extends Token
// case class MODE(pos: Position) extends Token
// case class APPLY(pos: Position) extends Token
// case class ON(pos: Position) extends Token
// case class USING(pos: Position) extends Token
// case class NEUTRAL(pos: Position) extends Token
// case class THERE(pos: Position) extends Token
// case class COORDINATES(pos: Position, x: Int, y: Int) extends Token
// case class CARDINAL(pos: Position, points: String) extends Token
// case class IS(pos: Position) extends Token
// case class ISNOT(pos: Position) extends Token
// case class INDEX(pos: Position, value: Int) extends Token
// case class DIR(pos: Position, dir: String) extends Token
// case class PIPE(pos: Position) extends Token
// case class STAR(pos: Position) extends Token
// case class FULL(pos: Position) extends Token
// case class EMPTY(pos: Position) extends Token
// case class EDGE(pos: Position) extends Token
// case class PERCENT(pos: Position, value: Double) extends Token
// case class NORMAL(pos: Position) extends Token
// case class SOFT(pos: Position) extends Token
// case class PATTERNCHAR(pos: Position) extends Token
// case class ARROW(pos: Position) extends Token

// object Lexer extends RegexParsers {

//   override def skipWhitespace: Boolean     = false

//   val logger = org.log4s.getLogger

//   type P[T] = Parser[T]

//   def apply(input: String): Try[Seq[Token]] =
//     parseAll(rep(parseToken), input) match
//       case Success(result, next) =>
//         logger.info("Lexer succeed")
//         scala.util.Success(result)
//       case Error(msg, next) =>
//         val exception = ParsingError(s"Fail to parse rule file (at l:${next.pos.line}, c:${next.pos.column}).\n\n" + msg)
//         logger.error(exception)("Lexer generated an error")
//         scala.util.Failure(exception)
//       case Failure(msg, next) =>
//         val exception = ParsingError(s"Fail to parse rule file (at l:${next.pos.line}, c:${next.pos.column}).\n\n" + msg)
//         logger.error(exception)("Lexer generated a failure")
//         scala.util.Failure(exception)

//   extension [T](p: P[T])
//     def pos: Position ~ T = ???

//   lazy val parseToken: P[Token] =
//     newline | singlecomment | multicomment | `def` | word | leftparenthese | rightparenthese |
//     comma | equal | doublequote | leftchevron | rightchevron | rulename | replace | shadow | shape | comment | `with` |
//     withexternal | withinternal | `if` | random | rotate | mode | apply | on | using | neutral | there | coordinates |
//     cardinal | is | isnot | index | dir | pipe | star | full | empty | edge | percent | normal | soft | patternchar |
//     arrow

//   lazy val newline: P[NEWLINE] = "\n *".r ^^ { NEWLINE(_.size - 1) }
//   lazy val singlecomment: P[SINGLECOMMENT] = "#[^\n]*".r ^^ { SINGLECOMMENT(_.drop(1)) }
//   lazy val multicomment: P[MULTICOMMENT] = "/\\*[\\S\\s]*\\*/".r ^^ { MULTICOMMENT(_.drop(2).dropRight(2)) }
//   lazy val `def`: P[DEF] = "def +".r >> { case s: Success[String] => DEF(s.next.pos) }
//   lazy val word: P[WORD] = ???
//   lazy val leftparenthese: P[LEFTPARENTHESE] = ???
//   lazy val rightparenthese: P[RIGHTPARENTHESE] = ???
//   lazy val comma: P[COMMA] = ???
//   lazy val equal: P[EQUAL] = ???
//   lazy val doublequote: P[DOUBLEQUOTE] = ???
//   lazy val leftchevron: P[LEFTCHEVRON] = ???
//   lazy val rightchevron: P[RIGHTCHEVRON] = ???
//   lazy val rulename: P[RULENAME] = ???
//   lazy val replace: P[REPLACE] = ???
//   lazy val shadow: P[SHADOW] = ???
//   lazy val shape: P[SHAPE] = ???
//   lazy val comment: P[COMMENT] = ???
//   lazy val `with`: P[WITH] = ???
//   lazy val withexternal: P[WITHEXTERNAL] = ???
//   lazy val withinternal: P[WITHINTERNAL] = ???
//   lazy val `if`: P[IF] = ???
//   lazy val random: P[RANDOM] = ???
//   lazy val rotate: P[ROTATE] = ???
//   lazy val mode: P[MODE] = ???
//   lazy val apply: P[APPLY] = ???
//   lazy val on: P[ON] = ???
//   lazy val using: P[USING] = ???
//   lazy val neutral: P[NEUTRAL] = ???
//   lazy val there: P[THERE] = ???
//   lazy val coordinates: P[COORDINATES] = ???
//   lazy val cardinal: P[CARDINAL] = ???
//   lazy val is: P[IS] = ???
//   lazy val isnot: P[ISNOT] = ???
//   lazy val index: P[INDEX] = ???
//   lazy val dir: P[DIR] = ???
//   lazy val pipe: P[PIPE] = ???
//   lazy val star: P[STAR] = ???
//   lazy val full: P[FULL] = ???
//   lazy val empty: P[EMPTY] = ???
//   lazy val edge: P[EDGE] = ???
//   lazy val percent: P[PERCENT] = ???
//   lazy val normal: P[NORMAL] = ???
//   lazy val soft: P[SOFT] = ???
//   lazy val patternchar: P[PATTERNCHAR] = ???
//   lazy val arrow: P[ARROW] = ???

// }
