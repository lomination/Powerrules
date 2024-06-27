package lomination.powerrules.writer

import lomination.powerrules.{Cond, Dir, Matcher, Pos}

enum Trilean:
  case O, ?, X

def toConds(pattern: Seq[Trilean])(matcher: Matcher): Seq[Cond] =
  val positions = Seq(Pos.nw, Pos.n, Pos.ne, Pos.w, Pos.zero, Pos.e, Pos.sw, Pos.s, Pos.se)
  (positions zip pattern).foldLeft(Seq()) { (conds: Seq[Cond], pt: (Pos, Trilean)) =>
    pt match
      case (pos, Trilean.O) => conds :+ (pos is matcher)
      case (pos, Trilean.?) => conds
      case (pos, Trilean.X) => conds :+ (pos isnot matcher)
  }

def defaultTilesConds(softMode: Boolean)(matcher: Matcher): Seq[(String, Seq[Dir], Seq[Cond])] =
  defaultTilePatterns(softMode) map { (t: (String, Seq[Dir], Seq[Trilean])) => (t._1, t._2, toConds(t._3)(matcher)) }

def externalTilesConds(softMode: Boolean)(matcher: Matcher): Seq[(String, Seq[Dir], Seq[Cond])] =
  externalTilePatterns(softMode) map { (t: (String, Seq[Dir], Seq[Trilean])) => (t._1, t._2, toConds(t._3)(matcher)) }

def internalTilesConds(softMode: Boolean)(matcher: Matcher): Seq[(String, Seq[Dir], Seq[Cond])] =
  internalTilePatterns(softMode) map { (t: (String, Seq[Dir], Seq[Trilean])) => (t._1, t._2, toConds(t._3)(matcher)) }

// @formatter:off

def defaultTilePatterns(softMode: Boolean): Seq[(String, Seq[Dir], Seq[Trilean])] =
  import lomination.powerrules.writer.Trilean.{O, ?, X}
  if (softMode)
    Seq(
      (
        "d1",
        Seq(Dir.p0),
        Seq(
          ?, ?, ?,
          ?, O, ?,
          ?, ?, ?
        )
      ),
      (
        "d2",
        Dir.positive,
        Seq(
          X, X, X,
          ?, O, ?,
          ?, ?, ?
        )
      ),
      (
        "d3",
        Dir.positive,
        Seq(
          X, X, ?,
          X, O, O,
          ?, O, ?
        )
      ),
      (
        "d4",
        Dir.positive,
        Seq(
          ?, X, ?,
          O, O, O,
          ?, X, ?
        )
      ),
      (
        "d5",
        Dir.positive,
        Seq(
          X, X, X,
          X, O, X,
          ?, O, ?
        )
      ),
      (
        "d6",
        Seq(Dir.p0),
        Seq(
          X, X, X,
          X, O, X,
          X, X, X
        )
      ),
      (
        "d7",
        Dir.positive,
        Seq(
          O, X, X,
          X, O, X,
          X, X, X
        )
      ),
      (
        "d8",
        Seq(Dir.p0, Dir.p1),
        Seq(
          O, X, X,
          X, O, X,
          X, X, O
        )
      )
    )
  else
    Seq(
      (
        "d1",
        Seq(Dir.p0),
        Seq(
          ?, O, ?,
          O, O, O,
          ?, O, ?
        )
      ),
      (
        "d2",
        Dir.positive,
        Seq(
          ?, X, ?,
          O, O, O,
          ?, O, ?
        )
      ),
      (
        "d3",
        Dir.positive,
        Seq(
          ?, X, ?,
          X, O, O,
          ?, O, ?
        )
      ),
      (
        "d4",
        Dir.positive,
        Seq(
          ?, X, ?,
          O, O, O,
          ?, X, ?
        )
      ),
      (
        "d5",
        Dir.positive,
        Seq(
          ?, X, ?,
          X, O, X,
          ?, O, ?
        )
      ),
      (
        "d6",
        Seq(Dir.p0),
        Seq(
          ?, X, ?,
          X, O, X,
          ?, X, ?
        )
      )
    )

def externalTilePatterns(softMode: Boolean): Seq[(String, Seq[Dir], Seq[Trilean])] =
  import lomination.powerrules.writer.Trilean.{O, ?, X}
  if (softMode)
    Seq(
    (
      "e1",
      Dir.positive,
      Seq(
        ?, O, ?,
        O, X, X,
        ?, X, ?
      )
    ),
    (
      "e2",
      Dir.positive,
      Seq(
        ?, O, ?,
        O, X, O,
        ?, X, ?
      )
    ),
    (
      "e4",
      Dir.positive,
      Seq(
        ?, O, ?,
        O, X, O,
        ?, O, ?
      )
    )
  )
  else
    Seq(
      (
        "e1",
        Dir.positive,
        Seq(
          O, O, ?,
          O, X, ?,
          ?, ?, ?
        )
      ),
      (
        "e2",
        Dir.positive,
        Seq(
          O, O, O,
          O, X, O,
          ?, ?, ?
        )
      ),
      (
        "e3",
        Dir.positive,
        Seq(
          O, O, O,
          O, X, O,
          X, O, O
        )
      ),
      (
        "e4",
        Seq(Dir.p0),
        Seq(
          O, O, O,
          O, X, O,
          O, O, O
        )
      ),
      (
        "e5",
        Seq(Dir.p0, Dir.p1),
        Seq(
          O, O, X,
          O, X, O,
          X, O, O
        )
      )
    )

def internalTilePatterns(softMode: Boolean): Seq[(String, Seq[Dir], Seq[Trilean])] =
  import lomination.powerrules.writer.Trilean.{O, ?, X}
  Seq(
    (
      "i1",
      Dir.positive,
      Seq(
        X, O, O,
        O, O, O,
        O, O, O
      )
    ),
    (
      "i2",
      Dir.positive,
      Seq(
        X, O, X,
        O, O, O,
        O, O, O
      )
    ),
    (
      "i3",
      Dir.positive,
      Seq(
        X, O, X,
        O, O, O,
        O, O, X
      )
    ),
    (
      "i4",
      Seq(Dir.p0),
      Seq(
        X, O, X,
        O, O, O,
        X, O, X
      )
    ),
    (
      "i5",
      Dir.positive,
      Seq(
        X, O, O,
        O, O, O,
        O, O, X
      )
    ),
    (
      "i6",
      Dir.positive,
      Seq(
        ?, X, ?,
        X, O, O,
        ?, O, X
      )
    ),
    (
      "i7",
      Dir.all,
      Seq(
        ?, X, ?,
        O, O, O,
        X, O, ?
      )
    ),
    (
      "i8",
      Dir.positive,
      Seq(
        ?, X, ?,
        O, O, O,
        X, O, X
      )
    )
  )
