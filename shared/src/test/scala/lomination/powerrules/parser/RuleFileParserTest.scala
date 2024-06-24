package lomination.powerrules.parser

import munit.FunSuite
import lomination.powerrules.{Comment,Dir,FullMatcher,Grid,NotEdgeMatcher,Op,Pos,Replace,Random,Shadow,Shape,Tile,TileMatcher,TmpTile}

class ParseRuleFile extends FunSuite {

  val P = RuleFileParser

  test("ParseRuleFile (1)") {
    val input = """|[r1]
                   |
                   |replace with 1+0
                   |
                   |[boo]
                   |
                   |[r2]
                   |
                   |replace with 2+0
                   |
                   |""".stripMargin
    val result = P(input)
    assert(result.isSuccess, s"Failed to parse test: ${result.failed.get.getMessage}")
  }

  test("ParseRuleFile (2): random spaces") {
    val input = """|             
                   |[r1]          
                   |[-]
                   |replace with 1+0
                   |           
                   |[------]          
                   |          
                   |[r2]          
                   |             
                   |replace with 2+0          
                   |          
                   |      """.stripMargin
    val result = P(input)
    assert(result.isSuccess, s"Failed to parse test: ${result.failed.get.getMessage}")
  }

  test("ParseRuleFile (3): random spaces") {
    val input = """|             
                   |[r1]          
                   |            
                   |replace with 1+0
                   |           
                   |[------]          
                   |          
                   |[r2]          
                   |             
                   |replace with 2+0""".stripMargin
    val result = P(input)
    assert(result.isSuccess, s"Failed to parse test: ${result.failed.get.getMessage}")
  }

  test("ParseRuleFile (4)") {
    val input = """|             
                   |[r1]          
                   |            
                   |replace with 1+0
                   |           
                   |[------]          
                   |          
                   |[r2]          
                   |             
                   |[b]""".stripMargin
    val result = P(input)
    assert(result.isSuccess, s"Failed to parse test: ${result.failed.get.getMessage}")
  }

  test("ParseRuleFile (4)") {
    val input = """|             
                   |[r1]          
                   |            
                   |replace with 1+0
                   |           
                   |[------]          
                   |          
                   |[r2]          
                   |             
                   |[b]     """.stripMargin
    val result = P(input)
    assert(result.isSuccess, s"Failed to parse test: ${result.failed.get.getMessage}")
  }

}

class ParseReplace extends FunSuite {

  val P = RuleFileParser

  test("ParseReplace (1)") {
    val input  = """replace with 2e-2"""
    val result = P.parse(P.command, input)
    val expected = Replace(
      Seq(Tile(0x2e, Dir.m2)),
      Seq(),
      Random.always,
      Seq(Dir.p0)
    )
    assert(result.successful, s"Failed to parse test: $result")
    assert(clue(result.get) == clue(expected))
  }

  test("ParseReplace (2): using re") {
    val input  = """re with 2e-2"""
    val result = P.parse(P.command, input)
    val expected = Replace(
      Seq(Tile(0x2e, Dir.m2)),
      Seq(),
      Random.always,
      Seq(Dir.p0)
    )
    assert(result.successful, s"Failed to parse test: $result")
    assert(clue(result.get) == clue(expected))
  }

  test("ParseReplace (3): indented") {
    val input = """|replace
                   |    with
                   |        2e-2""".stripMargin
    val result = P.parse(P.command, input)
    val expected = Replace(
      Seq(Tile(0x2e, Dir.m2)),
      Seq(),
      Random.always,
      Seq(Dir.p0)
    )
    assert(result.successful, s"Failed to parse test: $result")
    assert(clue(result.get) == clue(expected))
  }

  test("ParseReplace (4): with conds") {
    val input = """|replace
                   |    with 1f-0
                   |    if 0 0 is 9+2 & -1 0 is 3-0""".stripMargin
    val result = P.parse(P.command, input)
    val expected = Replace(
      Seq(Tile(0x1f, Dir.m0)),
      (Pos.zero is TileMatcher(9, Dir.p2)) & (Pos(-1, 0) is TileMatcher(3, Dir.m0)),
      Random.always,
      Seq(Dir.p0)
    )
    assert(result.successful, s"Failed to parse test: $result")
    assert(clue(result.get) == clue(expected))
  }

  test("ParseReplace (5): indented with conds") {
    val input = """|replace
                   |    with 1f-0
                   |    if
                   |        0 0 is 9+2 &
                   |        -1 0 is 3-0""".stripMargin
    val result = P.parse(P.command, input)
    val expected = Replace(
      Seq(Tile(0x1f, Dir.m0)),
      (Pos.zero is TileMatcher(9, Dir.p2)) & (Pos(-1, 0) is TileMatcher(3, Dir.m0)),
      Random.always,
      Seq(Dir.p0)
    )
    assert(result.successful, s"Failed to parse test: $result")
    assert(clue(result.get) == clue(expected))
  }

  test("ParseReplace (6): all statements") {
    val input = """|replace
                   |    with 1f-0
                   |    when 0 0 is 9+2 &
                   |        -1 0 is 3-0
                   |    random 0.33
                   |    rotate +0+1+2+3""".stripMargin
    val result = P.parse(P.command, input)
    val expected = Replace(
      Seq(Tile(0x1f, Dir.m0)),
      (Pos.zero is TileMatcher(9, Dir.p2)) & (Pos(-1, 0) is TileMatcher(3, Dir.m0)),
      Random(33),
      Seq(Dir.p0, Dir.p1, Dir.p2, Dir.p3)
    )
    assert(result.successful, s"Failed to parse test: $result")
    assert(clue(result.get) == clue(expected))
  }

  // test("ParseReplace (7): random spaces") {
  //   val input = """|replace       
  //                  |   
  //                  |
  //                  |    with         1f-0 20-0        
  //                  |
  //                  |    when    0     0    is     9+2    &   
  //                  |        -1    0  is      3-0
  //                  |    random        
  //                  |        0.33   
  //                  |    rotate    +0+1   +2
  //                  |        +3   
  //                  |""".stripMargin
  //   val result = P.parse(P.command, input)
  //   val expected = Replace(
  //     Seq(Tile(0x1f, Dir.m0), Tile(0x20, Dir.m0)),
  //     (Pos.zero is TileMatcher(9, Dir.p2)) & (Pos(-1, 0) is TileMatcher(3, Dir.m0)),
  //     Random(33),
  //     Seq(Dir.p0, Dir.p1, Dir.p2, Dir.p3)
  //   )
  //   assert(result.successful, s"Failed to parse test: $result")
  //   assert(clue(result.get) == clue(expected))
  // }

  test("ParseReplace (8): disordered statements") {
    val input = """|replace
                   |    random 0.33
                   |    rotate +0+1+2+3
                   |    when 0 0 is 9+2
                   |    with 1f-0""".stripMargin
    val result = P.parse(P.command, input)
    val expected = Replace(
      Seq(Tile(0x1f, Dir.m0)),
      Seq(Pos.zero is TileMatcher(9, Dir.p2)),
      Random(33),
      Seq(Dir.p0, Dir.p1, Dir.p2, Dir.p3)
    )
    assert(result.successful, s"Failed to parse test: $result")
    assert(clue(result.get) == clue(expected))
  }

  test("ParseReplace (9): missing statement") {
    val input = """|replace
                   |    random 0.33
                   |    rotate +0+1+2+3
                   |    when 0 0 is 9+2""".stripMargin
    val result = P.parse(P.command, input)
    assert(!result.successful, s"Parsing should have failed")
  }

}

class ParseShadow extends FunSuite {

  val P = RuleFileParser

  test("ParseShadow (1)") {
    val input = """|shadow
                   |    with 1 14+1 75+1 24+1 76+2 52
                   |    if 0 0 is full""".stripMargin
    val result = P.parse(P.shadow, input)
    val expected = Shadow(
      Seq(Tile(0x1, Dir.p0), Tile(0x14, Dir.p1), Tile(0x75, Dir.p1), Tile(0x24, Dir.p1), Tile(0x76, Dir.p2), Tile(0x52, Dir.p0)),
      Seq(),
      Seq(),
      Seq(Pos(0, 0) is FullMatcher(Op.Is)),
      false
    )
    assert(result.successful, s"Failed to parse test: $result")
    assert(clue(result.get) == clue(expected))
  }

  test("ParseShadow (2): all statements") {
    val input = """|shadow
                   |    with 1 14+1 75+1 24+1 76+2 52
                   |    withexternal 45 47 49
                   |    withinternal 35+2 85+2 b0+1 b1+3 b2 b3+1 60+1 b4
                   |    if 0 0 is full
                   |    mode soft""".stripMargin
    val result = P.parse(P.shadow, input)
    val expected = Shadow(
      Seq(Tile(0x1, Dir.p0), Tile(0x14, Dir.p1), Tile(0x75, Dir.p1), Tile(0x24, Dir.p1), Tile(0x76, Dir.p2), Tile(0x52, Dir.p0)),
      Seq(Tile(0x45, Dir.p0), Tile(0x47, Dir.p0), Tile(0x49, Dir.p0)),
      Seq(Tile(0x35, Dir.p2), Tile(0x85, Dir.p2), Tile(0xb0, Dir.p1), Tile(0xb1, Dir.p3), Tile(0xb2), Tile(0xb3, Dir.p1), Tile(0x60, Dir.p1), Tile(0xb4)),
      Seq(Pos(0, 0) is FullMatcher(Op.Is)),
      true
    )
    assert(result.successful, s"Failed to parse test: $result")
    assert(clue(result.get) == clue(expected))
  }

  test("ParseShadow (3): disordered statements") {
    val input = """|shadow
                   |    if 0 0 is full
                   |    withinternal 35+2 85+2 b0+1 b1+3 b2 b3+1 60+1 b4
                   |    mode soft
                   |    withexternal 45 47 49
                   |    with 1 14+1 75+1 24+1 76+2 52""".stripMargin
    val result = P.parse(P.shadow, input)
    val expected = Shadow(
      Seq(Tile(0x1, Dir.p0), Tile(0x14, Dir.p1), Tile(0x75, Dir.p1), Tile(0x24, Dir.p1), Tile(0x76, Dir.p2), Tile(0x52, Dir.p0)),
      Seq(Tile(0x45, Dir.p0), Tile(0x47, Dir.p0), Tile(0x49, Dir.p0)),
      Seq(Tile(0x35, Dir.p2), Tile(0x85, Dir.p2), Tile(0xb0, Dir.p1), Tile(0xb1, Dir.p3), Tile(0xb2), Tile(0xb3, Dir.p1), Tile(0x60, Dir.p1), Tile(0xb4)),
      Seq(Pos(0, 0) is FullMatcher(Op.Is)),
      true
    )
    assert(result.successful, s"Failed to parse test: $result")
    assert(clue(result.get) == clue(expected))
  }

  test("ParseShadow (4): with indentation") {
    val input = """|shadow
                   |    if
                   |        0 0 is full &
                   |        0 -1 isnot edge
                   |    withinternal
                   |        35+2 85+2 b0+1 b1+3 b2 b3+1
                   |        60+1
                   |        b4
                   |    mode
                   |        soft
                   |    withexternal 45 47 49
                   |    with
                   |        1 14+1 75+1
                   |        24+1 76+2 52""".stripMargin
    val result = P.parse(P.shadow, input)
    val expected = Shadow(
      Seq(Tile(0x1, Dir.p0), Tile(0x14, Dir.p1), Tile(0x75, Dir.p1), Tile(0x24, Dir.p1), Tile(0x76, Dir.p2), Tile(0x52, Dir.p0)),
      Seq(Tile(0x45, Dir.p0), Tile(0x47, Dir.p0), Tile(0x49, Dir.p0)),
      Seq(Tile(0x35, Dir.p2), Tile(0x85, Dir.p2), Tile(0xb0, Dir.p1), Tile(0xb1, Dir.p3), Tile(0xb2, Dir.p0), Tile(0xb3, Dir.p1), Tile(0x60, Dir.p1), Tile(0xb4, Dir.p0)),
      (Pos(0, 0) is FullMatcher(Op.Is)) & (Pos(0, -1) is NotEdgeMatcher),
      true
    )
    assert(result.successful, s"Failed to parse test: $result")
    assert(clue(result.get) == clue(expected))
  }

  test("ParseShadow (5): with random spaces") {
    val input = """|shadow             
                   |    if             
                   |               
                   |        0     0          is               full    &
                   |        0   -1  isnot        edge
                   |    withinternal   
                   |       
                   |               
                   |        35+2       85+2    b0+1 b1+3 b2 b3+1
                   |        60+1
                   |        b4         
                   |           
                   |    mode   
                   |        soft
                   |    withexternal 45      47 49
                   |    with      
                   |        1 14+1         75+1
                   |        24+1 76+2 52""".stripMargin
    val result = P.parse(P.shadow, input)
    val expected = Shadow(
      Seq(Tile(0x1, Dir.p0), Tile(0x14, Dir.p1), Tile(0x75, Dir.p1), Tile(0x24, Dir.p1), Tile(0x76, Dir.p2), Tile(0x52, Dir.p0)),
      Seq(Tile(0x45, Dir.p0), Tile(0x47, Dir.p0), Tile(0x49, Dir.p0)),
      Seq(Tile(0x35, Dir.p2), Tile(0x85, Dir.p2), Tile(0xb0, Dir.p1), Tile(0xb1, Dir.p3), Tile(0xb2, Dir.p0), Tile(0xb3, Dir.p1), Tile(0x60, Dir.p1), Tile(0xb4, Dir.p0)),
      (Pos(0, 0) is FullMatcher(Op.Is)) & (Pos(0, -1) is NotEdgeMatcher),
      true
    )
    assert(result.successful, s"Failed to parse test: $result")
    assert(clue(result.get) == clue(expected))
  }

  test("ParseShadow (5): missing statement") {
    val input = """|shadow
                   |    if 0 0 is full
                   |    withinternal 35+2 85+2 b0+1 b1+3 b2 b3+1 60+1 b4
                   |    mode soft
                   |    withexternal 45 47 49""".stripMargin
    val result = P.parse(P.shadow, input)
    assert(!result.successful, s"Parsing should have failed")
  }

}

class ParseShape extends FunSuite {

  val P = RuleFileParser

  test("ParseShape (1)") {
    val input = """|shape
                   |    apply
                   |        1 2
                   |        3 4
                   |    on
                   |        . .
                   |        . .
                   |    using
                   |        1 -> 1+0
                   |        2 -> 2+0
                   |        3 -> 3+0
                   |        4 -> 4+0
                   |    neutral 5+0
                   |    random 50%""".stripMargin
    val result = P.parse(P.shape, input)
    val expected = Shape(
      Grid(Seq(Seq(Some(Tile(1)), Some(Tile(2))), Seq(Some(Tile(3)), Some(Tile(4))))),
      Grid(Seq(Seq(Some(FullMatcher(Op.Is)), Some(FullMatcher(Op.Is))), Seq(Some(FullMatcher(Op.Is)), Some(FullMatcher(Op.Is))))),
      Tile(5),
      Random(50f),
      Seq(Dir.p0)
    )
    assert(result.successful, s"Failed to parse test: $result")
    assert(clue(result.get) == clue(expected))
  }

  test("ParseShape (2): all statements") {
    val input = """|shape
                   |    apply
                   |        1 2
                   |        3 4
                   |    on
                   |        . .
                   |        . .
                   |    using
                   |        1 -> 1+0
                   |        2 -> 2+0
                   |        3 -> 3+0
                   |        4 -> 4+0
                   |    neutral 5+0
                   |    random 50%
                   |    rotate +0-0""".stripMargin
    val result = P.parse(P.shape, input)
    val expected = Shape(
      Grid(Seq(Seq(Some(Tile(1)), Some(Tile(2))), Seq(Some(Tile(3)), Some(Tile(4))))),
      Grid(Seq(Seq(Some(FullMatcher(Op.Is)), Some(FullMatcher(Op.Is))), Seq(Some(FullMatcher(Op.Is)), Some(FullMatcher(Op.Is))))),
      Tile(5),
      Random(50f),
      Seq(Dir.p0, Dir.m0)
    )
    assert(result.successful, s"Failed to parse test: $result")
    assert(clue(result.get) == clue(expected))
  }

  test("ParseShape (3): disoredered statements") {
    val input = """|shape
                   |    using
                   |        1 -> 1+0
                   |        2 -> 2+0
                   |        3 -> 3+0
                   |        4 -> 4+0
                   |    neutral 5+0
                   |    random 50%
                   |    on
                   |        . .
                   |        . .
                   |    rotate +0-0
                   |    apply
                   |        1 2
                   |        3 4""".stripMargin
    val result = P.parse(P.shape, input)
    val expected = Shape(
      Grid(Seq(Seq(Some(Tile(1)), Some(Tile(2))), Seq(Some(Tile(3)), Some(Tile(4))))),
      Grid(Seq(Seq(Some(FullMatcher(Op.Is)), Some(FullMatcher(Op.Is))), Seq(Some(FullMatcher(Op.Is)), Some(FullMatcher(Op.Is))))),
      Tile(5),
      Random(50f),
      Seq(Dir.p0, Dir.m0)
    )
    assert(result.successful, s"Failed to parse test: $result")
    assert(clue(result.get) == clue(expected))
  }

  test("ParseShape (4): missing statement") {
    val input = """|shape
                   |    apply
                   |        1 2
                   |        3 4
                   |    on
                   |        . .
                   |        . .
                   |    neutral 5+0
                   |    random 50%""".stripMargin
    val result = P.parse(P.shape, input)
    assert(!result.successful, s"Parsing should have failed")
  }

  test("ParseShape (5): missing statement") {
    val input = """|shape
                   |    apply
                   |        1 2
                   |        3 4
                   |    on
                   |        . .
                   |        . .
                   |    using
                   |        1 -> 1+0
                   |        2 -> 2+0
                   |        3 -> 3+0
                   |        4 -> 4+0
                   |    random 50%""".stripMargin
    val result = P.parse(P.shape, input)
    assert(!result.successful, s"Parsing should have failed")
  }

}

class ParseComment extends FunSuite {

  val P = RuleFileParser

  test("ParseComment(1)") {
    val input    = """# a nice comment"""
    val result   = P.parse(P.comment, input)
    val expected = Comment("# a nice comment")
    assert(result.successful, s"Failed to parse test: $result")
    assert(clue(result.get) == clue(expected))
  }

  test("ParseComment(2): with special chars") {
    val input    = """# a less nice comment... !"#$%&'()*+,-./:;<=>?@[\]^_`{|}~"""
    val result   = P.parse(P.comment, input)
    val expected = Comment("""# a less nice comment... !"#$%&'()*+,-./:;<=>?@[\]^_`{|}~""")
    assert(result.successful, s"Failed to parse test: $result")
    assert(clue(result.get) == clue(expected))
  }

  test("ParseComment(3): with trailing spaces") {
    val input = """|# another one    
                   |      """.stripMargin
    val result   = P.parse(P.comment, input)
    val expected = Comment("# another one    ")
    assert(result.successful, s"Failed to parse test: $result")
    assert(clue(result.get) == clue(expected))
  }

  test("ParseComment(4): with trailing spaces") {
    val input = """|# the last one      
                   |      
                   |""".stripMargin
    val result   = P.parse(P.comment, input)
    val expected = Comment("# the last one      ")
    assert(result.successful, s"Failed to parse test: $result")
    assert(clue(result.get) == clue(expected))
  }

}

class ParseTile extends FunSuite {

  val P = RuleFileParser

  test("ParseTile (1)") {
    val input    = "12+0"
    val result   = P.parse(P.tile, input)
    val expected = Tile(0x12, Dir.p0)
    assert(result.successful, s"Failed to parse test: $result")
    assert(clue(result.get) == clue(expected))
  }

  test("ParseTile (2): 1 digit index") {
    val input    = "3+0"
    val result   = P.parse(P.tile, input)
    val expected = Tile(0x3, Dir.p0)
    assert(result.successful, s"Failed to parse test: $result")
    assert(clue(result.get) == clue(expected))
  }

  test("ParseTile (3): index starting with '0'") {
    val input    = "06+0"
    val result   = P.parse(P.tile, input)
    val expected = Tile(0x6, Dir.p0)
    assert(result.successful, s"Failed to parse test: $result")
    assert(clue(result.get) == clue(expected))
  }

  test("ParseTile (4): hexadecimal index") {
    val input    = "4a+0"
    val result   = P.parse(P.tile, input)
    val expected = Tile(0x4a, Dir.p0)
    assert(result.successful, s"Failed to parse test: $result")
    assert(clue(result.get) == clue(expected))
  }

  test("ParseTile (5): positive dir") {
    val input    = "a2+2"
    val result   = P.parse(P.tile, input)
    val expected = Tile(0xa2, Dir.p2)
    assert(result.successful, s"Failed to parse test: $result")
    assert(clue(result.get) == clue(expected))
  }

  test("ParseTile (6): negative dir") {
    val input    = "b3-3"
    val result   = P.parse(P.tile, input)
    val expected = Tile(0xb3, Dir.m3)
    assert(result.successful, s"Failed to parse test: $result")
    assert(clue(result.get) == clue(expected))
  }

  test("ParseTile (7): defualt dir") {
    val input    = "c4"
    val result   = P.parse(P.tile, input)
    val expected = Tile(0xc4, Dir.p0)
    assert(result.successful, s"Failed to parse test: $result")
    assert(clue(result.get) == clue(expected))
  }

}

class ParseRuleName extends FunSuite {

  val P = RuleFileParser

  test("ParseRuleName (1)") {
    val input    = "[test]"
    val result   = P.parse(P.ruleName, input)
    val expected = "test"
    assert(result.successful, s"Failed to parse test: $result")
    assert(clue(result.get) == clue(expected))
  }

  test("ParseRuleName (2): with brackets") {
    val input    = "[test with [brackets]]"
    val result   = P.parse(P.ruleName, input)
    val expected = "test with [brackets]"
    assert(result.successful, s"Failed to parse test: $result")
    assert(clue(result.get) == clue(expected))
  }

}

class ParseTmpTile extends FunSuite {

  val P = RuleFileParser

  test("ParseTmpTile (1)") {
    val input    = ":ff-3"
    val result   = P.parse(P.tmpTile, input)
    val expected = TmpTile(0xff, Dir.m3)
    assert(result.successful, s"Failed to parse test: $result")
    assert(clue(result.get) == clue(expected))
  }

}

class ParsePos extends FunSuite {

  val P = RuleFileParser

  test("ParsePos (1)") {
    val input    = "ne"
    val result   = P.parse(P.pos, input)
    val expected = Pos.ne
    assert(result.successful, s"Failed to parse test: $result")
    assert(clue(result.get) == clue(expected))
  }

  test("ParsePos (2)") {
    val input    = "sse"
    val result   = P.parse(P.pos, input)
    val expected = Pos(1, 2)
    assert(result.successful, s"Failed to parse test: $result")
    assert(clue(result.get) == clue(expected))
  }

  test("ParsePos (3): suffled") {
    val input    = "nwnnw"
    val result   = P.parse(P.pos, input)
    val expected = Pos(-2, -3)
    assert(result.successful, s"Failed to parse test: $result")
    assert(clue(result.get) == clue(expected))
  }

  test("ParsePos (4): invalid") {
    val input  = "nse"
    val result = P.parse(P.pos, input)
    assert(!result.successful, "Parsing should have failed")
  }

  test("ParsePos (5): invalid") {
    val input  = "nenneewen"
    val result = P.parse(P.pos, input)
    assert(!result.successful, "Parsing should have failed")
  }

  test("ParsePos (6): position o") {
    val input  = "o"
    val result = P.parse(P.pos, input)
    val expected = Pos.zero
    assert(result.successful, s"Failed to parse test: $result")
    assert(clue(result.get) == clue(expected))
  }

  test("ParsePos (7): position there") {
    val input  = "there"
    val result = P.parse(P.pos, input)
    val expected = Pos.zero
    assert(result.successful, s"Failed to parse test: $result")
    assert(clue(result.get) == clue(expected))
  }

}
