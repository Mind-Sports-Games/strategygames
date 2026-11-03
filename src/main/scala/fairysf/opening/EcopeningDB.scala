package strategygames.fairysf.opening

// format: off
object EcopeningDB {

  import Ecopening._

  val MAX_TURNS = 25

  lazy val all = allByEco.values.toList.sorted

  lazy val allByFen: Map[FEN, Ecopening] = allByEco.map {
    case (_, opening) => opening.fen -> opening
  }

  // the amazons fen always shows full pockets of arrows, whatever is on the board
  private val amazonsPockets = s"[${"P" * 46}${"p" * 46}]"

  lazy val allByEco: Map[ECO, Ecopening] = Map(
"A00" -> new Ecopening("A00", "xiangqi", "Xiangqi Start Pos", "Xiangqi Start Pos", "", "rnbakabnr/9/1c5c1/p1p1p1p1p/9/9/P1P1P1P1P/1C5C1/9/RNBAKABNR", ""),
"A01" -> new Ecopening("A01", "xiangqi", "Xiangqi Cannon 1", "Xiangqi Cannon 1", "h3e3", "rnbakabnr/9/1c5c1/p1p1p1p1p/9/9/P1P1P1P1P/1C2C4/9/RNBAKABNR", "h3e3"),
"A02" -> new Ecopening("A02", "xiangqi", "Xiangqi Cannon 1", "Xiangqi Cannon 1", "h3e3 b10c8", "r1bakabnr/9/1cn4c1/p1p1p1p1p/9/9/P1P1P1P1P/1C2C4/9/RNBAKABNR", "b10c8"),
"A03" -> new Ecopening("A03", "xiangqi", "Xiangqi Cannon 1", "Xiangqi Cannon 1", "h3e3 h10g8", "rnbakab1r/9/1c4nc1/p1p1p1p1p/9/9/P1P1P1P1P/1C2C4/9/RNBAKABNR", "h10g8"),
"A04" -> new Ecopening("A04", "xiangqi", "Xiangqi Cannon 2", "Xiangqi Cannon 2", "b3e3", "rnbakabnr/9/1c5c1/p1p1p1p1p/9/9/P1P1P1P1P/4C2C1/9/RNBAKABNR", "b3e3"),
"A05" -> new Ecopening("A05", "xiangqi", "Xiangqi Cannon 2", "Xiangqi Cannon 2", "b3e3 b10c8", "r1bakabnr/9/1cn4c1/p1p1p1p1p/9/9/P1P1P1P1P/4C2C1/9/RNBAKABNR", "b10c8"),
"A06" -> new Ecopening("A06", "xiangqi", "Xiangqi Cannon 2", "Xiangqi Cannon 2", "b3e3 h10g8", "rnbakab1r/9/1c4nc1/p1p1p1p1p/9/9/P1P1P1P1P/4C2C1/9/RNBAKABNR", "h10g8"),
"A07" -> new Ecopening("A07", "xiangqi", "Xiangqi Knight 1", "Xiangqi Knight 1", "h1g3", "rnbakabnr/9/1c5c1/p1p1p1p1p/9/9/P1P1P1P1P/1C4NC1/9/RNBAKAB1R", "h1g3"),
"A08" -> new Ecopening("A08", "xiangqi", "Xiangqi Knight 1", "Xiangqi Knight 1", "h1g3 b10c8", "r1bakabnr/9/1cn4c1/p1p1p1p1p/9/9/P1P1P1P1P/1C4NC1/9/RNBAKAB1R", "b10c8"),
"A09" -> new Ecopening("A09", "xiangqi", "Xiangqi Knight 1", "Xiangqi Knight 1", "h1g3 h10g8", "rnbakab1r/9/1c4nc1/p1p1p1p1p/9/9/P1P1P1P1P/1C4NC1/9/RNBAKAB1R", "h10g8"),
"A10" -> new Ecopening("A10", "xiangqi", "Xiangqi Knight 2", "Xiangqi Knight 2", "b1c3", "rnbakabnr/9/1c5c1/p1p1p1p1p/9/9/P1P1P1P1P/1CN4C1/9/R1BAKABNR", "b1c3"),
"A11" -> new Ecopening("A11", "xiangqi", "Xiangqi Knight 2", "Xiangqi Knight 2", "b1c3 b10c8", "r1bakabnr/9/1cn4c1/p1p1p1p1p/9/9/P1P1P1P1P/1CN4C1/9/R1BAKABNR", "b10c8"),
"A12" -> new Ecopening("A12", "xiangqi", "Xiangqi Knight 2", "Xiangqi Knight 2", "b1c3 h10g8", "rnbakab1r/9/1c4nc1/p1p1p1p1p/9/9/P1P1P1P1P/1CN4C1/9/R1BAKABNR", "h10g8"),
"B00" -> new Ecopening("B00", "minixiangqi", "MiniXiangqi Start Pos", "MiniXiangqi Start Pos", "", "rcnkncr/p1ppp1p/7/7/7/P1PPP1P/RCNKNCR", ""),
"B01" -> new Ecopening("B01", "minixiangqi", "MiniXiangqi Cannon right", "MiniXiangqi Cannon right", "f1f3", "rcnkncr/p1ppp1p/7/7/5C1/P1PPP1P/RCNKN1R", "f1f3"),
"B02" -> new Ecopening("B02", "minixiangqi", "MiniXiangqi Cannon right", "MiniXiangqi Cannon right", "f1f3 f7f5", "rcnkn1r/p1ppp1p/5c1/7/5C1/P1PPP1P/RCNKN1R", "f7f5"),
"B03" -> new Ecopening("B03", "minixiangqi", "MiniXiangqi Cannon right", "MiniXiangqi Cannon right", "f1f3 b7b5", "r1nkncr/p1ppp1p/1c5/7/5C1/P1PPP1P/RCNKN1R", "b7b5"),
"B04" -> new Ecopening("B04", "minixiangqi", "MiniXiangqi Cannon left", "MiniXiangqi Cannon left", "b1b3", "rcnkncr/p1ppp1p/7/7/1C5/P1PPP1P/R1NKNCR", "b1b3"),
"B05" -> new Ecopening("B05", "minixiangqi", "MiniXiangqi Cannon left", "MiniXiangqi Cannon left", "b1b3 b7b5", "r1nkncr/p1ppp1p/1c5/7/1C5/P1PPP1P/R1NKNCR", "b7b5"),
"B06" -> new Ecopening("B06", "minixiangqi", "MiniXiangqi Cannon left", "MiniXiangqi Cannon left", "b1b3 f7f5", "rcnkn1r/p1ppp1p/5c1/7/1C5/P1PPP1P/R1NKNCR", "f7f5"),
"C00" -> new Ecopening("C00", "shogi", "Shogi Start Pos", "Shogi Start Pos", "", "lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL[]", ""),
"D00" -> new Ecopening("D00", "minishogi", "MiniShogi Start Pos", "MiniShogi Start Pos", "", "rbsgk/4p/5/P4/KGSBR[]", ""),
"D01" -> new Ecopening("D01", "minishogi", "MiniShogi Opening 1", "MiniShogi Opening 1", "c1d2", "rbsgk/4p/5/P2S1/KG1BR[]", "c1d2"),
"D02" -> new Ecopening("D02", "minishogi", "MiniShogi Opening 1", "MiniShogi Opening 1", "c1d2 c5b4", "rb1gk/1s2p/5/P2S1/KG1BR[]", "c5b4"),
"D03" -> new Ecopening("D03", "minishogi", "MiniShogi Opening 1", "MiniShogi Opening 1", "c1d2 b5c4", "r1sgk/2b1p/5/P2S1/KG1BR[]", "b5c4"),
"D04" -> new Ecopening("D04", "minishogi", "MiniShogi Opening 1", "MiniShogi Opening 1", "c1d2 d5d4", "rbs1k/3gp/5/P2S1/KG1BR[]", "d5d4"),
"D05" -> new Ecopening("D05", "minishogi", "MiniShogi Opening 2", "MiniShogi Opening 2", "d1c2", "rbsgk/4p/5/P1B2/KGS1R[]", "d1c2"),
"D06" -> new Ecopening("D06", "minishogi", "MiniShogi Opening 2", "MiniShogi Opening 2", "d1c2 b5c4", "r1sgk/2b1p/5/P1B2/KGS1R[]", "b5c4"),
"D07" -> new Ecopening("D07", "minishogi", "MiniShogi Opening 2", "MiniShogi Opening 2", "d1c2 c5b4", "rb1gk/1s2p/5/P1B2/KGS1R[]", "c5b4"),
"D08" -> new Ecopening("D08", "minishogi", "MiniShogi Opening 2", "MiniShogi Opening 2", "d1c2 d5d4", "rbs1k/3gp/5/P1B2/KGS1R[]", "d5d4"),
"D09" -> new Ecopening("D09", "minishogi", "MiniShogi Opening 3", "MiniShogi Opening 3", "b1b2", "rbsgk/4p/5/PG3/K1SBR[]", "b1b2"),
"D10" -> new Ecopening("D10", "minishogi", "MiniShogi Opening 3", "MiniShogi Opening 3", "b1b2 b5c4", "r1sgk/2b1p/5/PG3/K1SBR[]", "b5c4"),
"D11" -> new Ecopening("D11", "minishogi", "MiniShogi Opening 3", "MiniShogi Opening 3", "b1b2 c5b4", "rb1gk/1s2p/5/PG3/K1SBR[]", "c5b4"),
"D12" -> new Ecopening("D12", "minishogi", "MiniShogi Opening 3", "MiniShogi Opening 3", "b1b2 d5d4", "rbs1k/3gp/5/PG3/K1SBR[]", "d5d4"),
"E00" -> new Ecopening("E00", "flipello", "Flipello Start Pos", "Flipello Start Pos", "", "8/8/8/3pP3/3Pp3/8/8/8[PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPpppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppp]", ""),
"F00" -> new Ecopening("F00", "flipello10", "Flipello10 Start Pos", "Flipello10 Start Pos", "", "10/10/10/10/4pP4/4Pp4/10/10/10/10[PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPpppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppp]", ""),
"G00" -> new Ecopening("G00", "amazons", "Amazons Start Pos", "Amazons Start Pos", "", s"3q2q3/10/10/q8q/10/10/Q8Q/10/10/3Q2Q3${amazonsPockets}", ""),
"G01" -> new Ecopening("G01", "amazons", "Amazons Opening 1", "Amazons Opening 1", "g1g7", s"3q2q3/10/10/q5Q2q/10/10/Q8Q/10/10/3Q6${amazonsPockets}", "g1g7"),
"G02" -> new Ecopening("G02", "amazons", "Amazons Opening 1", "Amazons Opening 1", "g1g7 P@d7", s"3q2q3/10/10/q2p2Q2q/10/10/Q8Q/10/10/3Q6${amazonsPockets}", "P@d7"),
"G03" -> new Ecopening("G03", "amazons", "Amazons Opening 1", "Amazons Opening 1", "g1g7 P@d7 j7g4", s"3q2q3/10/10/q2p2Q3/10/10/Q5q2Q/10/10/3Q6${amazonsPockets}", "j7g4"),
"G04" -> new Ecopening("G04", "amazons", "Amazons Opening 1", "Amazons Opening 1", "g1g7 P@d7 j7g4 P@d4", s"3q2q3/10/10/q2p2Q3/10/10/Q2p2q2Q/10/10/3Q6${amazonsPockets}", "P@d4"),
"G05" -> new Ecopening("G05", "amazons", "Amazons Opening 2", "Amazons Opening 2", "d1d7", s"3q2q3/10/10/q2Q5q/10/10/Q8Q/10/10/6Q3${amazonsPockets}", "d1d7"),
"G06" -> new Ecopening("G06", "amazons", "Amazons Opening 2", "Amazons Opening 2", "d1d7 P@g7", s"3q2q3/10/10/q2Q2p2q/10/10/Q8Q/10/10/6Q3${amazonsPockets}", "P@g7"),
"G07" -> new Ecopening("G07", "amazons", "Amazons Opening 2", "Amazons Opening 2", "d1d7 P@g7 a7d4", s"3q2q3/10/10/3Q2p2q/10/10/Q2q5Q/10/10/6Q3${amazonsPockets}", "a7d4"),
"G08" -> new Ecopening("G08", "amazons", "Amazons Opening 2", "Amazons Opening 2", "d1d7 P@g7 a7d4 P@g4", s"3q2q3/10/10/3Q2p2q/10/10/Q2q2p2Q/10/10/6Q3${amazonsPockets}", "P@g4"),
// black's replies to common white first turns. These are their own grouping, so that
// white is never offered them as continuations of its own opening.
"G09" -> new Ecopening("G09", "amazons-replies", "Amazons Reply", "Amazons g1g8 P@i6", "g1g8 P@i6", s"3q2q3/10/6Q3/q8q/8p1/10/Q8Q/10/10/3Q6${amazonsPockets}", "P@i6"),
"G10" -> new Ecopening("G10", "amazons-replies", "Amazons Reply", "Amazons g1g8 P@i6", "g1g8 P@i6 d10d3", s"6q3/10/6Q3/q8q/8p1/10/Q8Q/3q6/10/3Q6${amazonsPockets}", "d10d3"),
"G11" -> new Ecopening("G11", "amazons-replies", "Amazons Reply", "Amazons g1g8 P@i6", "g1g8 P@i6 d10d3 P@b5", s"6q3/10/6Q3/q8q/8p1/1p8/Q8Q/3q6/10/3Q6${amazonsPockets}", "P@b5"),
"G12" -> new Ecopening("G12", "amazons-replies", "Amazons Reply", "Amazons g1g9 P@f9", "g1g9 P@f9", s"3q2q3/5pQ3/10/q8q/10/10/Q8Q/10/10/3Q6${amazonsPockets}", "P@f9"),
"G13" -> new Ecopening("G13", "amazons-replies", "Amazons Reply", "Amazons g1g9 P@f9", "g1g9 P@f9 d10d2", s"6q3/5pQ3/10/q8q/10/10/Q8Q/10/3q6/3Q6${amazonsPockets}", "d10d2"),
"G14" -> new Ecopening("G14", "amazons-replies", "Amazons Reply", "Amazons g1g9 P@f9", "g1g9 P@f9 d10d2 P@e2", s"6q3/5pQ3/10/q8q/10/10/Q8Q/10/3qp5/3Q6${amazonsPockets}", "P@e2"),
"G15" -> new Ecopening("G15", "amazons-replies", "Amazons Reply", "Amazons d1d8 P@b6", "d1d8 P@b6", s"3q2q3/10/3Q6/q8q/1p8/10/Q8Q/10/10/6Q3${amazonsPockets}", "P@b6"),
"G16" -> new Ecopening("G16", "amazons-replies", "Amazons Reply", "Amazons d1d8 P@b6", "d1d8 P@b6 g10g3", s"3q6/10/3Q6/q8q/1p8/10/Q8Q/6q3/10/6Q3${amazonsPockets}", "g10g3"),
"G17" -> new Ecopening("G17", "amazons-replies", "Amazons Reply", "Amazons d1d8 P@b6", "d1d8 P@b6 g10g3 P@i5", s"3q6/10/3Q6/q8q/1p8/8p1/Q8Q/6q3/10/6Q3${amazonsPockets}", "P@i5"),
"G18" -> new Ecopening("G18", "amazons-replies", "Amazons Reply", "Amazons g1g7 P@b7", "g1g7 P@b7", s"3q2q3/10/10/qp4Q2q/10/10/Q8Q/10/10/3Q6${amazonsPockets}", "P@b7"),
"G19" -> new Ecopening("G19", "amazons-replies", "Amazons Reply", "Amazons g1g7 P@b7", "g1g7 P@b7 d10d4", s"6q3/10/10/qp4Q2q/10/10/Q2q5Q/10/10/3Q6${amazonsPockets}", "d10d4"),
"G20" -> new Ecopening("G20", "amazons-replies", "Amazons Reply", "Amazons g1g7 P@b7", "g1g7 P@b7 d10d4 P@i4", s"6q3/10/10/qp4Q2q/10/10/Q2q4pQ/10/10/3Q6${amazonsPockets}", "P@i4"),
"G21" -> new Ecopening("G21", "amazons-replies", "Amazons Reply", "Amazons j4e9 P@d9", "j4e9 P@d9", s"3q2q3/3pQ5/10/q8q/10/10/Q9/10/10/3Q2Q3${amazonsPockets}", "P@d9"),
"G22" -> new Ecopening("G22", "amazons-replies", "Amazons Reply", "Amazons j4e9 P@d9", "j4e9 P@d9 a7f2", s"3q2q3/3pQ5/10/9q/10/10/Q9/10/5q4/3Q2Q3${amazonsPockets}", "a7f2"),
"G23" -> new Ecopening("G23", "amazons-replies", "Amazons Reply", "Amazons j4e9 P@d9", "j4e9 P@d9 a7f2 P@g2", s"3q2q3/3pQ5/10/9q/10/10/Q9/10/5qp3/3Q2Q3${amazonsPockets}", "P@g2"),
"G24" -> new Ecopening("G24", "amazons-replies", "Amazons Reply", "Amazons d1i6 P@i7", "d1i6 P@i7", s"3q2q3/10/10/q7pq/8Q1/10/Q8Q/10/10/6Q3${amazonsPockets}", "P@i7"),
"G25" -> new Ecopening("G25", "amazons-replies", "Amazons Reply", "Amazons d1i6 P@i7", "d1i6 P@i7 g10b5", s"3q6/10/10/q7pq/8Q1/1q8/Q8Q/10/10/6Q3${amazonsPockets}", "g10b5"),
"G26" -> new Ecopening("G26", "amazons-replies", "Amazons Reply", "Amazons d1i6 P@i7", "d1i6 P@i7 g10b5 P@b4", s"3q6/10/10/q7pq/8Q1/1q8/Qp7Q/10/10/6Q3${amazonsPockets}", "P@b4"),
"G27" -> new Ecopening("G27", "amazons-replies", "Amazons Reply", "Amazons g1g7 P@g9", "g1g7 P@g9", s"3q2q3/6p3/10/q5Q2q/10/10/Q8Q/10/10/3Q6${amazonsPockets}", "P@g9"),
"G28" -> new Ecopening("G28", "amazons-replies", "Amazons Reply", "Amazons g1g7 P@g9", "g1g7 P@g9 d10d4", s"6q3/6p3/10/q5Q2q/10/10/Q2q5Q/10/10/3Q6${amazonsPockets}", "d10d4"),
"G29" -> new Ecopening("G29", "amazons-replies", "Amazons Reply", "Amazons g1g7 P@g9", "g1g7 P@g9 d10d4 P@d2", s"6q3/6p3/10/q5Q2q/10/10/Q2q5Q/10/3p6/3Q6${amazonsPockets}", "P@d2"),
"G30" -> new Ecopening("G30", "amazons-replies", "Amazons Reply", "Amazons g1g9 P@d9", "g1g9 P@d9", s"3q2q3/3p2Q3/10/q8q/10/10/Q8Q/10/10/3Q6${amazonsPockets}", "P@d9"),
"G31" -> new Ecopening("G31", "amazons-replies", "Amazons Reply", "Amazons g1g9 P@d9", "g1g9 P@d9 j7g4", s"3q2q3/3p2Q3/10/q9/10/10/Q5q2Q/10/10/3Q6${amazonsPockets}", "j7g4"),
"G32" -> new Ecopening("G32", "amazons-replies", "Amazons Reply", "Amazons g1g9 P@d9", "g1g9 P@d9 j7g4 P@d4", s"3q2q3/3p2Q3/10/q9/10/10/Q2p2q2Q/10/10/3Q6${amazonsPockets}", "P@d4"),
"G33" -> new Ecopening("G33", "amazons-replies", "Amazons Reply", "Amazons g1g6 P@i6", "g1g6 P@i6", s"3q2q3/10/10/q8q/6Q1p1/10/Q8Q/10/10/3Q6${amazonsPockets}", "P@i6"),
"G34" -> new Ecopening("G34", "amazons-replies", "Amazons Reply", "Amazons g1g6 P@i6", "g1g6 P@i6 d10d5", s"6q3/10/10/q8q/6Q1p1/3q6/Q8Q/10/10/3Q6${amazonsPockets}", "d10d5"),
"G35" -> new Ecopening("G35", "amazons-replies", "Amazons Reply", "Amazons g1g6 P@i6", "g1g6 P@i6 d10d5 P@b5", s"6q3/10/10/q8q/6Q1p1/1p1q6/Q8Q/10/10/3Q6${amazonsPockets}", "P@b5"),
"G36" -> new Ecopening("G36", "amazons-replies", "Amazons Reply", "Amazons d1d7 P@i7", "d1d7 P@i7", s"3q2q3/10/10/q2Q4pq/10/10/Q8Q/10/10/6Q3${amazonsPockets}", "P@i7"),
"G37" -> new Ecopening("G37", "amazons-replies", "Amazons Reply", "Amazons d1d7 P@i7", "d1d7 P@i7 g10g4", s"3q6/10/10/q2Q4pq/10/10/Q5q2Q/10/10/6Q3${amazonsPockets}", "g10g4"),
"G38" -> new Ecopening("G38", "amazons-replies", "Amazons Reply", "Amazons d1d7 P@i7", "d1d7 P@i7 g10g4 P@b4", s"3q6/10/10/q2Q4pq/10/10/Qp4q2Q/10/10/6Q3${amazonsPockets}", "P@b4"),
"G39" -> new Ecopening("G39", "amazons-replies", "Amazons Reply", "Amazons d1d9 P@e9", "d1d9 P@e9", s"3q2q3/3Qp5/10/q8q/10/10/Q8Q/10/10/6Q3${amazonsPockets}", "P@e9"),
"G40" -> new Ecopening("G40", "amazons-replies", "Amazons Reply", "Amazons d1d9 P@e9", "d1d9 P@e9 g10g2", s"3q6/3Qp5/10/q8q/10/10/Q8Q/10/6q3/6Q3${amazonsPockets}", "g10g2"),
"G41" -> new Ecopening("G41", "amazons-replies", "Amazons Reply", "Amazons d1d9 P@e9", "d1d9 P@e9 g10g2 P@f2", s"3q6/3Qp5/10/q8q/10/10/Q8Q/10/5pq3/6Q3${amazonsPockets}", "P@f2"),
"G42" -> new Ecopening("G42", "amazons-replies", "Amazons Reply", "Amazons a4f9 P@g9", "a4f9 P@g9", s"3q2q3/5Qp3/10/q8q/10/10/9Q/10/10/3Q2Q3${amazonsPockets}", "P@g9"),
"G43" -> new Ecopening("G43", "amazons-replies", "Amazons Reply", "Amazons a4f9 P@g9", "a4f9 P@g9 j7e2", s"3q2q3/5Qp3/10/q9/10/10/9Q/10/4q5/3Q2Q3${amazonsPockets}", "j7e2"),
"G44" -> new Ecopening("G44", "amazons-replies", "Amazons Reply", "Amazons a4f9 P@g9", "a4f9 P@g9 j7e2 P@d2", s"3q2q3/5Qp3/10/q9/10/10/9Q/10/3pq5/3Q2Q3${amazonsPockets}", "P@d2"),
"G45" -> new Ecopening("G45", "amazons-replies", "Amazons Reply", "Amazons g1b6 P@b7", "g1b6 P@b7", s"3q2q3/10/10/qp7q/1Q8/10/Q8Q/10/10/3Q6${amazonsPockets}", "P@b7"),
"G46" -> new Ecopening("G46", "amazons-replies", "Amazons Reply", "Amazons g1b6 P@b7", "g1b6 P@b7 d10i5", s"6q3/10/10/qp7q/1Q8/8q1/Q8Q/10/10/3Q6${amazonsPockets}", "d10i5"),
"G47" -> new Ecopening("G47", "amazons-replies", "Amazons Reply", "Amazons g1b6 P@b7", "g1b6 P@b7 d10i5 P@i4", s"6q3/10/10/qp7q/1Q8/8q1/Q7pQ/10/10/3Q6${amazonsPockets}", "P@i4"),
"G48" -> new Ecopening("G48", "amazons-replies", "Amazons Reply", "Amazons d1d7 P@d9", "d1d7 P@d9", s"3q2q3/3p6/10/q2Q5q/10/10/Q8Q/10/10/6Q3${amazonsPockets}", "P@d9"),
"G49" -> new Ecopening("G49", "amazons-replies", "Amazons Reply", "Amazons d1d7 P@d9", "d1d7 P@d9 g10g4", s"3q6/3p6/10/q2Q5q/10/10/Q5q2Q/10/10/6Q3${amazonsPockets}", "g10g4"),
"G50" -> new Ecopening("G50", "amazons-replies", "Amazons Reply", "Amazons d1d7 P@d9", "d1d7 P@d9 g10g4 P@g2", s"3q6/3p6/10/q2Q5q/10/10/Q5q2Q/10/6p3/6Q3${amazonsPockets}", "P@g2"),
"G51" -> new Ecopening("G51", "amazons-replies", "Amazons Reply", "Amazons d1d9 P@g9", "d1d9 P@g9", s"3q2q3/3Q2p3/10/q8q/10/10/Q8Q/10/10/6Q3${amazonsPockets}", "P@g9"),
"G52" -> new Ecopening("G52", "amazons-replies", "Amazons Reply", "Amazons d1d9 P@g9", "d1d9 P@g9 a7d4", s"3q2q3/3Q2p3/10/9q/10/10/Q2q5Q/10/10/6Q3${amazonsPockets}", "a7d4"),
"G53" -> new Ecopening("G53", "amazons-replies", "Amazons Reply", "Amazons d1d9 P@g9", "d1d9 P@g9 a7d4 P@g4", s"3q2q3/3Q2p3/10/9q/10/10/Q2q2p2Q/10/10/6Q3${amazonsPockets}", "P@g4"),
"G54" -> new Ecopening("G54", "amazons-replies", "Amazons Reply", "Amazons d1d6 P@b6", "d1d6 P@b6", s"3q2q3/10/10/q8q/1p1Q6/10/Q8Q/10/10/6Q3${amazonsPockets}", "P@b6"),
"G55" -> new Ecopening("G55", "amazons-replies", "Amazons Reply", "Amazons d1d6 P@b6", "d1d6 P@b6 g10g5", s"3q6/10/10/q8q/1p1Q6/6q3/Q8Q/10/10/6Q3${amazonsPockets}", "g10g5"),
"G56" -> new Ecopening("G56", "amazons-replies", "Amazons Reply", "Amazons d1d6 P@b6", "d1d6 P@b6 g10g5 P@i5", s"3q6/10/10/q8q/1p1Q6/6q1p1/Q8Q/10/10/6Q3${amazonsPockets}", "P@i5"),
  )
}
