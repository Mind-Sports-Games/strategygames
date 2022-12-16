package strategygames.fairysf.opening

// format: off
object EcopeningDB {

  import Ecopening._

  val MAX_MOVES = 25

  lazy val all = allByEco.values.toList.sorted

  lazy val allByFen: Map[FEN, Ecopening] = allByEco.map {
    case (_, opening) => opening.fen -> opening
  }

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
  )
}
