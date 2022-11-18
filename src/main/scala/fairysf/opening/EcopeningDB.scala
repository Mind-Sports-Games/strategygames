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
"C00" -> new Ecopening("C00", "shogi", "Shogi Start Pos", "Shogi Start Pos", "", "lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL[-]", ""),
"D00" -> new Ecopening("D00", "minishogi", "MiniShogi Start Pos", "MiniShogi Start Pos", "", "rbsgk/4p/5/P4/KGSBR[-]", ""),
"E00" -> new Ecopening("E00", "flipello", "Flipello Start Pos", "Flipello Start Pos", "", "8/8/8/3pP3/3Pp3/8/8/8[PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPpppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppp]", ""),
"F00" -> new Ecopening("F00", "flipello10", "Flipello10 Start Pos", "Flipello10 Start Pos", "", "10/10/10/10/4pP4/4Pp4/10/10/10/10[PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPpppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppp]", ""),
  )
}
