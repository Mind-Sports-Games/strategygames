package strategygames.go

import org.specs2.mutable.Specification

import strategygames.Player
import strategygames.go.format.FEN

class GoChainTest extends Specification with GoRulesTestSupport {

  private def boardOf(rows: String, declaredSize: Board.BoardSize, komiTenths: Int): Board = {
    val board = situationFrom(FEN(s"${rows}${pocket} b - 0 0 0 0 ${komiTenths} 0 1")).board
    if (board.variant.boardSize != declaredSize)
      sys.error(s"these rows make a ${board.variant.boardSize} board, not a ${declaredSize} one")
    board
  }

  private def board9x9(rows: String): Board   = boardOf(rows, Board.Dim9x9, 55)
  private def board13x13(rows: String): Board = boardOf(rows, Board.Dim13x13, 75)
  private def board19x19(rows: String): Board = boardOf(rows, Board.Dim19x19, 75)

  "a lone stone on a 9x9 board" should {
    val board = board9x9("9/9/9/9/4S4/9/9/9/9")

    "form a chain of itself" in {
      Chain.at(board, Pos.E5) === Set(Pos.E5)
    }

    "have four liberties in the centre" in {
      Chain.liberties(board, Set(Pos.E5)) === Set(Pos.D5, Pos.F5, Pos.E4, Pos.E6)
    }

    "report no chain at an empty point" in {
      Chain.at(board, Pos.D5) === Set.empty[Pos]
    }
  }

  "an occupied point, which no placement may be asked about" should {
    val board = board9x9("9/9/9/9/4S4/9/9/9/9")

    "be refused by capturedBy, naming the point" in {
      Chain.capturedBy(board, P2, Pos.E5) must throwAn[IllegalArgumentException].like { case failure =>
        failure.getMessage must contain(Pos.E5.key)
      }
    }

    "be refused by capturesUnlessSuicide, naming the point" in {
      Chain.capturesUnlessSuicide(board, P2, Pos.E5) must throwAn[IllegalArgumentException].like {
        case failure => failure.getMessage must contain(Pos.E5.key)
      }
    }

    "be refused whichever player is asking" in {
      Chain.capturedBy(board, P1, Pos.E5) must throwAn[IllegalArgumentException]
    }

    "still be answerable by the queries that place no stone" in {
      Chain.at(board, Pos.E5) === Set(Pos.E5)
      Chain.hasLiberty(board, Set(Pos.E5)) === true
    }
  }

  "the empty group, which is what Chain.at yields for an empty point" should {
    val board = board9x9("9/9/9/9/4S4/9/9/9/9")

    "have no liberties" in {
      Chain.liberties(board, Set.empty) === Set.empty[Pos]
    }

    "answer hasLiberty false, the same as a dead chain does" in {
      Chain.hasLiberty(board, Set.empty) === false
    }

    "make hasLiberty of Chain.at an unsound way to ask whether a point is captured" in {
      Chain.at(board, Pos.D5) === Set.empty[Pos]
      Chain.hasLiberty(board, Chain.at(board, Pos.D5)) === false
      board.pieces.contains(Pos.D5) === false
    }
  }

  "an L-shaped chain of four on a 13x13 board" should {
    val board = board13x13("13/13/13/13/13/13/13/13/2SS9/2S10/2S10/13/13")

    "reach every stone from the foot of the L" in {
      Chain.at(board, Pos.C3) === Set(Pos.C3, Pos.C4, Pos.C5, Pos.D5)
    }

    "reach every stone from the tip of the L" in {
      Chain.at(board, Pos.D5) === Chain.at(board, Pos.C3)
    }

    "count each shared liberty once" in {
      Chain.liberties(board, Chain.at(board, Pos.C3)) === Set(
        Pos.C2,
        Pos.B3,
        Pos.D3,
        Pos.B4,
        Pos.D4,
        Pos.B5,
        Pos.C6,
        Pos.D6,
        Pos.E5
      )
    }
  }

  "a chain running along the left edge of a 9x9 board" should {
    val board = board9x9("9/9/9/9/9/9/S8/S8/S8")

    "connect all three stones" in {
      Chain.at(board, Pos.A2) === Set(Pos.A1, Pos.A2, Pos.A3)
    }

    "have liberties only on the board" in {
      Chain.liberties(board, Chain.at(board, Pos.A2)) === Set(Pos.B1, Pos.B2, Pos.B3, Pos.A4)
    }
  }

  "a corner stone on a 19x19 board" should {
    val board = board19x19("19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/S18")

    "have exactly two liberties" in {
      Chain.liberties(board, Set(Pos.A1)) === Set(Pos.B1, Pos.A2)
    }

    "have a liberty" in {
      Chain.hasLiberty(board, Set(Pos.A1)) === true
    }
  }

  "a fully surrounded single stone on a 9x9 board" should {
    val board = board9x9("9/9/9/4S4/3SsS3/4S4/9/9/9")

    "still be a chain of one" in {
      Chain.at(board, Pos.E5) === Set(Pos.E5)
    }

    "have no liberties" in {
      Chain.liberties(board, Set(Pos.E5)) === Set.empty[Pos]
    }

    "report no liberty" in {
      Chain.hasLiberty(board, Set(Pos.E5)) === false
    }

    "leave the surrounding stones with liberties" in {
      Chain.hasLiberty(board, Set(Pos.D5)) === true
    }
  }

  "neighbours clipped to the 9x9 board" should {
    val eastEdge = board9x9("9/9/9/9/8S/9/9/9/9")
    val corner   = board9x9("8S/9/9/9/9/9/9/9/9")

    "give a stone on file i no east neighbour" in {
      Chain.liberties(eastEdge, Set(Pos.I5)) === Set(Pos.H5, Pos.I4, Pos.I6)
    }

    "give the top right corner two liberties" in {
      Chain.liberties(corner, Set(Pos.I9)) === Set(Pos.H9, Pos.I8)
    }
  }

  "neighbours clipped to the 13x13 board" should {
    val board = board13x13("13/13/13/13/13/13/12S/13/13/13/13/13/13")

    "give a stone on file m no east neighbour" in {
      Chain.liberties(board, Set(Pos.M7)) === Set(Pos.L7, Pos.M6, Pos.M8)
    }
  }

  "a placement filling the last liberty of a cornered enemy stone" should {
    val board = board9x9("9/9/9/9/9/9/9/S8/s8")

    "capture that stone" in {
      Chain.capturedBy(board, P1, Pos.B1) === Set(Pos.A1)
    }

    "be allowed, with that capture" in {
      Chain.capturesUnlessSuicide(board, P1, Pos.B1) === Some(Set(Pos.A1))
    }
  }

  "a placement on the shared last liberty of two enemy chains" should {
    val board = board9x9("9/9/9/3S1S3/2Ss1sS2/3S1S3/9/9/9")

    "capture both chains" in {
      Chain.capturedBy(board, P1, Pos.E5) === Set(Pos.D5, Pos.F5)
    }

    "capture nothing for the player those stones belong to" in {
      Chain.capturedBy(board, P2, Pos.E5) === Set.empty[Pos]
    }
  }

  "a placement touching one enemy chain on two sides" should {
    val board = board9x9("9/9/3SS4/2SssS3/2Ss5/3S5/9/9/9")

    "capture that chain exactly once" in {
      Chain.capturedBy(board, P1, Pos.E5) === Set(Pos.D5, Pos.D6, Pos.E6)
    }

    "remove three stones" in {
      Chain.capturedBy(board, P1, Pos.E5).size === 3
    }
  }

  "a placement with no enemy chain in atari beside it" should {
    val board = board9x9("9/9/9/9/4S4/9/9/9/9")

    "capture nothing" in {
      Chain.capturedBy(board, P2, Pos.D5) === Set.empty[Pos]
    }

    "be allowed, capturing nothing" in {
      Chain.capturesUnlessSuicide(board, P2, Pos.D5) === Some(Set.empty[Pos])
    }
  }

  "a placement into a corner enclosed by two separate enemy stones" should {
    val board = board9x9("9/9/9/9/9/9/9/s8/1s7")

    "capture nothing" in {
      Chain.capturedBy(board, P1, Pos.A1) === Set.empty[Pos]
    }

    "be refused as suicide" in {
      Chain.capturesUnlessSuicide(board, P1, Pos.A1) === None
    }
  }

  "a placement joining a friendly stone whose last liberty it takes" should {
    val board = board9x9("9/9/9/9/9/9/s8/Ss7/1s7")

    "capture nothing" in {
      Chain.capturedBy(board, P1, Pos.A1) === Set.empty[Pos]
    }

    "be refused as suicide for the whole chain" in {
      Chain.capturesUnlessSuicide(board, P1, Pos.A1) === None
    }
  }

  "a placement that would be suicide but for what it captures" should {
    val board = board9x9("9/9/9/9/9/9/9/sS7/1sS6")

    "capture the enclosing enemy stone" in {
      Chain.capturedBy(board, P1, Pos.A1) === Set(Pos.B1)
    }

    "be allowed, because the capture is what frees it" in {
      Chain.capturesUnlessSuicide(board, P1, Pos.A1) === Some(Set(Pos.B1))
    }
  }

  "the shared traversal flooding empty points instead of stones" should {
    val board = board9x9("9/9/9/4S4/3S1S3/4S4/9/9/9")

    def emptyPoint(board: Board): Pos => Boolean = !board.pieces.contains(_)

    "find a one point eye enclosed by stones" in {
      Chain.regionFrom(board, Pos.E5)(emptyPoint(board)) === Set(Pos.E5)
    }

    "find the whole outer region from any of its points" in {
      Chain.regionFrom(board, Pos.A1)(emptyPoint(board)) ===
        Chain.regionFrom(board, Pos.I9)(emptyPoint(board))
    }

    "hold every empty point but the eye and the stones" in {
      val outer = Chain.regionFrom(board, Pos.A1)(emptyPoint(board))
      outer.size === 76
      outer.contains(Pos.E5) === false
      outer.exists(board.pieces.contains) === false
    }

    "stop at points the predicate refuses, including the origin" in {
      Chain.regionFrom(board, Pos.D5)(emptyPoint(board)) === Set.empty[Pos]
    }

    "be the same traversal that Chain.at uses for stones" in {
      Chain.regionFrom(board, Pos.D5)(pos => board.pieces.get(pos).exists(_.is(P1))) ===
        Chain.at(board, Pos.D5)
    }

    "reach a region that closes back on itself without revisiting it" in {
      val ring = board9x9("9/9/2SSS4/2S1S4/2SSS4/9/9/9/9")
      Chain.regionFrom(ring, Pos.C6)(pos => ring.pieces.get(pos).exists(_.is(P1))) ===
        Chain.at(ring, Pos.C6)
      Chain.at(ring, Pos.C6).size === 8
    }
  }

  "capturesUnlessSuicide across every empty point of the capture and suicide boards" should {
    val boards = List(
      board9x9("9/9/9/9/9/9/9/sS7/1sS6"),
      board9x9("9/9/9/3S1S3/2Ss1sS2/3S1S3/9/9/9"),
      board9x9("9/9/3SS4/2SssS3/2Ss5/3S5/9/9/9"),
      board9x9("9/9/9/9/9/9/s8/Ss7/1s7"),
      board9x9("9/9/9/9/9/9/9/S8/s8")
    )

    val placements = for {
      board  <- boards
      player <- List(P1, P2)
      pos    <- board.variant.boardSize.validPos.filterNot(board.pieces.contains)
    } yield (board, player, pos)

    val capturingPlacements = placements.filter { case (board, player, pos) =>
      Chain.capturedBy(board, player, pos).nonEmpty
    }

    def keysOf(offenders: List[(Board, Player, Pos)]): List[(Player, String)] =
      offenders.map { case (_, player, pos) => (player, pos.key) }

    "find the placements that capture, so the invariants below are not vacuous" in {
      keysOf(capturingPlacements) === List(
        (P1, "a1"),
        (P1, "e5"),
        (P1, "e5"),
        (P2, "a1"),
        (P1, "b1")
      )
    }

    "never refuse a placement that captures" in {
      keysOf(capturingPlacements.filter { case (board, player, pos) =>
        Chain.capturesUnlessSuicide(board, player, pos).isEmpty
      }) === Nil
    }

    "agree with capturedBy wherever it allows the placement" in {
      keysOf(placements.filter { case (board, player, pos) =>
        Chain
          .capturesUnlessSuicide(board, player, pos)
          .exists(_ != Chain.capturedBy(board, player, pos))
      }) === Nil
    }

    "refuse only placements that capture nothing" in {
      keysOf(placements.filter { case (board, player, pos) =>
        Chain.capturesUnlessSuicide(board, player, pos).isEmpty &&
        Chain.capturedBy(board, player, pos).nonEmpty
      }) === Nil
    }
  }

}
