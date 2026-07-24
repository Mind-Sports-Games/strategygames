package strategygames.bench

import cats.data.Validated
import org.specs2.mutable.Specification

import strategygames.{ ActionStrs, GameLogic }
import strategygames.format.{ FEN, GameToUciStrings, UciDump }
import strategygames.variant.Variant

class GameToUciStringsChessSpec extends Specification {

  private val lib     = GameLogic.Chess()
  private val variant = Variant.libStandard(lib)

  private def joined(actionStrs: ActionStrs): String =
    actionStrs.map(_.mkString(",")).mkString(" ")

  private def baseline(actionStrs: ActionStrs, fen: Option[FEN]): String =
    UciDump(lib, actionStrs, fen, variant).map(joined).getOrElse(sys.error("UciDump invalid for chess case"))

  private def check(actionStrs: ActionStrs, fen: Option[FEN]) =
    GameToUciStrings(lib, actionStrs, fen, variant) must beEqualTo(Validated.valid(baseline(actionStrs, fen)))

  private def fen(value: String): Option[FEN] = Some(FEN.apply(lib, value))

  private def game(moves: String*): ActionStrs = moves.toVector.map(Vector(_))

  private val promotionFen = fen("8/P7/8/4k3/8/8/8/4K3 w - - 0 1")

  "GameToUciStrings chess Standard fast path" should {

    "match UciDump for kingside castling by both players" in {
      check(game("e4", "e5", "Nf3", "Nf6", "Bc4", "Bc5", "O-O", "O-O"), None)
    }

    "match UciDump for queenside castling by both players" in {
      check(game("d4", "d5", "Nc3", "Nc6", "Bf4", "Bf5", "Qd2", "Qd7", "O-O-O", "O-O-O"), None)
    }

    "match UciDump for queen promotion" in {
      check(game("a8=Q"), promotionFen)
    }

    "match UciDump for knight, rook and bishop underpromotion" in {
      check(game("a8=N"), promotionFen) and
        check(game("a8=R"), promotionFen) and
        check(game("a8=B"), promotionFen)
    }

    "match UciDump for en passant capture" in {
      check(game("exf3"), fen("4k3/8/8/8/4pP2/8/8/4K3 b - f3 0 1"))
    }

    "match UciDump for file disambiguation" in {
      check(game("Ncd5"), fen("4k3/8/8/8/8/2N1N3/8/4K3 w - - 0 1"))
    }

    "match UciDump for rank disambiguation" in {
      check(game("R1a3"), fen("R7/8/8/8/8/8/8/R3K1k1 w - - 0 1"))
    }
  }
}
