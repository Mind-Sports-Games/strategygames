package strategygames.fiveinarow

import org.specs2.mutable.Specification

import strategygames.{
  Game => StratGame,
  GameFamily,
  GameLogic,
  MoveMetrics,
  Player,
  Replay => StratReplay,
  Status
}
import strategygames.format.{ Forsyth => StratForsyth, Uci => StratUci }
import strategygames.format.pgn.Binary
import strategygames.fiveinarow.format.Forsyth

// One whole game of gomoku: P1 opens, P2 answers with swap2 and so takes black, P1 stays
// white, and black wins by completing an open four along the rising diagonal e5 to i9.
class FiveInARowFullGameTest extends Specification with FiveInARowTestSupport {

  val record: Vector[Vector[String]] = Vector(
    Vector("B@h8", "W@h9", "B@i8"),
    Vector("swap2", "W@j9", "B@g8"),
    Vector("W@j8"),
    Vector("B@f8"),  // four along rank 8, blocked at j8
    Vector("W@e8"),  // so white must close e8
    Vector("B@g7"),
    Vector("W@k10"),
    Vector("B@g9"),
    Vector("W@g10"),
    Vector("B@g6"),  // four down the g-file, blocked at g10
    Vector("W@g5"),  // so white must close g5
    Vector("B@i7"),
    Vector("W@h6"),
    Vector("B@c12"),
    Vector("W@d12"),
    Vector("B@f6"),
    Vector("W@l11"),
    Vector("B@i9"),  // f6 g7 h8 i9: an open four, with e5 and j10 both empty
    Vector("W@j10"), // white can close only one end
    Vector("B@e5")   // five
  )

  private val actions: List[String] = record.toList.flatMap(t => t)

  private def after(n: Int): Game = play(start, actions.take(n)*)

  private def stateFields(game: Game): String = fen(game).split(' ').drop(1).mkString(" ")

  "a full game of gomoku" should {

    val finished = after(actions.size)

    "pass through each step of the opening" in {
      stateFields(after(3)) === "w 1 c 1"
      after(3).player === Player.P2
      stateFields(after(6)) === "w 2 f 2"
      after(6).player === Player.P1
      stateFields(after(7)) === "b 2 - 2"
      after(7).player === Player.P2
    }

    "leave P2 holding black once the opening is over" in {
      finished.situation.blackSeat === Player.P2
      finished.situation.board(Pos.fromKey("h8").get).map(_.player) === Some(Player.P2)
      finished.situation.board(Pos.fromKey("h9").get).map(_.player) === Some(Player.P1)
    }

    "not end on the fours that white closed in time" in {
      after(actions.size - 1).situation.end must beFalse
      after(actions.size - 1).player === Player.P2
    }

    "end on the fifth stone, won by the seat holding black" in {
      finished.situation.end must beTrue
      finished.situation.status === Some(Status.VariantEnd)
      finished.situation.board.variant.colourWithLine(finished.situation.board) === Some(BlackStone)
      finished.situation.winner === Some(Player.P2)
    }

    "offer nothing more once it is won" in {
      finished.situation.dropsAsDrops must beEmpty
      finished.situation.canSwap must beFalse
      refused(finished, "W@a1") must beTrue
    }

    "record the opening's turns whole and every later turn as one stone" in {
      finished.actionStrs === record
      finished.turnCount === record.size
      finished.plies === actions.size
      finished.situation.board.pieces.size === actions.size - 1
    }

    "write a fen that reads back to the same finished position" in {
      stateFields(finished) === "w 2 - 11"
      Forsyth.<<(format.FEN(fen(finished))).map(_.winner) === Some(Some(Player.P2))
    }
  }

  "the same game through the wrapper" should {

    val lib     = GameLogic.FiveInARow()
    val gf      = GameFamily.FiveInARow()
    val variant = strategygames.variant.Variant.FiveInARow(strategygames.fiveinarow.variant.Gomoku)

    val played = actions.foldLeft(StratGame.apply(lib, variant)) { (g, str) =>
      g.applyUci(StratUci.apply(lib, gf, str).get, MoveMetrics()).fold(sys.error(_), _._1)
    }

    "finish with the same winner and record" in {
      played.situation.winner === Some(Player.P2)
      played.actionStrs === record
    }

    "replay from the record to the same position" in {
      StratReplay
        .gameFromUciStrings(lib, record, Player.P2, None, variant)
        .toOption
        .map(g => StratForsyth.>>(lib, g).value) === Some(StratForsyth.>>(lib, played).value)
    }

    "survive the binary format turn for turn" in {
      Binary.readActionStrs(lib, Binary.writeActionStrs(gf, record).get.toList).get === record
    }
  }
}
