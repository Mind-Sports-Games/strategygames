package strategygames.fiveinarow

import org.specs2.mutable.Specification

import strategygames.GameLogic
import strategygames.format.pgn.{ Reader, Tag, Tags }
import strategygames.variant.{ Variant => StratVariant }

// lila takes a move back by replaying the game through the reader, naming the variant in the tags
class FiveInARowReaderTest extends Specification {

  private def replayedVariant(v: variant.Variant, actionStrs: Vector[Vector[String]]) =
    Reader
      .replayResultFromActionStrs(
        GameLogic.FiveInARow(),
        actionStrs,
        identity,
        Tags(List(Tag(_.Variant, v.name)))
      )
      .andThen(_.valid)
      .fold(err => sys.error(err), _.state.board.variant)

  "a replay from the action strings" should {

    "keep a pente game pente" in {
      replayedVariant(variant.Pente, Vector(Vector("B@j10", "W@j11", "B@k10"))) ===
        StratVariant.FiveInARow(variant.Pente)
    }

    "keep a renju game renju" in {
      replayedVariant(variant.Renju, Vector(Vector("B@h8"))) === StratVariant.FiveInARow(variant.Renju)
    }

    "keep a gomoku game gomoku" in {
      replayedVariant(variant.Gomoku, Vector(Vector("B@h8", "W@h9", "B@i8"))) ===
        StratVariant.FiveInARow(variant.Gomoku)
    }
  }
}
