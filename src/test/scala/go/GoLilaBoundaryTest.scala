package strategygames.go

import org.specs2.mutable.Specification

import strategygames.Score
import strategygames.go.format.{ FEN, Forsyth }
import strategygames.go.variant.{ Go19x19, Go9x9, Variant }

// NOTE: the shape lila's stored-game reader constructs with, transcribed from `readGoGame`,
// `Rematcher` and `SgfDump`. Nothing else in this repository builds a board that way, so this is
// the only exercise the restore path has.
// TODO(lila): delete with `StoredPosition` and the compatibility `Board.apply`.
class GoLilaBoundaryTest extends Specification with GoRulesTestSupport {

  import GoLilaBoundaryTest._

  "the board lila rebuilds a stored game into" should {

    "compile from the fields lila has, naming no position state" in {
      restored(Go9x9, List("s@e5")).pieces.size === 1
    }

    "take the ply count from the moves, not from the clock lila hands over" in {
      // lila derives halfMoveClock with a chess heuristic — plies since the last capture or pawn move —
      // which answers 0 for every go game, because every go action string starts with a lower-case letter.
      (restored(Go9x9, List("s@e5", "s@d5", "s@c5")).history.halfMoveClock === 3) and
        (lilaHistory.halfMoveClock === 0)
    }

    "export the full move it stands at, rather than always the first" in {
      FEN(Forsyth.exportBoard(restored(Go9x9, List("s@e5", "s@d5", "s@c5", "s@b5")))).fullMove === Some(3)
    }

    "hand the move to the player whose turn it is" in {
      (playerOf(restored(Go9x9, List("s@e5"))) === "w") and
        (playerOf(restored(Go9x9, List("s@e5", "s@d5"))) === "b")
    }
  }

  "the position state lila stores nothing for" should {

    "count a run of passes, so the dead stone offer is still on the table after a reload" in {
      val afterTwoPasses = restored(Go9x9, List("s@e5", "s@d5", "pass", "pass"))
      (afterTwoPasses.consecutivePasses === 2) and
        (Situation(afterTwoPasses, P1).canSelectSquares === true)
    }

    "leave the offer off the table when only one player has passed" in {
      val afterOnePass = restored(Go9x9, List("s@e5", "s@d5", "pass"))
      (afterOnePass.consecutivePasses === 1) and
        (Situation(afterOnePass, P2).canSelectSquares === false)
    }

    "know the game is over once the dead stones were agreed" in {
      val settled = restored(Go9x9, List("s@e5", "s@d5", "pass", "pass", "ss:e5"))
      (settled.deadStonesSelected === true) and
        (Situation(settled, P1).end === true)
    }

    "recover the ko point, so the recapture stays forbidden across a reload" in {
      val inKo = restored(Go9x9, koSequence)
      (inKo.ko === Pos.fromKey("f5")) and
        (dropKeysOf(Situation(inKo, P2)) must not(contain("f5")))
    }

    "answer no ko point when the last placement made none" in {
      restored(Go9x9, List("s@e5", "s@d5")).ko === None
    }

    "take komi from the starting position when the game named one" in {
      (restored(Go9x9, List("s@e5")).komi === 5.5) and
        (restoredFrom(handicapFen, Go9x9, List("s@e5")).komi === 3.5) and
        (Go9x9.komi === 5.5)
    }
  }

  "the starting position lila reads back off a restored board" should {

    // Rematcher.scala:111 — a rematch of a handicap game inherits its start.
    "answer the fen the game began at" in {
      restoredFrom(handicapFen, Go9x9, List("s@e5")).apiPosition.initialFen === handicapFen
    }

    // SgfDump.scala:141-142 — the SGF names the komi and handicap the game began with.
    "answer it still, after the game has been played on" in {
      val played = restoredFrom(handicapFen, Go9x9, List("s@e5"))
      val onward = played.variant.boardAfter(Situation(played, P2), pointAt("d5"))
      (onward.apiPosition.initialFen.komi === 3.5) and
        (onward.apiPosition.initialFen.handicap === Some(2))
    }

    // Rematcher.scala:177 — go from position, i.e. a handicapped start.
    "be installable onto a fresh board, komi and all" in {
      val carried = Board(handicapFen.pieces, Go9x9).withPosition(Some(StoredPosition(handicapFen, List())))
      (carried.komi === 3.5) and (carried.apiPosition.initialFen === handicapFen)
    }

    "fall back to the variant's own start when the game named none" in {
      Board.init(Go19x19).apiPosition.initialFen === Go19x19.initialFen
    }
  }
}

object GoLilaBoundaryTest {

  // `s@`-prefixed, as lila stores them
  val koSequence: List[String] =
    List("g5", "f5", "f4", "e4", "f6", "d5", "h5", "e6", "e5").map(key => s"s@${key}")

  // two handicap stones, komi 3.5 — `Go9x9.fenFromSetupConfig` shape, with the stones written in
  val handicapFen: FEN =
    FEN("9/9/6S2/9/9/9/2S6/9/9[SSSSSSSSSSssssssssss] w - 20 35 0 0 35 0 1")

  val lilaHistory: History =
    History(
      lastTurn = List.empty,
      currentTurn = List.empty,
      halfMoveClock = 0,
      positionHashes = Array.empty,
      score = Score(0, 0),
      captures = Score(0, 0)
    )

  def restoredFrom(initialFen: FEN, goVariant: Variant, uciMoves: List[String]): Board =
    Board(
      pieces = piecesAfter(initialFen, goVariant, uciMoves),
      history = lilaHistory,
      variant = goVariant,
      pocketData = Some(PocketData.init),
      uciMoves = uciMoves,
      position = Some(Api.positionFromStartingFenAndMoves(initialFen, uciMoves))
    )

  def restored(goVariant: Variant, uciMoves: List[String]): Board =
    Board(
      pieces = piecesAfter(goVariant.initialFen, goVariant, uciMoves),
      history = lilaHistory,
      variant = goVariant,
      pocketData = Some(PocketData.init),
      uciMoves = uciMoves,
      position = None
    )

  // lila reads the stones out of its own binary column; here they come from a straight replay, which is
  // the same stones by a different route.
  private def piecesAfter(initialFen: FEN, goVariant: Variant, uciMoves: List[String]): PieceMap =
    Replay
      .situationsFromUci(uciMoves.flatMap(format.Uci.apply), Some(initialFen), goVariant)
      .toOption
      .flatMap(_.lastOption)
      .map(_.board.pieces)
      .getOrElse(sys.error(s"could not build the stones for ${uciMoves}"))

  def playerOf(board: Board): String = Forsyth.exportBoard(board).split(' ')(1)
}
