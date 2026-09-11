package strategygames.go

import org.specs2.mutable.Specification

import strategygames.go.format.{ FEN, Forsyth, Uci }
import strategygames.go.variant.Go9x9

class GoEndingTest extends Specification with GoRulesTestSupport {

  private val scriptedNineByNine =
    List("g3", "c7", "f2", "e5", "e1", "d4", "h4", "i1", "i5", "c5", "d5", "d6")

  private val scriptedReplayStart = FEN(s"9/9/6S2/9/9/9/2S6/9/9${pocket} w - 810 65 0 0 65 0 1")

  private val scriptedReplay =
    List("s@g3", "s@c7", "s@f2", "s@e5", "s@e1", "s@d4", "s@h4", "s@i1", "s@i5", "pass", "pass", "ss:i1")

  private def settling(game: Game, uci: String): Game =
    Uci(uci)
      .map(game.apply(_).map { case (next, _) => next })
      .getOrElse(sys.error(s"unreadable go uci: ${uci}"))
      .valueOr(error => sys.error(s"cannot settle with ${uci}: ${error}"))

  private def scriptedReplayFens: Option[List[FEN]] =
    Replay
      .situationsFromUci(scriptedReplay.flatMap(Uci(_)), Some(scriptedReplayStart), Go9x9)
      .toOption
      .map(_.map(Forsyth.>>))

  "two passes" should {
    val afterTwoPasses = playing(Go9x9, List("pass", "pass"))
    "not end the game" in {
      afterTwoPasses.situation.end === false
    }
    "leave every point of the board playable" in {
      dropKeysOf(afterTwoPasses.situation).size === 81
    }
    "open dead stone selection" in {
      afterTwoPasses.situation.canSelectSquares === true
    }
    "have dead stone selection closed again by a placement" in {
      playingOn(afterTwoPasses, List("e5")).situation.canSelectSquares === false
    }
  }

  "dead stone selection" should {
    val awaitingSelection = playing(Go9x9, scriptedNineByNine ++ List("pass", "pass"))
    "need an even run of passes: one pass does not open it" in {
      playing(Go9x9, scriptedNineByNine ++ List("pass")).situation.canSelectSquares === false
    }
    "need an even run of passes: a third pass closes it again" in {
      playing(Go9x9, scriptedNineByNine ++ List("pass", "pass", "pass")).situation.canSelectSquares === false
    }
    "reopen on the next pair of passes after a placement" in {
      playing(
        Go9x9,
        scriptedNineByNine ++ List("pass", "pass", "pass", "b1", "pass", "pass")
      ).situation.canSelectSquares === true
    }
    "be what ends the game, rather than the passes that opened it" in {
      (awaitingSelection.situation.end === false) and
        (playingOn(awaitingSelection, List("ss:i1")).situation.end === true)
    }
    "accept an empty settlement, which lifts nothing and still ends the game" in {
      val settledEmpty = playingOn(awaitingSelection, List("ss:"))
      (settledEmpty.situation.end === true) and
        (settledEmpty.board.pieces.size === awaitingSelection.board.pieces.size)
    }
    "offer no further drops once the game has ended" in {
      playingOn(awaitingSelection, List("ss:i1")).situation.drops === None
    }
  }

  "a settlement" should {
    val awaitingSelection = playing(Go9x9, scriptedNineByNine ++ List("pass", "pass"))
    val settled           = playingOn(awaitingSelection, List("ss:i1"))
    "advance the ply count by one" in {
      settled.plies === awaitingSelection.plies + 1
    }
    "hand the turn on, as any other action does" in {
      (settled.situation.player === !awaitingSelection.situation.player) and
        (fenOf(awaitingSelection).value.split(' ')(1) === "b") and
        (fenOf(settled).value.split(' ')(1) === "w")
    }
  }

  "four consecutive passes" should {
    "end the game without anyone naming a dead stone" in {
      playing(Go9x9, List("e5", "pass", "pass", "pass", "pass")).situation.end === true
    }
    "not end it when a stone splits them into two pairs" in {
      playing(Go9x9, List("e5", "pass", "pass", "d5", "pass", "pass")).situation.end === false
    }
    "warn about the subsequent pass only while a pair stands unbroken" in {
      val fourPasses = List("e5", "pass", "pass", "pass", "pass")
      (playing(Go9x9, fourPasses.take(3)).situation.isSubsequentPassWarning === true) and
        (playing(Go9x9, fourPasses.take(4)).situation.isSubsequentPassWarning === true) and
        (playing(Go9x9, fourPasses).situation.isSubsequentPassWarning === false)
    }
  }

  "a key naming a square the board does not have" should {
    val awaitingSelection = playing(Go9x9, scriptedNineByNine ++ List("pass", "pass"))
    "be ignored in a settlement, which then behaves as an empty one" in {
      fenOf(playingOn(awaitingSelection, List("ss:n4"))) ===
        fenOf(playingOn(awaitingSelection, List("ss:")))
    }
    "not be aliased onto an on board point of the same index" in {
      stoneAt(playingOn(awaitingSelection, List("ss:n4")), "e5") === Some(Piece(P2, Stone))
    }
    "be refused outright as a drop" in {
      awaitingSelection.situation.drop(Role.defaultRole, pointAt("n4")).isInvalid === true
    }
  }

  "a key that is no coordinate at all" should {
    val awaitingSelection = playing(Go9x9, scriptedNineByNine ++ List("pass", "pass"))

    "be dropped from a settlement, leaving the empty settlement behind" in {
      (Uci("ss:zz") === Some(Uci.SelectSquares(Nil))) and
        (fenOf(settling(awaitingSelection, "ss:zz")) === fenOf(settling(awaitingSelection, "ss:")))
    }

    "be dropped from a settlement naming readable keys beside it" in {
      (Uci("ss:i1,zz") === Some(Uci.SelectSquares(List(pointAt("i1"))))) and
        (fenOf(settling(awaitingSelection, "ss:i1,zz")) === fenOf(settling(awaitingSelection, "ss:i1")))
    }

    "be an error as a drop, which a settlement key of the same shape is not" in {
      (Uci("s@zz") must throwAn[Exception]) and
        (settling(awaitingSelection, "ss:zz").situation.end === true)
    }
  }

  "the scripted 9x9 replay" should {
    "walk through its thirteen recorded fens" in {
      scriptedReplayFens must beSome(
        List(
          FEN(s"9/9/6S2/9/9/9/2S6/9/9${pocket} w - 810 65 0 0 65 0 1"),
          FEN(s"9/9/6S2/9/9/9/2S3s2/9/9${pocket} b - 20 75 0 0 65 0 2"),
          FEN(s"9/9/2S3S2/9/9/9/2S3s2/9/9${pocket} w - 30 75 0 0 65 0 2"),
          FEN(s"9/9/2S3S2/9/9/9/2S3s2/5s3/9${pocket} b - 30 85 0 0 65 0 3"),
          FEN(s"9/9/2S3S2/9/4S4/9/2S3s2/5s3/9${pocket} w - 40 85 0 0 65 0 3"),
          FEN(s"9/9/2S3S2/9/4S4/9/2S3s2/5s3/4s4${pocket} b - 40 95 0 0 65 0 4"),
          FEN(s"9/9/2S3S2/9/4S4/3S5/2S3s2/5s3/4s4${pocket} w - 50 95 0 0 65 0 4"),
          FEN(s"9/9/2S3S2/9/4S4/3S3s1/2S3s2/5s3/4s4${pocket} b - 50 105 0 0 65 0 5"),
          FEN(s"9/9/2S3S2/9/4S4/3S3s1/2S3s2/5s3/4s3S${pocket} w - 60 105 0 0 65 0 5"),
          FEN(s"9/9/2S3S2/9/4S3s/3S3s1/2S3s2/5s3/4s3S${pocket} b - 60 115 0 0 65 0 6"),
          FEN(s"9/9/2S3S2/9/4S3s/3S3s1/2S3s2/5s3/4s3S${pocket} w - 60 115 0 0 65 1 6"),
          FEN(s"9/9/2S3S2/9/4S3s/3S3s1/2S3s2/5s3/4s3S${pocket} b - 60 115 0 0 65 2 7"),
          FEN(s"9/9/2S3S2/9/4S3s/3S3s1/2S3s2/5s3/4s4${pocket} w - 50 215 0 0 65 3 7")
        )
      )
    }
    "leave board and scores untouched across the two passes, moving only the pass count" in {
      scriptedReplayFens.map(_.slice(9, 12).map(fen => (fen.board, fen.player1Score, fen.player2Score))) must
        beSome(List.fill(3)(("9/9/2S3S2/9/4S3s/3S3s1/2S3s2/5s3/4s3S", 60, 115)))
    }
    "count the passes as they accumulate" in {
      scriptedReplayFens.map(_.slice(9, 12).map(_.fenPassCount)) must beSome(List(0, 1, 2))
    }
  }
}
