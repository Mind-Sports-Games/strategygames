package strategygames.go.engine

import org.specs2.mutable.Specification

class GoGameTest extends Specification {

  import GoEngineTestSupport._

  private def play(game: GoGame, keys: List[String]): GoGame =
    keys.foldLeft(game) { (played, key) =>
      played.play(if (key == "pass") played.state.passMove else engineMove(played.state.size, key))
    }

  private val nineByNine = GoGame.initial(9, 5.5)

  "a fresh game" should {
    "not have ended" in {
      (nineByNine.ended === false) and
        (nineByNine.inDeadStoneSelectionPhase === false) and
        (nineByNine.plyCount === 0) and
        (nineByNine.fullMoveNumber === 1) and
        (nineByNine.fenPassCount === 0)
    }
    "score komi to white alone" in {
      (nineByNine.p1Score === 0.0) and
        (nineByNine.p2Score === 5.5) and
        (nineByNine.p1FenScore === 0) and
        (nineByNine.p2FenScore === 55) and
        (nineByNine.gameScore === -55) and
        (nineByNine.gameOutcome === -1000)
    }
  }

  "two consecutive passes" should {
    val passed = play(nineByNine, List("pass", "pass"))
    "open dead stone selection without ending the game" in {
      (passed.inDeadStoneSelectionPhase === true) and
        (passed.ended === false) and
        (passed.fenPassCount === 2)
    }
    "still allow drops" in {
      passed.state.legalMoves.length === 82
    }
  }

  "selecting dead stones" should {
    val passed   = GoFen
      .parse(
        "4S4/4S4/4S4/4S4/4S1s2/4S4/4S4/4S4/4S4[SSSSSSSSSSssssssssss] b - 450 65 0 0 55 2 1"
      )
      .getOrElse(nineByNine)
    val resolved = passed.selectDeadStones(List(engineMove(9, "g5")))
    "end the game" in {
      (resolved.ended === true) and (resolved.fenPassCount === 3)
    }
    "remove the selected stones and rescore" in {
      (passed.p1Score === 45.0) and
        (passed.p2Score === 6.5) and
        (resolved.state.stoneOwnerAt(engineMove(9, "g5")) === GoState.NoOwner) and
        (resolved.p1Score === 81.0) and
        (resolved.p2Score === 5.5) and
        (resolved.gameOutcome === 1000) and
        (resolved.winningPlayer === Some(GoState.BlackPlayer))
    }
    "advance the ply count by one and keep the player to move" in {
      (resolved.plyCount === passed.plyCount + 1) and
        (resolved.state.turn === passed.state.turn)
    }
    "accept an empty selection" in {
      val agreed = passed.selectDeadStones(Nil)
      (agreed.ended === true) and (agreed.p1Score === 45.0) and (agreed.p2Score === 6.5)
    }
  }

  "equal area with no komi" should {
    val fen  =
      "3S1s3/3S1s3/3S1s3/3S1s3/3S1s3/3S1s3/3S1s3/3S1s3/3S1s3[SSSSSSSSSSssssssssss] b - 360 360 0 0 0 0 1"
    val game = GoFen.parse(fen).toOption
    "be a draw" in {
      (game.map(_.p1FenScore) === Some(360)) and
        (game.map(_.p2FenScore) === Some(360)) and
        (game.map(_.gameScore) === Some(0)) and
        (game.map(_.gameOutcome) === Some(0)) and
        (game.flatMap(_.winningPlayer) === None)
    }
    "emit the fen it was parsed from" in {
      game.map(GoFen.render) === Some(fen)
    }
  }

  "changing komi" should {
    "move the white score and the outcome" in {
      val generous = nineByNine.withKomi(0.0)
      (generous.p2FenScore === 0) and
        (generous.komiTenths === 0) and
        (generous.gameScore === 0) and
        (generous.gameOutcome === 0)
    }
  }
}
