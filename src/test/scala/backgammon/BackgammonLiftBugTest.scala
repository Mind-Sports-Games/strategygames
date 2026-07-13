package strategygames.backgammon

import strategygames.Player
import strategygames.backgammon.format.{ FEN, Forsyth }

// NOTE: helper lives outside the spec — computing over Action *inside* a specs2
// example triggers its implicit Action -> ValueCheck conversion (cf. BackgammonTestUtils).
object BackgammonLiftBugUtils {
  def turnPlayers(validTurns: List[List[Action]]): Set[Player] =
    validTurns.flatten.map(_.player).toSet
}

// A game-winning bear-off must yield the clean single-action turn `[^l1]` (gnubg
// agrees the only legal play here is `1/off`). Regression guard for the
// Situation.nextTurn fix: a game-ending lift flips Lift.playerAfter to the
// opponent, so turn assembly used to recurse into the opponent's situation and
// append a phantom P2 action with the die the win left unused — the completed
// `[^l1]` was never emitted and the bot's turn-matcher got NoMatchingTurn.
//
// NOTE: FEN ace index — stoneArray index 0 (Pos.L1, P1's bear-off point) is the
// LAST comma entry of the SECOND board half, so ".../11,1S" is one P1 checker on
// the ace (see FEN.stoneArray / Forsyth).
class BackgammonLiftBugTest extends BackgammonTest {

  // P1: 1 checker on the ace (index 0), 14 lifted. P2: 3 checkers off the race,
  // 12 lifted. Roll 6-2, both dice unused, P1 (w) to play.
  val fen: FEN = FEN("6,3s,5/11,1S[] 6/2 - w 14 12 - 99")

  val situation: Situation =
    Forsyth.<<(fen).getOrElse(sys.error("could not read bear-off FEN"))

  "single checker on the ace, rolling 6-2 (game-winning overshoot)" should {

    // Pin the position + roll, and confirm the lift itself IS generated — the
    // bug was only in turn assembly, not lift generation.
    "have the position we think it does" in {
      situation.player === Player.P1
      // the lone checker sits on the ace: 1 pip from bearing off
      situation.board.furthestFromEnd(Player.P1) === 1
      situation.board.piecesCanLift(Player.P1) === true
      situation.board.unusedDice.sorted === List(2, 6)
      situation.board.pieces.get(Pos.L1).map(_._2) === Some(1)
      situation.board.history.hasRolledDiceThisTurn === true
      // the game-winning bear-off IS available as a single lift
      situation.canLift === true
      liftsToUciSet(situation.lifts) === Set("^l1")
    }

    "offer the completed game-winning turn" in {
      situation.validTurns.map(actionsToUciList).contains(List("^l1")) === true
    }

    "not leak an opponent action into the winning turn" in {
      BackgammonLiftBugUtils.turnPlayers(situation.validTurns) === Set(Player.P1)
    }
  }

  // Control: a non-game-ending bear-off (two on the 6-point, roll 6-2) never
  // flips to the opponent, so its turns stay clean all-P1. Isolates the fix to
  // the game-ending overshoot that leaves a die unused.
  "two checkers on the 6-point, rolling 6-2 (non game-ending bear-off)" should {
    val control: Situation =
      Forsyth
        .<<(FEN("6,3s,5/6,2S,5[] 6/2 - w 13 12 - 99"))
        .getOrElse(sys.error("could not read control FEN"))

    "have the position we think it does" in {
      control.player === Player.P1
      // both checkers on the 6-point: 6 pips from bearing off
      control.board.furthestFromEnd(Player.P1) === 6
      control.board.pieces.get(Pos.G1).map(_._2) === Some(2)
    }

    "offer the bear-off as a clean, all-P1 completed turn" in {
      control.canLift === true
      control.lifts.nonEmpty === true
      control.validTurns.nonEmpty === true
      BackgammonLiftBugUtils.turnPlayers(control.validTurns) === Set(Player.P1)
    }
  }
}
