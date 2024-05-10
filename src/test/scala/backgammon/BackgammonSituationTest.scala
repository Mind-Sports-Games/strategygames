package strategygames.backgammon

import cats.data.Validated._
import org.specs2.matcher.ValidatedMatchers
import org.specs2.mutable.Specification

import strategygames.Player

class BackgammonSituationTest extends Specification with ValidatedMatchers {

  "valid initial situation" should {
    val situation = Situation(Board.init(variant.Backgammon), Player.P1)

    "have valid action options" in {
      situation.canMove must_== false
      situation.canDrop must_== false
      situation.canLift must_== false
      situation.canRollDice must_== true
      situation.canEndTurn must_== false
      situation.canOnlyDrop must_== false
      situation.canOnlyLift must_== false
      situation.canOnlyRollDice must_== true
      situation.canOnlyEndTurn must_== false
      situation.canCapture must_== false
      situation.forcedAction.nonEmpty must_== false
      situation.board.unusedDice.isEmpty must_== true
      situation.board.usedDice.isEmpty must_== true
      situation.board.piecesOnBar(Player.P1) must_== false
      situation.board.piecesOnBar(Player.P2) must_== false
      situation.board.piecesOnBoardCount must_== 30
      situation.board.piecesCanLift(Player.P1) must_== false
      situation.board.piecesCanLift(Player.P2) must_== false
      situation.board.furthestFromEnd(Player.P1) must_== 24
      situation.board.furthestFromEnd(Player.P2) must_== 24
      situation.board.history.hasRolledDiceThisTurn must_== false
    }
    "have valid dice rolls" in {
      situation.diceRolls.size must_== 30
    }
  }

  "valid game after first dice roll" should {
    val init = Game.apply(variant.Backgammon)

    val g1option = init.randomizeDiceRoll.map(init.applyDiceRoll)
    "be a valid game after a random dice roll" in {
      g1option.nonEmpty must_== true
    }
    // We've verified that orElse wont be hit in the test above
    // but we need to use the non option value now
    val g1       = g1option.getOrElse(init.pp("this should never print"))
    "have valid moves after initial random dice roll" in {
      g1.situation.canMove must_== true
      g1.situation.canDrop must_== false
      g1.situation.canLift must_== false
      g1.situation.canRollDice must_== false
      g1.situation.canEndTurn must_== false
      g1.situation.canOnlyDrop must_== false
      g1.situation.canOnlyLift must_== false
      g1.situation.canOnlyRollDice must_== false
      g1.situation.canOnlyEndTurn must_== false
      g1.situation.canCapture must_== false
      g1.situation.forcedAction.nonEmpty must_== false
      g1.situation.board.unusedDice.size must_== 2
      g1.situation.board.usedDice.size must_== 0
      g1.situation.board.piecesOnBar(Player.P1) must_== false
      g1.situation.board.piecesOnBar(Player.P2) must_== false
      g1.situation.board.piecesOnBoardCount must_== 30
      g1.situation.board.piecesCanLift(Player.P1) must_== false
      g1.situation.board.piecesCanLift(Player.P2) must_== false
      g1.situation.board.furthestFromEnd(Player.P1) must_== 24
      g1.situation.board.furthestFromEnd(Player.P2) must_== 24
      g1.situation.board.history.hasRolledDiceThisTurn must_== true
      init.situation.board.pieces must_== g1.situation.board.pieces
    }
    val m1       = g1.situation.moves.values.flatten.head
    val g2       = g1.apply(m1)
    "be valid after initial first move" in {
      g2.situation.canMove must_== true
      g2.situation.canDrop must_== false
      g2.situation.canLift must_== false
      g2.situation.canRollDice must_== false
      g2.situation.canEndTurn must_== false
      g2.situation.canOnlyDrop must_== false
      g2.situation.canOnlyLift must_== false
      g2.situation.canOnlyRollDice must_== false
      g2.situation.canOnlyEndTurn must_== false
      g2.situation.canCapture must_== false
      g2.situation.forcedAction.nonEmpty must_== false
      g2.situation.board.unusedDice.size must_== 1
      g2.situation.board.usedDice.size must_== 1
      g2.situation.board.piecesOnBar(Player.P1) must_== false
      g2.situation.board.piecesOnBar(Player.P2) must_== false
      g2.situation.board.piecesOnBoardCount must_== 30
      g2.situation.board.piecesCanLift(Player.P1) must_== false
      g2.situation.board.piecesCanLift(Player.P2) must_== false
      g2.situation.board.history.hasRolledDiceThisTurn must_== true
    }
    val m2       = g2.situation.moves.values.flatten.head
    val g3       = g2.apply(m2)
    "be valid after initial second move" in {
      g3.situation.canMove must_== false
      g3.situation.canDrop must_== false
      g3.situation.canLift must_== false
      g3.situation.canRollDice must_== false
      g3.situation.canEndTurn must_== true
      g3.situation.canOnlyDrop must_== false
      g3.situation.canOnlyLift must_== false
      g3.situation.canOnlyRollDice must_== false
      g3.situation.canOnlyEndTurn must_== true
      g3.situation.canCapture must_== false
      g3.situation.forcedAction.nonEmpty must_== false
      g3.situation.board.unusedDice.size must_== 0
      g3.situation.board.usedDice.size must_== 2
      g3.situation.board.piecesOnBar(Player.P1) must_== false
      g3.situation.board.piecesOnBar(Player.P2) must_== false
      g3.situation.board.piecesOnBoardCount must_== 30
      g3.situation.board.piecesCanLift(Player.P1) must_== false
      g3.situation.board.piecesCanLift(Player.P2) must_== false
      g3.situation.board.history.hasRolledDiceThisTurn must_== true
      g3.situation.board.history.didRollDiceLastTurn must_== false
    }
    val g4       = g3.endTurn() match {
      case Valid((g, _)) => g
      case _             => g3.pp("this should never print")
    }
    "be valid after first turn ends" in {
      g4.situation.canMove must_== false
      g4.situation.canDrop must_== false
      g4.situation.canLift must_== false
      g4.situation.canRollDice must_== true
      g4.situation.canEndTurn must_== false
      g4.situation.canOnlyDrop must_== false
      g4.situation.canOnlyLift must_== false
      g4.situation.canOnlyRollDice must_== true
      g4.situation.canOnlyEndTurn must_== false
      g4.situation.canCapture must_== false
      g4.situation.forcedAction.nonEmpty must_== false
      g4.situation.diceRolls.size must_== 36
      g4.situation.board.unusedDice.size must_== 0
      g4.situation.board.usedDice.size must_== 0
      g4.situation.board.piecesOnBar(Player.P1) must_== false
      g4.situation.board.piecesOnBar(Player.P2) must_== false
      g4.situation.board.piecesOnBoardCount must_== 30
      g4.situation.board.piecesCanLift(Player.P1) must_== false
      g4.situation.board.piecesCanLift(Player.P2) must_== false
      g4.situation.board.history.hasRolledDiceThisTurn must_== false
      g4.situation.board.history.didRollDiceLastTurn must_== true
      g4.situation.player must_!= g3.situation.player
    }

  }

}
