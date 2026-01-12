package strategygames.backgammon

import cats.data.Validated._
import org.specs2.matcher.ValidatedMatchers
import org.specs2.mutable.Specification

import strategygames.{ Player, Status }

class BackgammonSituationTest extends Specification with ValidatedMatchers {

  "valid initial situation" should {
    val situation = Situation(Board.init(variant.Backgammon).initialiseCube, Player.P1)

    "have valid action options" in {
      situation.canMove === false
      situation.canDrop === false
      situation.canLift === false
      situation.canRollDice === true
      situation.canEndTurn === false
      situation.canCubeAction === false
      situation.canOnlyDrop === false
      situation.canOnlyLift === false
      situation.canOnlyRollDice === true
      situation.canOnlyEndTurn === false
      situation.canOnlyCubeAction === false
      situation.canCapture === false
      situation.forcedAction.nonEmpty === false
      situation.board.unusedDice.isEmpty === true
      situation.board.usedDice.isEmpty === true
      situation.board.piecesOnBar(Player.P1) === false
      situation.board.piecesOnBar(Player.P2) === false
      situation.board.piecesOnBoardCount === 30
      situation.board.piecesCanLift(Player.P1) === false
      situation.board.piecesCanLift(Player.P2) === false
      situation.board.furthestFromEnd(Player.P1) === 24
      situation.board.furthestFromEnd(Player.P2) === 24
      situation.board.pipCount(Player.P1) === 167
      situation.board.pipCount(Player.P2) === 167
      situation.board.racePosition === false
      situation.board.history.hasRolledDiceThisTurn === false
    }
    "have valid dice rolls" in {
      situation.diceRolls.size === 30
    }
  }

  "valid game after first dice roll" should {
    val init: Game = Game.apply(variant.Backgammon)
    val g0: Game   = init.copy(situation = init.situation.copy(board = init.board.initialiseCube))

    val g1option: Option[Game] = g0.randomizeDiceRoll.map(g0.applyDiceRoll)
    "be a valid game after a random dice roll" in {
      g1option.nonEmpty === true
    }
    // We've verified that orElse wont be hit in the test above
    // but we need to use the non option value now
    val g1: Game = g1option.getOrElse(init.pp("this should never print"))
    "have valid moves after initial random dice roll" in {
      g1.situation.canMove === true
      g1.situation.canDrop === false
      g1.situation.canLift === false
      g1.situation.canRollDice === false
      g1.situation.canEndTurn === false
      g1.situation.canCubeAction === false
      g1.situation.canOnlyDrop === false
      g1.situation.canOnlyLift === false
      g1.situation.canOnlyRollDice === false
      g1.situation.canOnlyEndTurn === false
      g1.situation.canOnlyCubeAction === false
      g1.situation.canCapture === false
      g1.situation.forcedAction.nonEmpty === false
      g1.situation.board.unusedDice.size === 2
      g1.situation.board.usedDice.size === 0
      g1.situation.board.piecesOnBar(Player.P1) === false
      g1.situation.board.piecesOnBar(Player.P2) === false
      g1.situation.board.piecesOnBoardCount === 30
      g1.situation.board.piecesCanLift(Player.P1) === false
      g1.situation.board.piecesCanLift(Player.P2) === false
      g1.situation.board.furthestFromEnd(Player.P1) === 24
      g1.situation.board.furthestFromEnd(Player.P2) === 24
      g1.situation.board.pipCount(Player.P1) === 167
      g1.situation.board.pipCount(Player.P2) === 167
      g1.situation.board.racePosition === false
      g1.situation.board.history.hasRolledDiceThisTurn === true
      g0.situation.board.pieces === g1.situation.board.pieces
    }
    val m1: Move = g1.situation.moves.values.flatten[Move].head
    val g2: Game = g1.apply(m1)
    "be valid after initial first move" in {
      g2.situation.canMove === true
      g2.situation.canDrop === false
      g2.situation.canLift === false
      g2.situation.canRollDice === false
      g2.situation.canEndTurn === false
      g2.situation.canCubeAction === false
      g2.situation.canOnlyDrop === false
      g2.situation.canOnlyLift === false
      g2.situation.canOnlyRollDice === false
      g2.situation.canOnlyEndTurn === false
      g2.situation.canOnlyCubeAction === false
      g2.situation.canCapture === false
      g2.situation.forcedAction.nonEmpty === false
      g2.situation.board.unusedDice.size === 1
      g2.situation.board.usedDice.size === 1
      g2.situation.board.piecesOnBar(Player.P1) === false
      g2.situation.board.piecesOnBar(Player.P2) === false
      g2.situation.board.piecesOnBoardCount === 30
      g2.situation.board.piecesCanLift(Player.P1) === false
      g2.situation.board.piecesCanLift(Player.P2) === false
      g2.situation.board.furthestFromEnd(Player.P1) === 24
      g2.situation.board.furthestFromEnd(Player.P2) === 24
      g2.situation.board.pipCount(g2.situation.player) < 167 === true
      g2.situation.board.pipCount(!g2.situation.player) === 167
      g2.situation.board.racePosition === false
      g2.situation.board.history.hasRolledDiceThisTurn === true
    }
    val m2: Move = g2.situation.moves.values.flatten[Move].head
    val g3: Game = g2.apply(m2)
    "be valid after initial second move" in {
      g3.situation.canMove === false
      g3.situation.canDrop === false
      g3.situation.canLift === false
      g3.situation.canRollDice === false
      g3.situation.canEndTurn === true
      g3.situation.canCubeAction === false
      g3.situation.canOnlyDrop === false
      g3.situation.canOnlyLift === false
      g3.situation.canOnlyRollDice === false
      g3.situation.canOnlyEndTurn === true
      g3.situation.canOnlyCubeAction === false
      g3.situation.canCapture === false
      g3.situation.forcedAction.nonEmpty === false
      g3.situation.board.unusedDice.size === 0
      g3.situation.board.usedDice.size === 2
      g3.situation.board.piecesOnBar(Player.P1) === false
      g3.situation.board.piecesOnBar(Player.P2) === false
      g3.situation.board.piecesOnBoardCount === 30
      g3.situation.board.piecesCanLift(Player.P1) === false
      g3.situation.board.piecesCanLift(Player.P2) === false
      g3.situation.board.racePosition === false
      g3.situation.board.history.hasRolledDiceThisTurn === true
      g3.situation.board.history.firstDiceRollHappened === false
    }
    val g4: Game = g3.endTurn() match {
      case Valid((g, _)) => g
      case _             => g3.pp("this should never print")
    }
    "be valid after first turn ends" in {
      g4.situation.canMove === false
      g4.situation.canDrop === false
      g4.situation.canLift === false
      g4.situation.canRollDice === true
      g4.situation.canEndTurn === false
      g4.situation.canCubeAction === true
      g4.situation.canOnlyDrop === false
      g4.situation.canOnlyLift === false
      g4.situation.canOnlyRollDice === false
      g4.situation.canOnlyEndTurn === false
      g4.situation.canOnlyCubeAction === false
      g4.situation.canCapture === false
      g4.situation.forcedAction.nonEmpty === false
      g4.situation.diceRolls.size === 36
      g4.situation.board.unusedDice.size === 0
      g4.situation.board.usedDice.size === 0
      g4.situation.board.piecesOnBar(Player.P1) === false
      g4.situation.board.piecesOnBar(Player.P2) === false
      g4.situation.board.piecesOnBoardCount === 30
      g4.situation.board.piecesCanLift(Player.P1) === false
      g4.situation.board.piecesCanLift(Player.P2) === false
      g4.situation.board.racePosition === false
      g4.situation.board.history.hasRolledDiceThisTurn === false
      g4.situation.board.history.firstDiceRollHappened === true
      g4.situation.player !== g3.situation.player
    }
    val ca1: CubeAction = g4.situation.cubeActions.head
    val g5: Game = g4.applyCubeAction(ca1)
    "be valid after offer double applied" in {
      g5.situation.canMove === false
      g5.situation.canDrop === false
      g5.situation.canLift === false
      g5.situation.canRollDice === false
      g5.situation.canEndTurn === false
      g5.situation.canCubeAction === true
      g5.situation.canOnlyDrop === false
      g5.situation.canOnlyLift === false
      g5.situation.canOnlyRollDice === false
      g5.situation.canOnlyEndTurn === false
      g5.situation.canOnlyCubeAction === true
      g5.situation.canCapture === false
      g5.situation.forcedAction.nonEmpty === false
      g5.situation.board.unusedDice.size === 0
      g5.situation.board.usedDice.size === 0
      g5.situation.board.cubeData.map(_.underOffer) === Some(true)
      g5.situation.board.cubeData.map(_.rejected) === Some(false)
      g5.situation.board.piecesOnBar(Player.P1) === false
      g5.situation.board.piecesOnBar(Player.P2) === false
      g5.situation.board.piecesOnBoardCount === 30
      g5.situation.board.piecesCanLift(Player.P1) === false
      g5.situation.board.piecesCanLift(Player.P2) === false
      g5.situation.board.racePosition === false
      g5.situation.board.history.hasRolledDiceThisTurn === false
      g5.situation.board.history.firstDiceRollHappened === true
      g5.situation.player !== g4.situation.player
    }
    val ca2: CubeAction = g5.situation.cubeActions.filter(_.interaction.name == "reject").head
    val g6: Game = g5.applyCubeAction(ca2)
    "be valid after cube offer rejected" in {
      g6.situation.end === true
      g6.situation.winner.nonEmpty === true
      g6.situation.status === Some(Status.CubeDropped)
      g6.situation.board.unusedDice.size === 0
      g6.situation.board.usedDice.size === 0
      g6.situation.board.cubeData.map(_.rejected) === Some(true)
      g6.situation.board.piecesOnBar(Player.P1) === false
      g6.situation.board.piecesOnBar(Player.P2) === false
      g6.situation.board.piecesOnBoardCount === 30
      g6.situation.board.piecesCanLift(Player.P1) === false
      g6.situation.board.piecesCanLift(Player.P2) === false
      g6.situation.board.racePosition === false
      g6.situation.board.history.hasRolledDiceThisTurn === false
      g6.situation.board.history.firstDiceRollHappened === true
    }

  }

}
