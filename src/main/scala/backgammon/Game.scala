package strategygames.backgammon
import strategygames.{ ClockBase, MoveMetrics, Player, VActionStrs }

import cats.data.Validated

import scala.annotation.nowarn

import scala.util.Random

import strategygames.backgammon.format.{ FEN, Uci }
import strategygames.backgammon.variant.Variant

case class Game(
    situation: Situation,
    actionStrs: VActionStrs = Vector(),
    clock: Option[ClockBase] = None,
    plies: Int = 0,
    turnCount: Int = 0,
    startedAtPly: Int = 0,
    startedAtTurn: Int = 0
) {

  def apply(
      orig: Pos,
      dest: Pos,
      metrics: MoveMetrics = MoveMetrics()
  ): Validated[String, (Game, Move)] =
    situation.move(orig, dest).map(_ withMetrics metrics) map { move =>
      apply(move) -> move
    }

  def apply(move: Move): Game = {
    val newSituation = move.situationAfter
    val switchPlayer = situation.player != newSituation.player

    copy(
      situation = newSituation,
      plies = plies + 1,
      turnCount = turnCount + (if (switchPlayer) 1 else 0),
      actionStrs = applyActionStr(move.toUci.uci),
      clock = applyClock(move.metrics, newSituation.status.isEmpty, switchPlayer)
    )
  }

  def drop(
      role: Role,
      pos: Pos,
      metrics: MoveMetrics = MoveMetrics()
  ): Validated[String, (Game, Drop)] =
    situation.drop(role, pos).map(_ withMetrics metrics) map { drop =>
      applyDrop(drop) -> drop
    }

  def applyDrop(drop: Drop): Game = {
    val newSituation = drop.situationAfter
    val switchPlayer = situation.player != newSituation.player

    copy(
      situation = newSituation,
      plies = plies + 1,
      turnCount = turnCount + (if (switchPlayer) 1 else 0),
      actionStrs = applyActionStr(drop.toUci.uci),
      clock = applyClock(drop.metrics, newSituation.status.isEmpty, switchPlayer)
    )
  }

  def lift(
      pos: Pos,
      metrics: MoveMetrics = MoveMetrics()
  ): Validated[String, (Game, Lift)] =
    situation.lift(pos).map(_ withMetrics metrics) map { lift =>
      applyLift(lift) -> lift
    }

  def applyLift(lift: Lift): Game = {
    val newSituation = lift.situationAfter
    // if the lift triggers the end of the game we want to end the turn
    val switchPlayer = situation.player != newSituation.player || situation.end

    copy(
      situation = newSituation,
      plies = plies + 1,
      turnCount = turnCount + (if (switchPlayer) 1 else 0),
      actionStrs = applyActionStr(lift.toUci.uci),
      clock = applyClock(lift.metrics, newSituation.status.isEmpty, switchPlayer)
    )
  }

  def diceRoll(
      dice: List[Int],
      metrics: MoveMetrics = MoveMetrics()
  ): Validated[String, (Game, DiceRoll)] =
    situation.diceRoll(dice).map(_ withMetrics metrics) map { dr =>
      applyDiceRoll(dr) -> dr
    }

  def applyDiceRoll(dr: DiceRoll): Game = {
    if (
      !situation.board.history.didRollDiceLastTurn &&
      plies == 0 &&
      startedAtPly == 0 &&
      actionStrs.isEmpty &&
      dr.dice(0) > dr.dice(1)
    ) {
      // 50% of the time at the start of the game we need to flip the start player
      val gameBeforeDiceRoll = applyEndTurn(EndTurn(situation, situation.board))
      val diceRoll           = gameBeforeDiceRoll.situation.diceRoll(dr.dice).map(_ withMetrics dr.metrics) match {
        case Validated.Valid(diceRoll) => diceRoll
        case Validated.Invalid(e)      => sys.error(e)
      }
      gameBeforeDiceRoll.applyDiceRoll(diceRoll)
    } else {
      // apply a dice roll as normal
      val newSituation = dr.situationAfter
      val switchPlayer = situation.player != newSituation.player

      copy(
        situation = newSituation,
        plies = plies + 1,
        turnCount = turnCount + (if (switchPlayer) 1 else 0),
        actionStrs = applyActionStr(dr.toUci.uci),
        clock = applyClock(dr.metrics, newSituation.status.isEmpty, switchPlayer)
      )
    }
  }

  def endTurn(
      metrics: MoveMetrics = MoveMetrics()
  ): Validated[String, (Game, EndTurn)] =
    situation.endTurn.map(_ withMetrics metrics) map { endTurn =>
      applyEndTurn(endTurn) -> endTurn
    }

  def applyEndTurn(endTurn: EndTurn): Game = {
    val newSituation = endTurn.situationAfter
    val switchPlayer = situation.player != newSituation.player

    copy(
      situation = newSituation,
      plies = plies + 1,
      turnCount = turnCount + (if (switchPlayer) 1 else 0),
      actionStrs = applyActionStr(endTurn.toUci.uci),
      clock = applyClock(endTurn.metrics, newSituation.status.isEmpty, switchPlayer)
    )
  }

  def randomizeDiceRoll: Option[DiceRoll] =
    Random.shuffle(situation.board.variant.validDiceRolls(situation)).headOption

  def randomizeAndApplyDiceRoll(
      metrics: MoveMetrics = MoveMetrics()
  ): Validated[String, (Game, DiceRoll)] =
    randomizeDiceRoll.map(_ withMetrics metrics).map(dr => (applyDiceRoll(dr), dr)) match {
      case Some(gdr) => Validated.valid(gdr)
      case None      => Validated.invalid(s"$situation cannot randomize a dice roll")
    }

  def apply(uci: Uci.Move): Validated[String, (Game, Move)]               = apply(uci.orig, uci.dest)
  def apply(uci: Uci.Drop): Validated[String, (Game, Drop)]               = drop(uci.role, uci.pos)
  def apply(uci: Uci.Lift): Validated[String, (Game, Lift)]               = lift(uci.pos)
  def apply(uci: Uci.DiceRoll): Validated[String, (Game, DiceRoll)]       = diceRoll(uci.dice)
  def apply(@nowarn uci: Uci.EndTurn): Validated[String, (Game, EndTurn)] = endTurn()
  def apply(uci: Uci): Validated[String, (Game, Action)]                  = (uci match {
    case u: Uci.Move     => apply(u)
    case u: Uci.Drop     => apply(u)
    case u: Uci.Lift     => apply(u)
    case u: Uci.DiceRoll => apply(u)
    case u: Uci.EndTurn  => apply(u)
  }) map { case (g, a) => g -> a }

  private def applyClock(metrics: MoveMetrics, gameActive: Boolean, switchClock: Boolean) =
    clock.map { c =>
      {
        val newC = c.step(metrics, gameActive, switchClock)
        if (turnCount - startedAtTurn == 1 && switchClock) newC.start else newC
      }
    }

  private def applyActionStr(actionStr: String): VActionStrs =
    if (hasJustSwitchedTurns || actionStrs.size == 0)
      actionStrs :+ Vector(actionStr)
    else
      actionStrs.updated(actionStrs.size - 1, actionStrs(actionStrs.size - 1) :+ actionStr)

  def hasJustSwitchedTurns: Boolean =
    player == Player.fromTurnCount(actionStrs.size + startedAtTurn)

  def player = situation.player

  def board = situation.board

  def halfMoveClock: Int = board.history.halfMoveClock

  // Aka Fullmove number (in Forsyth-Edwards Notation):
  // The number of the completed turns by each player ('full move')
  // It starts at 1, and is incremented after P2's move (turn)
  def fullTurnCount: Int = 1 + turnCount / 2

  def withTurnsAndPlies(p: Int, t: Int) = copy(plies = p, turnCount = t)

}

object Game {

  def apply(variant: Variant): Game =
    new Game(Situation(Board init variant, P1))

  def apply(variantOption: Option[Variant], fen: Option[FEN]): Game = {
    val variant = variantOption | Variant.default
    val g       = apply(variant)
    fen
      .flatMap {
        format.Forsyth.<<<@(variant, _)
      }
      .fold(g) { parsed =>
        g.copy(
          situation = Situation(
            board = parsed.situation.board withVariant g.board.variant withPocketData {
              parsed.situation.board.pocketData orElse g.board.pocketData
            },
            player = parsed.situation.player
          ),
          plies = parsed.plies,
          turnCount = parsed.turnCount
        )
      }
  }

}
