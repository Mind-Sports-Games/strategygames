package strategygames.fiveinarow

import strategygames.{ ClockBase, MoveMetrics, Player, VActionStrs }

import cats.data.Validated

import strategygames.fiveinarow.format.{ FEN, Uci }
import strategygames.fiveinarow.variant.Variant

case class Game(
    situation: Situation,
    actionStrs: VActionStrs = Vector(),
    clock: Option[ClockBase] = None,
    plies: Int = 0,
    turnCount: Int = 0,
    startedAtPly: Int = 0,
    startedAtTurn: Int = 0
) {

  def apply(drop: Drop): Game = applyAction(drop, drop.metrics)

  def apply(swap: Swap): Game = applyAction(swap, swap.metrics)

  def apply(swap2: Swap2): Game = applyAction(swap2, swap2.metrics)

  def apply(offer10: Offer10): Game = applyAction(offer10, offer10.metrics)

  def apply(ss: SelectSquares): Game = applyAction(ss, ss.metrics)

  private def applyAction(action: Action, metrics: MoveMetrics): Game = {
    val newSituation = action.situationAfter
    val switchPlayer = situation.player != newSituation.player

    copy(
      situation = newSituation,
      plies = plies + 1,
      turnCount = turnCount + (if (switchPlayer) 1 else 0),
      actionStrs = applyActionStr(action.toUci.uci),
      clock = applyClock(metrics, newSituation.status.isEmpty, switchPlayer)
    )
  }

  def apply(uci: Uci.Drop): Validated[String, (Game, Drop)] =
    situation.drop(uci.role, uci.pos).map(drop => apply(drop) -> drop)

  def apply(@annotation.nowarn uci: Uci.Swap): Validated[String, (Game, Swap)] =
    situation.swap().map(swap => apply(swap) -> swap)

  def apply(@annotation.nowarn uci: Uci.Swap2): Validated[String, (Game, Swap2)] =
    situation.swap2().map(swap2 => apply(swap2) -> swap2)

  def apply(@annotation.nowarn uci: Uci.Offer10): Validated[String, (Game, Offer10)] =
    situation.offer10().map(o => apply(o) -> o)

  def apply(uci: Uci.SelectSquares): Validated[String, (Game, SelectSquares)] =
    situation.selectSquares(uci.squares).map(ss => apply(ss) -> ss)

  def apply(uci: Uci): Validated[String, (Game, Action)] = (uci match {
    case u: Uci.Drop          => apply(u)
    case u: Uci.Swap          => apply(u)
    case u: Uci.Swap2         => apply(u)
    case u: Uci.Offer10       => apply(u)
    case u: Uci.SelectSquares => apply(u)
  }) map { case (g, a) => g -> a }

  private def applyClock(metrics: MoveMetrics, gameActive: Boolean, switchClock: Boolean) =
    clock.map { c =>
      val newC = c.step(metrics, gameActive, switchClock)
      if (turnCount - startedAtTurn == 1 && switchClock) newC.start else newC
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

  def drawOfferAllowed: Boolean =
    board.variant.canOfferDraw && board.pieces.size >= board.variant.drawOfferMinStones

  def halfMoveClock: Int = board.history.halfMoveClock

  def fullTurnCount: Int = 1 + turnCount / 2

  def withTurnsAndPlies(p: Int, t: Int) = copy(plies = p, turnCount = t)

}

object Game {

  def apply(variant: Variant): Game = new Game(Situation(variant))

  def apply(variantOption: Option[Variant], fen: Option[FEN]): Game = {
    val variant = variantOption.getOrElse(Variant.default)
    val g       = apply(variant)
    fen
      .flatMap(format.Forsyth.<<<@(variant, _))
      .fold(g) { parsed =>
        g.copy(
          situation = Situation(
            board = parsed.situation.board withVariant g.board.variant,
            player = parsed.situation.player
          ),
          plies = parsed.plies,
          turnCount = parsed.turnCount
        )
      }
  }

}
