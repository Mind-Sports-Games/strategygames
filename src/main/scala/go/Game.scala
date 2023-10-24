package strategygames.go
import strategygames.{ Clock, MoveMetrics, VActions }

import cats.data.Validated

import strategygames.go.format.{ pgn, FEN, Uci }

case class Game(
    situation: Situation,
    actions: Vector[Vector[String]] = Vector(),
    clock: Option[Clock] = None,
    plies: Int = 0,
    turnCount: Int = 0,
    startedAtPlies: Int = 0,
    startedAtTurn: Int = 0
) {
  def apply(drop: Drop): Game = {
    val newSituation = drop.situationAfter
    val switchPlayer = situation.player != newSituation.player

    copy(
      situation = newSituation,
      plies = plies + 1,
      turnCount = turnCount + (if (switchPlayer) 1 else 0),
      actions = applyAction(drop.toUci.uci, switchPlayer),
      clock = applyClock(drop.metrics, newSituation.status.isEmpty, switchPlayer)
    )
  }

  def apply(pass: Pass): Game = {
    val newSituation = pass.situationAfter
    val switchPlayer = situation.player != newSituation.player

    copy(
      situation = newSituation,
      plies = plies + 1,
      turnCount = turnCount + (if (switchPlayer) 1 else 0),
      actions = applyAction(pass.toUci.uci, switchPlayer),
      clock = applyClock(pass.metrics, newSituation.status.isEmpty, switchPlayer)
    )
  }

  def apply(ss: SelectSquares): Game = {
    val newSituation = ss.situationAfter
    val switchPlayer = situation.player != newSituation.player

    copy(
      situation = newSituation,
      plies = plies + 1,
      turnCount = turnCount + (if (switchPlayer) 1 else 0),
      actions = applyAction(ss.toUci.uci, switchPlayer),
      clock = applyClock(ss.metrics, newSituation.status.isEmpty, switchPlayer)
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

  def applyDrop(drop: Drop): Game = apply(drop)

  def pass(
      metrics: MoveMetrics = MoveMetrics()
  ): Validated[String, (Game, Pass)] =
    situation.pass().map(_ withMetrics metrics) map { pass =>
      applyPass(pass) -> pass
    }

  def applyPass(pass: Pass): Game = apply(pass)

  def selectSquares(
      squares: List[Pos],
      metrics: MoveMetrics = MoveMetrics()
  ): Validated[String, (Game, SelectSquares)] =
    situation.selectSquares(squares).map(_ withMetrics metrics) map { ss =>
      applySelectSquares(ss) -> ss
    }

  def applySelectSquares(ss: SelectSquares): Game = apply(ss)

  def apply(uci: Uci.Drop): Validated[String, (Game, Drop)]                   = drop(uci.role, uci.pos)
  def apply(uci: Uci.Pass): Validated[String, (Game, Pass)]                   = pass()
  def apply(uci: Uci.SelectSquares): Validated[String, (Game, SelectSquares)] = selectSquares(uci.squares)
  def apply(uci: Uci): Validated[String, (Game, Action)]                      = (uci match {
    case u: Uci.Drop          => apply(u)
    case u: Uci.Pass          => apply(u)
    case u: Uci.SelectSquares => apply(u)
  }) map { case (g, a) => g -> a }

  private def applyAction(action: String, switchPlayer: Boolean): VActions = {
    if (switchPlayer || actions.size == 0)
      actions :+ Vector(action)
    else
      actions.updated(actions.size - 1, actions(actions.size - 1) :+ action)
  }

  private def applyClock(
      metrics: MoveMetrics,
      gameActive: Boolean,
      switchClock: Boolean,
      pauseClock: Boolean = false
  ) =
    clock.map { c =>
      {
        val newC = c.step(metrics, gameActive, switchClock)
        if (pauseClock) newC.pause
        else if (actions.size == 1 && switchClock) newC.start
        else newC
      }
    }

  def player = situation.player

  def board = situation.board

  def halfMoveClock: Int = board.history.halfMoveClock

  // Aka Fullmove number (in Forsyth-Edwards Notation):
  // The number of the completed turns by each player ('full move')
  // It starts at 1, and is incremented after P2's move (turn)
  def fullTurnCount: Int = 1 + turnCount / 2

  // def currentTurnCount: Int = turnCount + (if (actions.size > 0) 1 else 0)

  def withTurns(p: Int, t: Int) = copy(plies = p, turnCount = t)
}

object Game {
  def apply(variant: strategygames.go.variant.Variant): Game =
    new Game(Situation(Board init variant, P1))

  def apply(variantOption: Option[strategygames.go.variant.Variant], fen: Option[FEN]): Game = {
    val variant = variantOption | strategygames.go.variant.Variant.default
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
