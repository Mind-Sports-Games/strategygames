package strategygames.chess
import strategygames.{ ClockBase, MoveMetrics, Player, VActionStrs }

import cats.data.Validated

import strategygames.chess.format.FEN
import strategygames.chess.format.{ pgn, Uci }

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
      promotion: Option[PromotableRole] = None,
      metrics: MoveMetrics = MoveMetrics()
  ): Validated[String, (Game, Move)] =
    situation.move(orig, dest, promotion).map(_.normalizeCastle withMetrics metrics) map { move =>
      apply(move) -> move
    }

  def apply(move: Move): Game = {
    val newSituation = move.situationAfter
    val switchPlayer = situation.player != newSituation.player

    copy(
      situation = newSituation,
      plies = plies + 1,
      turnCount = turnCount + (if (switchPlayer) 1 else 0),
      actionStrs = applyActionStr(pgn.Dumper(situation, move, newSituation)),
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
    val newSituation = drop situationAfter
    val switchPlayer = situation.player != newSituation.player

    copy(
      situation = newSituation,
      plies = plies + 1,
      turnCount = turnCount + (if (switchPlayer) 1 else 0),
      actionStrs = applyActionStr(pgn.Dumper(drop, newSituation)),
      clock = applyClock(drop.metrics, newSituation.status.isEmpty, switchPlayer)
    )
  }

  private def applyClock(metrics: MoveMetrics, gameActive: Boolean, switchClock: Boolean) =
    clock.map { c =>
      {
        val newC = c.step(metrics, gameActive, switchClock)
        if (turnCount - startedAtTurn == 1 && switchClock) newC.start else newC
      }
    }

  def apply(uci: Uci.Move): Validated[String, (Game, Move)] = apply(uci.orig, uci.dest, uci.promotion)
  def apply(uci: Uci.Drop): Validated[String, (Game, Drop)] = drop(uci.role, uci.pos)
  def apply(uci: Uci): Validated[String, (Game, Action)]    = (uci match {
    case u: Uci.Move => apply(u)
    case u: Uci.Drop => apply(u)
  }) map { case (g, a) => g -> a }

  private def applyActionStr(actionStr: String): VActionStrs = {
    if (hasJustSwitchedTurns || actionStrs.size == 0)
      actionStrs :+ Vector(actionStr)
    else
      actionStrs.updated(actionStrs.size - 1, actionStrs(actionStrs.size - 1) :+ actionStr)
  }

  def hasJustSwitchedTurns: Boolean =
    player == Player.fromTurnCount(actionStrs.size + startedAtTurn)

  def player = situation.player

  def board = situation.board

  def isStandardInit = board.pieces == strategygames.chess.variant.Standard.pieces

  def halfMoveClock: Int = board.history.halfMoveClock

  // Aka Fullmove number (in Forsyth-Edwards Notation):
  // The number of the completed turns by each player ('full move')
  // It starts at 1, and is incremented after P2's move (turn)
  def fullTurnCount: Int = 1 + turnCount / 2

  // doesnt seem to be used anywhere
  // def moveString = s"$fullTurnCount${player.fold(".", "...")}"

  def withBoard(b: Board) = copy(situation = situation.copy(board = b))

  def updateBoard(f: Board => Board) = withBoard(f(board))

  def withPlayer(c: Player) = copy(situation = situation.copy(player = c))

  def withTurnsAndPlies(p: Int, t: Int) = copy(plies = p, turnCount = t)
}

object Game {
  def apply(variant: strategygames.chess.variant.Variant): Game =
    new Game(Situation(Board init variant, P1))

  def apply(board: Board): Game = apply(board, P1)

  def apply(board: Board, player: Player): Game = new Game(Situation(board, player))

  def apply(variantOption: Option[strategygames.chess.variant.Variant], fen: Option[FEN]): Game = {
    val variant = variantOption | strategygames.chess.variant.Standard
    val g       = apply(variant)
    fen
      .flatMap {
        format.Forsyth.<<<@(variant, _)
      }
      .fold(g) { parsed =>
        g.copy(
          situation = Situation(
            board = parsed.situation.board withVariant g.board.variant withCrazyData {
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
