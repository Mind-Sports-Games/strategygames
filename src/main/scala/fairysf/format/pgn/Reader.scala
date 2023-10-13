package strategygames.fairysf
package format.pgn
import strategygames.{
  Actions,
  ByoyomiClock,
  FischerClock,
  GameFamily,
  Move => StratMove,
  Situation => StratSituation
}

import strategygames.format.pgn.{ ParsedPgn, Sans, Tags }

import strategygames.fairysf.format.Uci

import cats.data.Validated

object Reader {

  sealed trait Result {
    def valid: Validated[String, Replay]
  }

  object Result {
    case class Complete(replay: Replay)                    extends Result {
      def valid = Validated.valid(replay)
    }
    case class Incomplete(replay: Replay, failure: String) extends Result {
      def valid = Validated.invalid(failure)
    }
  }

  def full(pgn: String, tags: Tags = Tags.empty): Validated[String, Result] =
    fullWithSans(pgn, identity, tags)

  // Because fairysf deals exclusively with UCI moves, we need this version
  // for parsing replaces from uci move
  def replayResult(gf: GameFamily, uciStrs: Iterable[String], tags: Tags): Validated[String, Result] = {
    val uci = uciStrs.flatMap(Uci(gf, _))
    if (uci.size < uciStrs.size) {
      Validated.invalid("Invalid UCI moves")
    } else {
      Validated.valid(
        makeReplayFromUCI(
          makeGame(tags),
          uci.toList
        )
      )
    }
  }

  def fullWithSans(pgn: String, op: Sans => Sans, tags: Tags = Tags.empty): Validated[String, Result] =
    // seemingly this isn't used
    Parser.full(cleanUserInput(pgn)) map { parsed =>
      makeReplay(makeGame(parsed.tags ++ tags), op(parsed.sans))
    }

  def fullWithSans(parsed: ParsedPgn, op: Sans => Sans): Result =
    makeReplay(makeGame(parsed.tags), op(parsed.sans))

  def replayResultFromActions(
      actions: Actions,
      op: Actions => Actions,
      tags: Tags
  ): Validated[String, Result] =
    Validated.valid(makeReplayWithActions(makeGame(tags), op(actions)))

  // remove invisible byte order mark
  def cleanUserInput(str: String) = str.replace(s"\ufeff", "")

  private def makeReplay(game: Game, sans: Sans): Result =
    sans.value.foldLeft[Result](Result.Complete(Replay(game))) {
      case (Result.Complete(replay), san) =>
        san(StratSituation.wrap(replay.state.situation)).fold(
          err => Result.Incomplete(replay, err),
          ply => Result.Complete(replay addPly StratMove.toFairySF(ply))
        )
      case (r: Result.Incomplete, _)      => r
    }

  private def makeReplayFromUCI(game: Game, ucis: List[Uci]): Result =
    ucis.foldLeft[Result](Result.Complete(Replay(game))) {
      case (Result.Complete(replay), uci) =>
        uci(replay.state.situation).fold(
          err => Result.Incomplete(replay, err),
          ply => Result.Complete(replay addPly ply)
        )
      case (r: Result.Incomplete, _)      => r
    }

  private def makeReplayWithActions(game: Game, actions: Actions): Result = {
    var lastMove: Option[String] = None
    var lastDest: Option[String] = None
    // This doesnt support multiaction properly, but it correctly handles a game like Amazons
    // by implementing specific fairy multiaction logic using switchPlayerAfterMove
    Parser.pliesToFairyUciMoves(actions.flatten).foldLeft[Result](Result.Complete(Replay(game))) {
      case (Result.Complete(replay), m) =>
        m match {
          case Uci.Move.moveR(orig, dest, promotion) => {
            lastMove = Some(m)
            lastDest = Some(dest)
            (Pos.fromKey(orig), Pos.fromKey(dest)) match {
              case (Some(orig), Some(dest)) =>
                Result.Complete(
                  replay.addPly(
                    Left {
                      if (game.situation.board.variant.switchPlayerAfterMove)
                        Replay.replayMove(
                          replay.state,
                          orig,
                          dest,
                          promotion,
                          replay.state.board.apiPosition.makeMoves(List(m)),
                          replay.state.board.uciMoves :+ m
                        )
                      else
                        Replay.replayMoveWithoutAPI(
                          replay.state,
                          replay.state.board.pieces(orig),
                          orig,
                          dest,
                          promotion
                        )
                    }
                  )
                )
              case _                        => Result.Incomplete(replay, s"Error making replay with move: ${m}")
            }
          }
          case Uci.Drop.dropR(role, dest)            =>
            (Role.allByForsyth(replay.state.board.variant.gameFamily).get(role(0)), Pos.fromKey(dest)) match {
              case (Some(role), Some(dest)) =>
                Result.Complete(
                  replay.addPly(
                    Right {
                      val uci =
                        if (game.situation.board.variant.switchPlayerAfterMove) m
                        else s"${lastMove.getOrElse("")},${lastDest.getOrElse("")}${dest.key}"
                      Replay.replayDrop(
                        replay.state,
                        role,
                        dest,
                        replay.state.board.apiPosition.makeMoves(List(uci)),
                        replay.state.board.uciMoves :+ uci
                      )
                    }
                  )
                )
              case _                        => Result.Incomplete(replay, s"Error making replay with drop: ${m}")
            }
          case _                                     => Result.Incomplete(replay, s"Error making replay with uci move: ${m}")
        }
      case (r: Result.Incomplete, _)    => r
    }
  }

  private def makeGame(tags: Tags) = {
    val g = Game(
      variantOption = tags.fairysfVariant,
      fen = tags.fairysfFen
    )
    g.copy(
      startedAtTurn = g.currentTurnCount,
      startPlayer = g.situation.player,
      clock = tags.clockConfig.flatMap {
        case fc: FischerClock.Config => Some(FischerClock.apply(fc))
        case bc: ByoyomiClock.Config => Some(ByoyomiClock.apply(bc))
        case _                       => None
      }
    )
  }
}
