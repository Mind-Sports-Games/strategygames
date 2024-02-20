package strategygames.backgammon
package format.pgn
import strategygames.{ Action => StratAction, ActionStrs, ByoyomiClock, Clock, Situation => StratSituation }

import strategygames.format.pgn.{ ParsedPgn, Sans, Tags }

import strategygames.backgammon.format.Uci

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

  def fullWithSans(pgn: String, op: Sans => Sans, tags: Tags = Tags.empty): Validated[String, Result] =
    Parser.full(cleanUserInput(pgn)) map { parsed =>
      makeReplay(makeGame(parsed.tags ++ tags), op(parsed.sans))
    }

  def fullWithSans(parsed: ParsedPgn, op: Sans => Sans): Result =
    makeReplay(makeGame(parsed.tags), op(parsed.sans))

  def replayResultFromActionStrs(
      actionStrs: ActionStrs,
      op: ActionStrs => ActionStrs,
      tags: Tags
  ): Validated[String, Result] =
    Validated.valid(makeReplayWithActionStrs(makeGame(tags), op(actionStrs)))

  // remove invisible byte order mark
  def cleanUserInput(str: String) = str.replace(s"\ufeff", "")

  private def makeReplay(game: Game, sans: Sans): Result =
    sans.value.foldLeft[Result](Result.Complete(Replay(game))) {
      case (Result.Complete(replay), san) =>
        san(StratSituation.wrap(replay.state.situation)).fold(
          err => Result.Incomplete(replay, err),
          action => Result.Complete(replay addAction StratAction.toBackgammon(action))
        )
      case (r: Result.Incomplete, _)      => r
    }

  private def makeReplayWithActionStrs(game: Game, actionStrs: ActionStrs): Result =
    actionStrs.flatten.foldLeft[Result](Result.Complete(Replay(game))) {
      case (Result.Complete(replay), actionStr) =>
        actionStr match {
          case Uci.Move.moveR(orig, dest)   => {
            (Pos.fromKey(orig), Pos.fromKey(dest)) match {
              case (Some(orig), Some(dest)) =>
                Result.Complete(
                  replay.addAction(
                    Replay.replayMove(replay.state, orig, dest)
                  )
                )
              case _                        =>
                Result.Incomplete(replay, s"Error making replay with move: ${actionStr}")
            }
          }
          case Uci.Drop.dropR(role, dest)   => {
            (Role.allByForsyth(replay.state.board.variant.gameFamily).get(role(0)), Pos.fromKey(dest)) match {
              case (Some(role), Some(dest)) =>
                Result.Complete(
                  replay.addAction(
                    Replay.replayDrop(replay.state, role, dest)
                  )
                )
              case _                        =>
                Result.Incomplete(replay, s"Error making replay with drop: ${actionStr}")
            }
          }
          case Uci.Lift.liftR(orig)         => {
            (Pos.fromKey(orig)) match {
              case (Some(orig)) =>
                Result.Complete(
                  replay.addAction(
                    Replay.replayLift(replay.state, orig)
                  )
                )
              case _            =>
                Result.Incomplete(replay, s"Error making replay with lift: ${actionStr}")
            }
          }
          case Uci.DiceRoll.diceRollR(dice) => {
            (Uci.DiceRoll.fromStrings(dice).dice) match {
              case dice if dice.size == 2 =>
                Result.Complete(
                  replay.addAction(
                    Replay.replayDiceRoll(replay.state, dice)
                  )
                )
              case _                      =>
                Result.Incomplete(replay, s"Error making replay with dice: ${actionStr}")
            }
          }
          case Uci.EndTurn.endTurnR()       => {
            Result.Complete(
              replay.addAction(
                Replay.replayEndTurn(replay.state)
              )
            )
          }
          case _                            =>
            Result.Incomplete(replay, s"Error making replay with uci action: ${actionStr}")
        }
      case (r: Result.Incomplete, _)            => r
    }

  private def makeGame(tags: Tags) = {
    val g = Game(
      variantOption = tags.backgammonVariant,
      fen = tags.backgammonFen
    )
    g.copy(
      startedAtPly = g.plies,
      startedAtTurn = g.turnCount,
      clock = tags.clockConfig.flatMap {
        case fc: Clock.Config        => Some(Clock.apply(fc))
        case bc: ByoyomiClock.Config => Some(ByoyomiClock.apply(bc))
        case _                       => None
      }
    )
  }
}
