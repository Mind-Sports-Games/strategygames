package strategygames.mancala
package format.pgn
import strategygames.{ FischerClock, Drop => StratDrop, Move => StratMove, Situation => StratSituation }

import strategygames.format.pgn.{ ParsedPgn, Sans, Tags }

import strategygames.mancala.format.Uci

import cats.data.Validated
import cats.implicits._

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

  def moves(moveStrs: Iterable[String], tags: Tags): Validated[String, Result] =
    movesWithSans(moveStrs, identity, tags)

  def fullWithSans(pgn: String, op: Sans => Sans, tags: Tags = Tags.empty): Validated[String, Result] =
    Parser.full(cleanUserInput(pgn)) map { parsed =>
      makeReplay(makeGame(parsed.tags ++ tags), op(parsed.sans))
    }

  def fullWithSans(parsed: ParsedPgn, op: Sans => Sans): Result =
    makeReplay(makeGame(parsed.tags), op(parsed.sans))

  def movesWithSans(moveStrs: Iterable[String], op: Sans => Sans, tags: Tags): Validated[String, Result] =
    Parser.moves(moveStrs, tags.mancalaVariant | variant.Variant.default) map { moves =>
      makeReplay(makeGame(tags), op(moves))
    }

  def movesWithPgns(
      moveStrs: Iterable[String],
      op: Iterable[String] => Iterable[String],
      tags: Tags
  ): Validated[String, Result] =
    Validated.valid(makeReplayWithPgn(makeGame(tags), op(moveStrs)))

  // remove invisible byte order mark
  def cleanUserInput(str: String) = str.replace(s"\ufeff", "")

  private def makeReplay(game: Game, sans: Sans): Result =
    sans.value.foldLeft[Result](Result.Complete(Replay(game))) {
      case (Result.Complete(replay), san) =>
        san(StratSituation.wrap(replay.state.situation)).fold(
          err => Result.Incomplete(replay, err),
          move => Result.Complete(replay addMove StratMove.toMancala(move))
        )
      case (r: Result.Incomplete, _)      => r
    }

  private def makeReplayWithPgn(game: Game, moves: Iterable[String]): Result =
    Parser.pgnMovesToUciMoves(moves).foldLeft[Result](Result.Complete(Replay(game))) {
      case (Result.Complete(replay), m) =>
        m match {
          case Uci.Move.moveR(orig, dest, _) => {
            (Pos.fromKey(orig), Pos.fromKey(dest)) match {
              case (Some(orig), Some(dest)) =>
                Result.Complete(
                  replay.addMove(
                    Replay.replayMove(
                      replay.state,
                      orig,
                      dest,
                      replay.state.board.apiPosition
                        .makeMoves(List(m).map(uciMove => Api.uciToMove(uciMove))),
                      replay.state.board.uciMoves :+ m
                    )
                  )
                )
              case _                        => Result.Incomplete(replay, s"Error making replay with move: ${m}")
            }
          }
          case _                             => Result.Incomplete(replay, s"Error making replay with uci move: ${m}")
        }
      case (r: Result.Incomplete, _)    => r
    }

  private def makeGame(tags: Tags) = {
    val g = Game(
      variantOption = tags.mancalaVariant,
      fen = tags.mancalaFen
    )
    g.copy(
      startedAtTurn = g.turns,
      // TODO: byoyomi, we should also read byoyomi here.
      clock = tags.clockConfig.flatMap({
        case c: FischerClock.Config => Some(FischerClock.apply(c))
        case _ => None
      })
    )
  }
}
