package strategygames
package format.pgn

import strategygames.format.pgn.Tags
import strategygames.chess.format.pgn.{ Reader => ChessReader }
import strategygames.draughts.format.pdn.{ Reader => DraughtsReader }

import cats.data.Validated

object Reader {

  sealed trait Result {
    def valid: Validated[String, Replay]
  }

  object Result {
    case class ChessComplete(replay: chess.Replay) extends Result {
      def valid = Validated.valid(Replay.Chess(replay))
    }
    case class ChessIncomplete(replay: chess.Replay, failure: String) extends Result {
      def valid = Validated.invalid(failure)
    }
    case class DraughtsComplete(replay: draughts.Replay) extends Result {
      def valid = Validated.valid(Replay.Draughts(replay))
    }
    case class DraughtsIncomplete(replay: draughts.Replay, failure: String) extends Result {
      def valid = Validated.invalid(failure)
    }

    def wrap(result: ChessReader.Result) = result match {
      case ChessReader.Result.Complete(replay)            => Result.ChessComplete(replay)
      case ChessReader.Result.Incomplete(replay, failure) => Result.ChessIncomplete(replay, failure)
    }

    def wrap(result: DraughtsReader.Result) = result match {
      case DraughtsReader.Result.Complete(replay)            => Result.DraughtsComplete(replay)
      case DraughtsReader.Result.Incomplete(replay, failure) => Result.DraughtsIncomplete(replay, failure)
    }
  }

  def full(lib: GameLib, pgn: String, tags: Tags = Tags.empty): Validated[String, Result] =
    lib match {
      case GameLib.Chess()    => ChessReader.full(pgn, tags).map(Result.wrap)
      case GameLib.Draughts() => DraughtsReader.full(pgn, tags).map(Result.wrap)
    }

  def moves(
      lib: GameLib,
      moveStrs: Iterable[String],
      tags: Tags,
      iteratedCapts: Boolean = false
  ): Validated[String, Result] =
    lib match {
      case GameLib.Chess()    => ChessReader.moves(moveStrs, tags).map(Result.wrap)
      case GameLib.Draughts() => DraughtsReader.moves(moveStrs, tags, iteratedCapts).map(Result.wrap)
    }

  def fullWithSans(
      lib: GameLib,
      pgn: String,
      op: Sans => Sans,
      tags: Tags = Tags.empty,
      iteratedCapts: Boolean = false
  ): Validated[String, Result] =
    lib match {
      case GameLib.Chess()    => ChessReader.fullWithSans(pgn, op, tags).map(Result.wrap)
      case GameLib.Draughts() => DraughtsReader.fullWithSans(pgn, op, tags, iteratedCapts).map(Result.wrap)
    }

  /* TODO: Maybe port this? I don't think it's used.
  def fullWithSans(lib: GameLib, parsed: ParsedPdn, op: Sans => Sans): Result =
    lib match {
      case GameLib.Chess()    => Result.wrap(ChessReader.fullWithSans(parsed, op))
      case GameLib.Draughts() => Result.wrap(DraughtsReader.fullWithSans(parsed, op))
    }
  */

  def movesWithSans(
      lib: GameLib,
      moveStrs: Iterable[String],
      op: Sans => Sans,
      tags: Tags,
      iteratedCapts: Boolean = false
  ): Validated[String, Result] =
    lib match {
      case GameLib.Chess() => ChessReader.movesWithSans(moveStrs, op, tags).map(Result.wrap)
      case GameLib.Draughts() =>
        DraughtsReader.movesWithSans(moveStrs, op, tags, iteratedCapts).map(Result.wrap)
    }

  // remove invisible byte order mark
  def cleanUserInput(str: String) = str.replace(s"\ufeff", "")

}
