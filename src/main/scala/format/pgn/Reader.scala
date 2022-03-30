package strategygames
package format.pgn

import strategygames.format.pgn.Tags
import strategygames.chess.format.pgn.{ Reader => ChessReader }
import strategygames.draughts.format.pdn.{ Reader => DraughtsReader }
import strategygames.fairysf.format.pgn.{ Reader => FairySFReader }
import strategygames.mancala.format.pgn.{ Reader => MancalaReader }

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
    case class FairySFComplete(replay: fairysf.Replay) extends Result {
      def valid = Validated.valid(Replay.FairySF(replay))
    }
    case class FairySFIncomplete(replay: fairysf.Replay, failure: String) extends Result {
      def valid = Validated.invalid(failure)
    }
    case class MancalaComplete(replay: mancala.Replay) extends Result {
      def valid = Validated.valid(Replay.Mancala(replay))
    }
    case class MancalaIncomplete(replay: mancala.Replay, failure: String) extends Result {
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

    def wrap(result: FairySFReader.Result) = result match {
      case FairySFReader.Result.Complete(replay)            => Result.FairySFComplete(replay)
      case FairySFReader.Result.Incomplete(replay, failure) => Result.FairySFIncomplete(replay, failure)
    }

    def wrap(result: MancalaReader.Result) = result match {
      case MancalaReader.Result.Complete(replay)            => Result.MancalaComplete(replay)
      case MancalaReader.Result.Incomplete(replay, failure) => Result.MancalaIncomplete(replay, failure)
    }
  }

  def fullWithSans(
      lib: GameLogic,
      pgn: String,
      op: Sans => Sans,
      tags: Tags = Tags.empty,
      iteratedCapts: Boolean = false
  ): Validated[String, Result] =
    lib match {
      case GameLogic.Chess()    => ChessReader.fullWithSans(pgn, op, tags).map(Result.wrap)
      case GameLogic.Draughts() => DraughtsReader.fullWithSans(pgn, op, tags, iteratedCapts).map(Result.wrap)
      case GameLogic.FairySF()  => FairySFReader.fullWithSans(pgn, op, tags).map(Result.wrap)
      case GameLogic.Mancala()    => MancalaReader.fullWithSans(pgn, op, tags).map(Result.wrap)
    }

  def movesWithSans(
      lib: GameLogic,
      moveStrs: Iterable[String],
      op: Sans => Sans,
      tags: Tags,
      iteratedCapts: Boolean = false
  ): Validated[String, Result] =
    lib match {
      case GameLogic.Chess() =>
        ChessReader.movesWithSans(moveStrs, op, tags).map(Result.wrap)
      case GameLogic.Draughts() =>
        DraughtsReader.movesWithSans(moveStrs, op, tags, iteratedCapts).map(Result.wrap)
      case GameLogic.FairySF() =>
        //FairySFReader.movesWithSans(moveStrs, op, tags).map(Result.wrap)
        sys.error("Sans not implemented for fairysf")
      case GameLogic.Mancala() =>
        sys.error("Sans not implemented for mancala")
    }

  def movesWithPgns(
      lib: GameLogic,
      moveStrs: Iterable[String],
      op: Iterable[String] => Iterable[String],
      tags: Tags
  ): Validated[String, Result] =
    lib match {
      case GameLogic.Chess()    => sys.error("movesWithPgns not implemented for chess")
      case GameLogic.Draughts() => sys.error("movesWithPgns not implemented for draughts")
      case GameLogic.FairySF() =>
        FairySFReader.movesWithPgns(moveStrs, op, tags).map(Result.wrap)
      case GameLogic.Mancala() =>
        MancalaReader.movesWithPgns(moveStrs, op, tags).map(Result.wrap)
    }

}
