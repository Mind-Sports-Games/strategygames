package strategygames
package format.pgn

import strategygames.format.pgn.Tags
import strategygames.chess.format.pgn.{ Reader => ChessReader }
import strategygames.draughts.format.pdn.{ Reader => DraughtsReader }
import strategygames.fairysf.format.pgn.{ Reader => FairySFReader }
import strategygames.samurai.format.pgn.{ Reader => SamuraiReader }
import strategygames.togyzkumalak.format.pgn.{ Reader => TogyzkumalakReader }
import strategygames.go.format.pgn.{ Reader => GoReader }

import cats.data.Validated

object Reader {

  sealed trait Result {
    def valid: Validated[String, Replay]
  }

  object Result {
    case class ChessComplete(replay: chess.Replay)                                  extends Result {
      def valid = Validated.valid(Replay.Chess(replay))
    }
    case class ChessIncomplete(replay: chess.Replay, failure: String)               extends Result {
      def valid = Validated.invalid(failure)
    }
    case class DraughtsComplete(replay: draughts.Replay)                            extends Result {
      def valid = Validated.valid(Replay.Draughts(replay))
    }
    case class DraughtsIncomplete(replay: draughts.Replay, failure: String)         extends Result {
      def valid = Validated.invalid(failure)
    }
    case class FairySFComplete(replay: fairysf.Replay)                              extends Result {
      def valid = Validated.valid(Replay.FairySF(replay))
    }
    case class FairySFIncomplete(replay: fairysf.Replay, failure: String)           extends Result {
      def valid = Validated.invalid(failure)
    }
    case class SamuraiComplete(replay: samurai.Replay)                              extends Result {
      def valid = Validated.valid(Replay.Samurai(replay))
    }
    case class SamuraiIncomplete(replay: samurai.Replay, failure: String)           extends Result {
      def valid = Validated.invalid(failure)
    }
    case class TogyzkumalakComplete(replay: togyzkumalak.Replay)                    extends Result {
      def valid = Validated.valid(Replay.Togyzkumalak(replay))
    }
    case class TogyzkumalakIncomplete(replay: togyzkumalak.Replay, failure: String) extends Result {
      def valid = Validated.invalid(failure)
    }
    case class GoComplete(replay: go.Replay)                                        extends Result {
      def valid = Validated.valid(Replay.Go(replay))
    }
    case class GoIncomplete(replay: go.Replay, failure: String)                     extends Result {
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

    def wrap(result: SamuraiReader.Result) = result match {
      case SamuraiReader.Result.Complete(replay)            => Result.SamuraiComplete(replay)
      case SamuraiReader.Result.Incomplete(replay, failure) => Result.SamuraiIncomplete(replay, failure)
    }

    def wrap(result: TogyzkumalakReader.Result) = result match {
      case TogyzkumalakReader.Result.Complete(replay)            => Result.TogyzkumalakComplete(replay)
      case TogyzkumalakReader.Result.Incomplete(replay, failure) =>
        Result.TogyzkumalakIncomplete(replay, failure)
    }

    def wrap(result: GoReader.Result) = result match {
      case GoReader.Result.Complete(replay)            => Result.GoComplete(replay)
      case GoReader.Result.Incomplete(replay, failure) => Result.GoIncomplete(replay, failure)
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
      case GameLogic.Chess()        => ChessReader.fullWithSans(pgn, op, tags).map(Result.wrap)
      case GameLogic.Draughts()     => DraughtsReader.fullWithSans(pgn, op, tags, iteratedCapts).map(Result.wrap)
      case GameLogic.FairySF()      => FairySFReader.fullWithSans(pgn, op, tags).map(Result.wrap)
      case GameLogic.Samurai()      => SamuraiReader.fullWithSans(pgn, op, tags).map(Result.wrap)
      case GameLogic.Togyzkumalak() => TogyzkumalakReader.fullWithSans(pgn, op, tags).map(Result.wrap)
      case GameLogic.Go()           => GoReader.fullWithSans(pgn, op, tags).map(Result.wrap)
    }

  // TODO Merge the following two functions by refactoring Sans and integrating to other libs
  def replayResultFromActionsUsingSan(
      lib: GameLogic,
      actions: Actions,
      op: Sans => Sans,
      tags: Tags,
      iteratedCapts: Boolean = false
  ): Validated[String, Result] =
    lib match {
      case GameLogic.Chess()        =>
        ChessReader.replayResultFromActionsUsingSan(actions, op, tags).map(Result.wrap)
      case GameLogic.Draughts()     =>
        DraughtsReader.replayResultFromActionsUsingSan(actions, op, tags, iteratedCapts).map(Result.wrap)
      case GameLogic.FairySF()      =>
        sys.error("Sans not implemented for fairysf")
      case GameLogic.Samurai()      =>
        sys.error("Sans not implemented for samurai")
      case GameLogic.Togyzkumalak() =>
        sys.error("Sans not implemented for togyzkumalak")
      case GameLogic.Go()           =>
        sys.error("Sans not implemented for go")
    }

  def replayResultFromActions(
      lib: GameLogic,
      actions: Actions,
      op: Actions => Actions,
      tags: Tags
  ): Validated[String, Result] =
    lib match {
      case GameLogic.Chess()        =>
        sys.error("replayResultFromActions not implemented for chess. Use replayResultFromActionsUsingSan")
      case GameLogic.Draughts()     =>
        sys.error("replayResultFromActions not implemented for draughts. Use replayResultFromActionsUsingSan")
      case GameLogic.FairySF()      =>
        FairySFReader.replayResultFromActions(actions, op, tags).map(Result.wrap)
      case GameLogic.Samurai()      =>
        SamuraiReader.replayResultFromActions(actions, op, tags).map(Result.wrap)
      case GameLogic.Togyzkumalak() =>
        TogyzkumalakReader.replayResultFromActions(actions, op, tags).map(Result.wrap)
      case GameLogic.Go()           =>
        GoReader.replayResultFromActions(actions, op, tags).map(Result.wrap)
    }

}
