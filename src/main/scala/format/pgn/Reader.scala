package strategygames
package format.pgn

import strategygames.format.pgn.Tags
import strategygames.chess.format.pgn.{ Reader => ChessReader }
import strategygames.draughts.format.pdn.{ Reader => DraughtsReader }
import strategygames.fairysf.format.pgn.{ Reader => FairySFReader }
import strategygames.samurai.format.pgn.{ Reader => SamuraiReader }
import strategygames.togyzkumalak.format.pgn.{ Reader => TogyzkumalakReader }
import strategygames.go.format.pgn.{ Reader => GoReader }
import strategygames.backgammon.format.pgn.{ Reader => BackgammonReader }
import strategygames.abalone.format.pgn.{ Reader => AbaloneReader }

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
    case class BackgammonComplete(replay: backgammon.Replay)                        extends Result {
      def valid = Validated.valid(Replay.Backgammon(replay))
    }
    case class BackgammonIncomplete(replay: backgammon.Replay, failure: String)     extends Result {
      def valid = Validated.invalid(failure)
    }
    case class AbaloneComplete(replay: abalone.Replay)                              extends Result {
      def valid = Validated.valid(Replay.Abalone(replay))
    }
    case class AbaloneIncomplete(replay: abalone.Replay, failure: String)           extends Result {
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

    def wrap(result: BackgammonReader.Result) = result match {
      case BackgammonReader.Result.Complete(replay)            => Result.BackgammonComplete(replay)
      case BackgammonReader.Result.Incomplete(replay, failure) =>
        Result.BackgammonIncomplete(replay, failure)
    }

    def wrap(result: AbaloneReader.Result) = result match {
      case AbaloneReader.Result.Complete(replay)            => Result.AbaloneComplete(replay)
      case AbaloneReader.Result.Incomplete(replay, failure) => Result.AbaloneIncomplete(replay, failure)
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
      case GameLogic.Backgammon()   => BackgammonReader.fullWithSans(pgn, op, tags).map(Result.wrap)
      case GameLogic.Abalone()      => AbaloneReader.fullWithSans(pgn, op, tags).map(Result.wrap)
    }

  // TODO Merge the following two functions by refactoring Sans and integrating to other libs
  def replayResultFromActionStrsUsingSan(
      lib: GameLogic,
      actionStrs: ActionStrs,
      op: Sans => Sans,
      tags: Tags,
      iteratedCapts: Boolean = false
  ): Validated[String, Result] =
    lib match {
      case GameLogic.Chess()        =>
        ChessReader.replayResultFromActionStrsUsingSan(actionStrs, op, tags).map(Result.wrap)
      case GameLogic.Draughts()     =>
        DraughtsReader
          .replayResultFromActionStrsUsingSan(actionStrs, op, tags, iteratedCapts)
          .map(Result.wrap)
      case GameLogic.FairySF()      =>
        sys.error("Sans not implemented for fairysf")
      case GameLogic.Samurai()      =>
        sys.error("Sans not implemented for samurai")
      case GameLogic.Togyzkumalak() =>
        sys.error("Sans not implemented for togyzkumalak")
      case GameLogic.Go()           =>
        sys.error("Sans not implemented for go")
      case GameLogic.Backgammon()   =>
        sys.error("Sans not implemented for backgammon")
      case GameLogic.Abalone()      =>
        sys.error("Sans not implemented for abalone")
    }

  def replayResultFromActionStrs(
      lib: GameLogic,
      actionStrs: ActionStrs,
      op: ActionStrs => ActionStrs,
      tags: Tags
  ): Validated[String, Result] =
    lib match {
      case GameLogic.Chess()        =>
        sys.error(
          "replayResultFromActionStrs not implemented for chess. Use replayResultFromActionStrsUsingSan"
        )
      case GameLogic.Draughts()     =>
        sys.error(
          "replayResultFromActionStrs not implemented for draughts. Use replayResultFromActionStrsUsingSan"
        )
      case GameLogic.FairySF()      =>
        FairySFReader.replayResultFromActionStrs(actionStrs, op, tags).map(Result.wrap)
      case GameLogic.Samurai()      =>
        SamuraiReader.replayResultFromActionStrs(actionStrs, op, tags).map(Result.wrap)
      case GameLogic.Togyzkumalak() =>
        TogyzkumalakReader.replayResultFromActionStrs(actionStrs, op, tags).map(Result.wrap)
      case GameLogic.Go()           =>
        GoReader.replayResultFromActionStrs(actionStrs, op, tags).map(Result.wrap)
      case GameLogic.Backgammon()   =>
        BackgammonReader.replayResultFromActionStrs(actionStrs, op, tags).map(Result.wrap)
      case GameLogic.Abalone()      =>
        AbaloneReader.replayResultFromActionStrs(actionStrs, op, tags).map(Result.wrap)
    }

}
