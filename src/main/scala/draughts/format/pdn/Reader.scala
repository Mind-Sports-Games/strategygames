package draughts
package format.pdn
import strategygames.{ Clock, GameLib }

import cats.data.Validated
import cats.implicits._

object Reader {

  sealed trait Result {
    def valid: Validated[String, Replay]
  }

  object Result {
    case class Complete(replay: Replay) extends Result {
      def valid = Validated.valid(replay)
    }
    case class Incomplete(replay: Replay, failure: String) extends Result {
      def valid = Validated.invalid(failure)
    }
  }

  def full(pdn: String, tags: Tags = Tags.empty): Validated[String, Result] =
    fullWithSans(pdn, identity, tags, true)

  def moves(
      moveStrs: Traversable[String],
      tags: Tags,
      iteratedCapts: Boolean = false
  ): Validated[String, Result] =
    movesWithSans(moveStrs, identity, tags, iteratedCapts)

  def fullWithSans(
      pdn: String,
      op: Sans => Sans,
      tags: Tags = Tags.empty,
      iteratedCapts: Boolean = false
  ): Validated[String, Result] =
    Parser.full(cleanUserInput(pdn)) map { parsed =>
      makeReplay(makeGame(parsed.tags ++ tags), op(parsed.sans), iteratedCapts)
    }

  def fullWithSans(parsed: ParsedPdn, op: Sans => Sans): Result =
    makeReplay(makeGame(parsed.tags), op(parsed.sans))

  def movesWithSans(
      moveStrs: Traversable[String],
      op: Sans => Sans,
      tags: Tags,
      iteratedCapts: Boolean = false
  ): Validated[String, Result] =
    Parser.moves(moveStrs, tags.variant | variant.Variant.default) map { moves =>
      makeReplay(makeGame(tags), op(moves), iteratedCapts)
    }

  // remove invisible byte order mark
  def cleanUserInput(str: String) = str.replace(s"\ufeff", "")

  private def makeReplay(game: DraughtsGame, sans: Sans, iteratedCapts: Boolean = false): Result = {
    def mk(replay: Replay, moves: List[San], ambs: List[(San, String)]): Result = {
      var newAmb = none[(San, String)]
      val res = moves match {
        case san :: rest =>
          san(
            replay.state.situation,
            iteratedCapts,
            if (ambs.isEmpty) None
            else ambs.collect({ case (ambSan, ambUci) if ambSan == san => ambUci }).some
          ).fold(
            err => Result.Incomplete(replay, err),
            move => {
              if (
                iteratedCapts && move.capture.fold(false)(_.length > 1) && move.situationBefore
                  .ambiguitiesMove(move) > ambs.length + 1
              )
                newAmb = (san -> move.toUci.uci).some
              mk(replay.addMove(move, iteratedCapts), rest, newAmb.fold(ambs)(_ :: ambs))
            }
          )
        case _ => Result.Complete(replay)
      }
      res match {
        case Result.Incomplete(_, _) if newAmb.isDefined => mk(replay, moves, newAmb.get :: ambs)
        case _                                           => res
      }
    }
    mk(Replay(game), sans.value, Nil)
  }

  private def makeGame(tags: Tags) = {
    val g = DraughtsGame(
      variantOption = tags.variant,
      fen = tags(_.FEN)
    )
    g.copy(
      startedAtTurn = g.turns,
      clock = tags.clockConfig map (config => Clock.apply(GameLib.Draughts(), config))
    )
  }
}
