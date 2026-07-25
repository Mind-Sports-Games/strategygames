package strategygames.bench

import org.specs2.mutable.Specification

import cats.data.Validated

import strategygames.{ ActionStrs, Player, Score }
import strategygames.go.{ Api, Game, Replay }
import strategygames.go.format.{ FEN, Forsyth }
import strategygames.go.variant.{ Go9x9, Variant => GoVariant }

class GoReplayDefaultSpec extends Specification {

  private val framings = List(Player.P1, Player.P2)

  private val opening = List("s@a1", "s@c3")

  private def turnPerAction(actions: List[String]): ActionStrs =
    actions.map(Vector(_)).toVector

  private def unwrap(replayed: Validated[String, Game]): Game = replayed match {
    case Validated.Valid(game)    => game
    case Validated.Invalid(error) => sys.error(s"go replay failed: ${error}")
  }

  private def fast(
      actionStrs: ActionStrs,
      activePlayer: Player,
      initialFen: Option[FEN],
      variant: GoVariant
  ): Game = unwrap(Replay.gameFromUciStrings(actionStrs, activePlayer, initialFen, variant))

  private def perPly(
      actionStrs: ActionStrs,
      activePlayer: Player,
      initialFen: Option[FEN],
      variant: GoVariant
  ): Game = unwrap(Replay.gameFromUciStringsPerPly(actionStrs, activePlayer, initialFen, variant))

  private def fieldsOf(game: Game): List[(String, Any)] = List(
    "fen"           -> (Forsyth >> game).value,
    "pieces"        -> game.situation.board.pieces,
    "player"        -> game.situation.player,
    "status"        -> game.situation.status,
    "end"           -> game.situation.end,
    "winner"        -> game.situation.winner,
    "plies"         -> game.plies,
    "turnCount"     -> game.turnCount,
    "startedAtPly"  -> game.startedAtPly,
    "startedAtTurn" -> game.startedAtTurn,
    "actionStrs"    -> game.actionStrs,
    "uciMoves"      -> game.situation.board.uciMoves,
    "captures"      -> game.situation.history.captures,
    "score"         -> game.situation.history.score,
    "halfMoveClock" -> game.situation.history.halfMoveClock,
    "lastTurn"      -> game.situation.history.lastTurn.map(_.uci),
    "currentTurn"   -> game.situation.history.currentTurn.map(_.uci),
    "pocketData"    -> game.situation.board.pocketData
  )

  private def mismatches(
      label: String,
      actionStrs: ActionStrs,
      initialFen: Option[FEN],
      variant: GoVariant
  ): List[String] =
    framings.flatMap { activePlayer =>
      fieldsOf(fast(actionStrs, activePlayer, initialFen, variant))
        .zip(fieldsOf(perPly(actionStrs, activePlayer, initialFen, variant)))
        .collect {
          case ((field, fromFast), (_, fromPerPly)) if fromFast != fromPerPly =>
            s"${label} activePlayer=${activePlayer} ${field}: perPly=${fromPerPly} fast=${fromFast}"
        }
    }

  private def corpusMismatches(sizeKey: String): List[String] = {
    val corpus = GoCorpusGame.load(sizeKey)
    mismatches(sizeKey, corpus.actionStrs, corpus.initialFen, corpus.size.variant)
  }

  private def resumedFen(actions: List[String]): FEN =
    Api.positionFromVariantNameAndFEN(Go9x9.key, Go9x9.initialFen.value).makeMoves(actions).fen

  private val handWritten: List[(String, ActionStrs, Option[FEN])] = List(
    (
      "trailing dead stone selection",
      turnPerAction(opening ++ List("pass", "pass", "ss:a1")),
      None
    ),
    (
      "four trailing passes",
      turnPerAction(opening ++ List("pass", "pass", "pass", "pass")),
      None
    ),
    (
      "interleaved passes then a settlement",
      turnPerAction(List("s@a1", "pass", "s@c3", "pass", "pass", "ss:c3")),
      None
    ),
    (
      "an explicit variant initial fen",
      turnPerAction(opening ++ List("s@e5", "s@g7", "pass", "pass", "ss:e5")),
      Some(Go9x9.initialFen)
    )
  )

  "the batch default go replay" should {

    "agree with the per ply path on every field of every committed go corpus" in {
      GoBoardSize.all.flatMap(size => corpusMismatches(size.key)) must beEmpty
    }

    "agree with the per ply path on every hand written ending" in {
      handWritten.flatMap { case (label, actionStrs, initialFen) =>
        mismatches(label, actionStrs, initialFen, Go9x9)
      } must beEmpty
    }
  }

  "a game resumed from a fen counting one or two passes" should {

    val resumptions = for {
      passes <- List(List("pass"), List("pass", "pass"))
      action <- List("pass", "ss:a1")
    } yield (s"resume after ${passes.size} pass then ${action}", resumedFen(opening ++ passes), action)

    "agree with the per ply path on every field" in {
      resumptions.flatMap { case (label, fen, action) =>
        mismatches(label, turnPerAction(List(action)), Some(fen), Go9x9)
      } must beEmpty
    }

    "count only the action given as a ply on both paths" in {
      resumptions
        .map { case (_, fen, action) =>
          val actionStrs = turnPerAction(List(action))
          val batch      = fast(actionStrs, Player.P2, Some(fen), Go9x9)
          val oracle     = perPly(actionStrs, Player.P2, Some(fen), Go9x9)
          (batch.plies must beEqualTo(oracle.startedAtPly + 1)) and
            (batch.plies must beEqualTo(oracle.plies)) and
            (batch.turnCount must beEqualTo(oracle.turnCount))
        }
        .reduce(_ and _)
    }

    "agree on the raw engine fen and on dead stone selection availability" in {
      resumptions
        .map { case (_, fen, action) =>
          val actionStrs = turnPerAction(List(action))
          val batch      = fast(actionStrs, Player.P1, Some(fen), Go9x9)
          val oracle     = perPly(actionStrs, Player.P1, Some(fen), Go9x9)
          (batch.situation.board.apiPosition.fen must beEqualTo(oracle.situation.board.apiPosition.fen)) and
            (batch.situation.canSelectSquares must beEqualTo(oracle.situation.canSelectSquares))
        }
        .reduce(_ and _)
    }
  }

  "a game resumed from a fen whose dead stones are already settled" should {

    val settled = resumedFen(opening ++ List("pass", "pass", "ss:a1"))

    "refuse a further action on both paths" in {
      (fast(turnPerAction(List("pass")), Player.P2, Some(settled), Go9x9) must throwAn[Exception]) and
        (perPly(turnPerAction(List("pass")), Player.P2, Some(settled), Go9x9) must throwAn[Exception])
    }
  }

  "the score of a pass terminated game" should {

    "equal the final position's score on both paths when a drop preceded the passes" in {
      val actionStrs = turnPerAction(opening ++ List("pass", "pass"))
      val batch      = fast(actionStrs, Player.P1, None, Go9x9)
      val oracle     = perPly(actionStrs, Player.P1, None, Go9x9)
      (batch.situation.history.score must beEqualTo(oracle.situation.history.score)) and
        (batch.situation.history.score must beEqualTo(batch.situation.board.apiPosition.fenScore)) and
        (oracle.situation.history.score must beEqualTo(oracle.situation.board.apiPosition.fenScore))
    }

    "stay unscored on both paths when no drop was ever played" in {
      val actionStrs = turnPerAction(List("pass", "pass"))
      val batch      = fast(actionStrs, Player.P1, None, Go9x9)
      val oracle     = perPly(actionStrs, Player.P1, None, Go9x9)
      (batch.situation.history.score must beEqualTo(Score(0, 0))) and
        (oracle.situation.history.score must beEqualTo(Score(0, 0)))
    }
  }
}
