package strategygames.go

import org.specs2.mutable.Specification

import strategygames.Player
import strategygames.go.format.FEN

class GoPs11ParityTest extends Specification with GoRulesTestSupport {

  import GoPs11ParityTest._

  private def replayed(game: StoredGoGame): Game =
    Replay
      .gameFromUciStrings(
        game.actions.map(Vector(_)).toVector,
        Player.fromTurnCount(game.plies),
        Some(game.initialFen),
        game.variant
      )
      .valueOr(refusal => sys.error(s"${game.id}: ${refusal}"))

  private lazy val replays: List[(StoredGoGame, Game)] =
    StoredGoGames.all.map(game => (game, replayed(game)))

  private def winnerOf(played: Game): String =
    played.situation.winner.fold("none")(_.fold("p1", "p2"))

  private def fieldsDiffering(game: StoredGoGame, played: Game): List[Int] = {
    val ours   = fieldsOf(fenOf(played))
    val theirs = fieldsOf(game.ps11.finalFen)
    ours.indices.filter(field => ours(field) != theirs(field)).toList
  }

  private def gamesDifferingAt(field: Int): List[String] =
    replays.collect { case (game, played) if fieldsDiffering(game, played).contains(field) => game.id }

  "our replay of every stored game" should {

    "reach the ply ps11 reached" in {
      forall(replays) { case (game, played) => played.plies === game.ps11.plies }
    }

    "end where ps11 ended" in {
      forall(replays) { case (game, played) => played.situation.end === game.ps11.end }
    }

    "report the status ps11 reported" in {
      forall(replays) { case (game, played) =>
        played.situation.status.fold("none")(_.toString) === game.ps11.status
      }
    }

    "reach the board, player, scores, komi and turn ps11 reached" in {
      forall(replays) { case (game, played) =>
        comparableFen(fenOf(played)) === comparableFen(game.ps11.finalFen)
      }
    }

    "differ from ps11 nowhere else in the fen" in {
      forall(replays) { case (game, played) =>
        fieldsDiffering(game, played).filterNot(knownToDiffer.contains) === Nil
      }
    }
  }

  "the places our replay knowingly differs from ps11" should {

    "be the ko point of a game ps11 could not recover one for" in {
      gamesDifferingAt(koPoint) === List("9OQaGrmi", "PTbCdQhR", "alsd1TB5", "tO4EZZoF", "yERmsWJF")
    }

    "be the capture count of every game ps11 counted a settlement into" in {
      gamesDifferingAt(p1Captures).toSet.union(gamesDifferingAt(p2Captures).toSet) must haveSize(46)
    }

    "be the settlement state of the two games that end on a repetition" in {
      gamesDifferingAt(passState) === endingOnARepetition
    }

    "be the winner of those same two games, which ps11 left unnamed" in {
      replays.collect { case (game, played) if winnerOf(played) != game.ps11.winner => game.id } ===
        endingOnARepetition
    }

    "leave ps11 naming no winner for them despite the area score it reached" in {
      forall(StoredGoGames.all.filter(game => endingOnARepetition.contains(game.id))) { game =>
        game.ps11.winner === "none"
      }
    }
  }
}

object GoPs11ParityTest {

  private val koPoint = 2

  private val p1Captures = 5

  private val p2Captures = 6

  private val passState = 8

  val knownToDiffer: List[Int] = List(koPoint, p1Captures, p2Captures, passState)

  val endingOnARepetition: List[String] = List("gKIjsXZc", "wH9kQR7E")

  def fieldsOf(fen: FEN): Vector[String] = fen.value.split(' ').toVector

  def comparableFen(fen: FEN): String =
    fieldsOf(fen).zipWithIndex
      .map { case (field, index) => if (knownToDiffer.contains(index)) "?" else field }
      .mkString(" ")
}
