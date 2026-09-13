package strategygames.go

import java.nio.charset.StandardCharsets

import org.specs2.mutable.Specification

import strategygames.Player
import strategygames.go.format.FEN
import strategygames.go.variant.{ Go19x19, Go9x9 }

class GoSituationalSuperkoTest extends Specification with GoRulesTestSupport {

  import GoSituationalSuperkoTest._

  private def gameFromUciStrings(actions: List[String]) =
    Replay
      .gameFromUciStrings(actions.map(Vector(_)).toVector, Player.fromTurnCount(actions.size), None, Go9x9)
      .valueOr(sys.error)

  private val pliesOfTheCycle = 6

  "the recapture that returns the board to where it stood an odd number of plies earlier" should {

    val beforeTheReturn = playing(Go9x9, upToTheReturningCapture)

    "stand on a board with no ko point" in {
      koPointOf(fenOf(beforeTheReturn)) === "-"
    }

    "be offered by drop generation" in {
      dropKeysOf(beforeTheReturn.situation) must contain(returningCapture)
    }

    "be offered by the drop map" in {
      beforeTheReturn.situation.drops.map(_.map(_.key)).getOrElse(Nil) must contain(returningCapture)
    }

    "be accepted when asked for by name" in {
      beforeTheReturn.situation.drop(Role.defaultRole, pointAt(returningCapture)).isValid === true
    }

    "leave the game playable rather than end it" in {
      playingOn(beforeTheReturn, List(returningCapture)).situation.end === false
    }

    "reach the board the game held three plies earlier, with the other player to move" in {
      val returned = playingOn(beforeTheReturn, List(returningCapture))
      val earlier  = playing(Go9x9, upToTheReturningCapture.take(9))
      (returned.board.pieces === earlier.board.pieces) and
        (returned.situation.player === !earlier.situation.player)
    }

    "replay onto that same board" in {
      val returned = gameFromUciStrings(dropsOf(theWholeCycle))
      (returned.plies === theWholeCycle.size) and
        (returned.board.pieces === playing(Go9x9, upToTheReturningCapture.take(9)).board.pieces)
    }
  }

  "a repeat that brings back the same player to move as well as the same stones" should {

    val beforeTheRepeat = playing(Go9x9, tripleKoReturningToTheSamePlayer.init)

    "not be offered by drop generation" in {
      dropKeysOf(beforeTheRepeat.situation) must not(contain(tripleKoReturningToTheSamePlayer.last))
    }

    "be refused when asked for by name" in {
      beforeTheRepeat.situation
        .drop(Role.defaultRole, pointAt(tripleKoReturningToTheSamePlayer.last))
        .isInvalid === true
    }

    "replay onto the board it recreates, with the game still going" in {
      val replayed = gameFromUciStrings(dropsOf(tripleKoReturningToTheSamePlayer))
      val earlier  = playing(Go9x9, tripleKoReturningToTheSamePlayer.dropRight(pliesOfTheCycle))
      (replayed.plies === tripleKoReturningToTheSamePlayer.size) and
        (replayed.board.pieces === earlier.board.pieces) and
        (replayed.situation.player === earlier.situation.player) and
        (replayed.situation.end === false)
    }
  }

  "the stored 19x19 game" should {

    "hold every ply of it" in {
      storedActions.size === 1001
    }

    "replay to its last ply" in {
      val replayed = Replay
        .gameFromUciStrings(
          storedActions.map(Vector(_)).toVector,
          Player.fromTurnCount(storedActions.size),
          Some(storedFen),
          Go19x19
        )
        .valueOr(sys.error)
      replayed.plies === storedActions.size
    }
  }
}

object GoSituationalSuperkoTest {

  val upToTheReturningCapture: List[String] =
    List("e3", "g3", "f2", "h2", "f4", "h4", "g2", "i3", "g4", "f3", "h3")

  val returningCapture: String = "g3"

  val theWholeCycle: List[String] = upToTheReturningCapture :+ returningCapture

  val tripleKoReturningToTheSamePlayer: List[String] = List(
    "b8",
    "b7",
    "c9",
    "c6",
    "d8",
    "d7",
    "f8",
    "f7",
    "g9",
    "g6",
    "h8",
    "h7",
    "b2",
    "b3",
    "c1",
    "c4",
    "d2",
    "d3",
    "f2",
    "f3",
    "g1",
    "g4",
    "h2",
    "h3",
    "c7",
    "c2",
    "g3",
    "g8",
    "g7",
    "c8",
    "c3",
    "g2",
    "c7",
    "c2",
    "g3"
  )

  def dropsOf(keys: List[String]): List[String] = keys.map(key => s"s@${key}")

  private val resourcePath = "/go/yERmsWJF.moves"

  private lazy val storedLines: List[String] = {
    val stream = Option(getClass.getResourceAsStream(resourcePath))
      .getOrElse(sys.error(s"missing stored go game resource ${resourcePath}"))
    try new String(stream.readAllBytes(), StandardCharsets.UTF_8).linesIterator.toList
    finally stream.close()
  }

  lazy val storedFen: FEN = FEN(storedLines.head)

  lazy val storedActions: List[String] = storedLines(1).trim.split("\\s+").toList
}
