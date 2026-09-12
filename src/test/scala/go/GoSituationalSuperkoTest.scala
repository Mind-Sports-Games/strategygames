package strategygames.go

import org.specs2.mutable.Specification

import scala.util.Try

import strategygames.Player
import strategygames.format.pgn.{ Tag, Tags }
import strategygames.go.format.{ FEN, Uci }
import strategygames.go.variant.{ Go19x19, Go9x9 }

class GoSituationalSuperkoTest extends Specification with GoRulesTestSupport {

  import GoSituationalSuperkoTest._

  private val goTags = Tags(List(Tag(_.Variant, Go9x9.name)))

  private def turnPerAction(actions: List[String]) = actions.map(Vector(_)).toVector

  private def gameFromUciStrings(actions: List[String]) =
    Replay
      .gameFromUciStrings(turnPerAction(actions), Player.fromTurnCount(actions.size), None, Go9x9)
      .valueOr(sys.error)

  private def replayFromActionStrs(actions: List[String]) =
    Replay
      .apply(turnPerAction(actions), P1, Player.fromTurnCount(actions.size), None, Go9x9)
      .andThen(_.valid)
      .valueOr(sys.error)

  private def readerFromActionStrs(actions: List[String]) =
    format.pgn.Reader
      .replayResultFromActionStrs(turnPerAction(actions), identity, goTags)
      .andThen(_.valid)
      .valueOr(sys.error)

  private def replayFromUciList(actions: List[String]) =
    Replay(actions.flatMap(Uci(_)), None, Go9x9)

  private def situationsFromUciList(actions: List[String]) =
    Replay.situationsFromUci(actions.flatMap(Uci(_)), None, Go9x9)

  private def refusalPerLoader(keys: List[String]): List[Boolean] = {
    val actions = dropsOf(keys)
    List(
      Try(gameFromUciStrings(actions)).isFailure,
      Try(replayFromActionStrs(actions)).isFailure,
      Try(readerFromActionStrs(actions)).isFailure,
      replayFromUciList(actions).isInvalid,
      situationsFromUciList(actions).isInvalid
    )
  }

  private val everyLoader = List.fill(5)(true)
  private val noLoader    = List.fill(5)(false)

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

    "be accepted by every loader that replays a stored game" in {
      refusalPerLoader(theWholeCycle) === noLoader
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

    "be refused by every loader when the record continues past it" in {
      refusalPerLoader(tripleKoReturningToTheSamePlayer :+ anyFurtherPoint) === everyLoader
    }

    "be accepted by every loader as the last action of a record" in {
      refusalPerLoader(tripleKoReturningToTheSamePlayer) === noLoader
    }

    "end the record it closes" in {
      gameFromUciStrings(dropsOf(tripleKoReturningToTheSamePlayer)).situation.end === true
    }
  }

  "a stored action that is illegal for any reason other than superko" should {

    "still be refused on an occupied point" in {
      refusalPerLoader(onAnOccupiedPoint) === everyLoader
    }

    "still be refused when it is suicide" in {
      refusalPerLoader(asSuicide) === everyLoader
    }

    "still be refused when it recaptures at the simple ko point" in {
      refusalPerLoader(atTheSimpleKoPoint) === everyLoader
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

  val anyFurtherPoint: String = "a1"

  val onAnOccupiedPoint: List[String] = List("e5", "e5")

  val asSuicide: List[String] = List("e5", "a2", "e6", "b1", "a1")

  val atTheSimpleKoPoint: List[String] =
    List("b2", "c2", "a3", "d3", "b4", "c4", "c3", "b3", "c3")

  def dropsOf(keys: List[String]): List[String] = keys.map(key => s"s@${key}")

  private lazy val storedGame: StoredGoGame = StoredGoGames.named("yERmsWJF")

  lazy val storedFen: FEN = storedGame.initialFen

  lazy val storedActions: List[String] = storedGame.actions
}
