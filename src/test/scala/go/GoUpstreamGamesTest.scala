package strategygames.go

import java.nio.charset.StandardCharsets

import org.specs2.mutable.Specification

import strategygames.go.variant.Go19x19

class GoUpstreamGamesTest extends Specification with GoRulesTestSupport {

  import GoUpstreamGamesTest._

  private def playedWithOnlyLegalMoves(game: UpstreamGame): Either[String, Game] =
    game.keys.zipWithIndex.foldLeft[Either[String, Game]](Right(Game(Go19x19))) {
      case (Right(played), (key, ply)) =>
        played.situation
          .drop(Role.defaultRole, pointAt(key))
          .fold(
            error => Left(s"game ${game.index} ply ${ply}: ${error}"),
            drop => Right(played.apply(drop))
          )
      case (failure, _)                => failure
    }

  private lazy val replayed: List[(UpstreamGame, Either[String, Game])] =
    games.map(game => (game, playedWithOnlyLegalMoves(game)))

  "the ported upstream suite" should {
    "hold every upstream bench game" in {
      games.size === 8
    }
  }

  "every upstream 19x19 suite game" should {
    "replay from the empty board with only legal moves" in {
      forall(replayed) { case (_, outcome) => outcome must beRight }
    }
    "account for every placement as a stone still standing or a stone taken" in {
      forall(replayed) { case (game, outcome) =>
        outcome must beRight { (played: Game) =>
          val blackPlaced   = game.keys.zipWithIndex.count(_._2 % 2 == 0)
          val whitePlaced   = game.keys.size - blackPlaced
          val blackStanding = played.board.pieces.count(_._2.player == P1)
          val whiteStanding = played.board.pieces.count(_._2.player == P2)
          val taken         = played.situation.history.captures
          (blackStanding + taken.p2 === blackPlaced) and
            (whiteStanding + taken.p1 === whitePlaced)
        }
      }
    }
    "take at least one stone" in {
      forall(replayed) { case (_, outcome) =>
        outcome must beRight { (played: Game) =>
          val taken = played.situation.history.captures
          taken.p1 + taken.p2 must be_>(0)
        }
      }
    }
  }
}

object GoUpstreamGamesTest {

  final case class UpstreamGame(index: Int, keys: List[String])

  private val upstreamFiles = "abcdefghjklmnopqrst"

  private val resourcePath = "/go/upstream-go-bench.suite"

  lazy val games: List[UpstreamGame] = suiteText.linesIterator.zipWithIndex.collect {
    case (line, index) if line.contains(" moves ") =>
      UpstreamGame(index, line.split(" moves ", 2)(1).trim.split("\\s+").toList.map(keyOf))
  }.toList

  private def keyOf(upstreamCoordinate: String): String = {
    val file = upstreamFiles.indexOf(upstreamCoordinate.charAt(0))
    require(file >= 0, s"unknown upstream file in ${upstreamCoordinate}")
    s"${('a' + file).toChar}${upstreamCoordinate.drop(1)}"
  }

  private def suiteText: String = {
    val stream = Option(getClass.getResourceAsStream(resourcePath))
      .getOrElse(sys.error(s"missing upstream go suite resource ${resourcePath}"))
    try new String(stream.readAllBytes(), StandardCharsets.UTF_8)
    finally stream.close()
  }
}
