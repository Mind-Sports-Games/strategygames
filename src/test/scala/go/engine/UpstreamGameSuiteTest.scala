package strategygames.go.engine

import java.nio.charset.StandardCharsets

import org.specs2.mutable.Specification

class UpstreamGameSuiteTest extends Specification {

  import UpstreamGameSuiteTest._

  "every upstream 19x19 suite game" should {
    "replay from the empty board with only legal moves" in {
      forall(games) { game =>
        replay(game) must beRight
      }
    }
    "leave a board whose stones and captures account for every placement" in {
      forall(games) { game =>
        replay(game) must beRight { (state: GoState) =>
          val blackPlaced  = game.moves.zipWithIndex.count(_._2 % 2 == 0)
          val whitePlaced  = game.moves.length - blackPlaced
          val blackOnBoard =
            (0 until state.passMove).count(state.stoneOwnerAt(_) == GoState.BlackPlayer)
          val whiteOnBoard =
            (0 until state.passMove).count(state.stoneOwnerAt(_) == GoState.WhitePlayer)
          (blackOnBoard + state.capturesByWhite === blackPlaced) and
            (whiteOnBoard + state.capturesByBlack === whitePlaced)
        }
      }
    }
    "capture at least one stone in each game" in {
      forall(games) { game =>
        replay(game) must beRight { (state: GoState) =>
          state.capturesByBlack + state.capturesByWhite must be_>(0)
        }
      }
    }
  }

  "the ported suite" should {
    "hold every upstream bench game" in {
      games.length === 8
    }
  }
}

object UpstreamGameSuiteTest {

  final case class SuiteGame(index: Int, moves: Vector[Int])

  private val boardSize = 19

  private val upstreamFiles = "abcdefghjklmnopqrst"

  private val resourcePath = "/go/upstream-go-bench.suite"

  lazy val games: List[SuiteGame] = readSuite.linesIterator.zipWithIndex.collect {
    case (line, index) if line.contains(" moves ") =>
      SuiteGame(index, line.split(" moves ", 2)(1).trim.split("\\s+").toVector.map(engineMoveOf))
  }.toList

  def replay(game: SuiteGame): Either[String, GoState] =
    game.moves.zipWithIndex.foldLeft[Either[String, GoState]](Right(GoState.initial(boardSize))) {
      case (Right(state), (move, ply)) =>
        if (state.isLegal(move)) Right(state(move))
        else Left(s"game ${game.index} ply $ply: illegal move $move")
      case (failure, _)                => failure
    }

  private def engineMoveOf(coordinate: String): Int = {
    val file = upstreamFiles.indexOf(coordinate.charAt(0))
    val rank = coordinate.drop(1).toInt
    require(file >= 0 && file < boardSize, s"unknown upstream file in $coordinate")
    require(rank >= 1 && rank <= boardSize, s"unknown upstream rank in $coordinate")
    boardSize * (rank - 1) + file
  }

  private def readSuite: String = {
    val stream = Option(getClass.getResourceAsStream(resourcePath))
      .getOrElse(sys.error(s"missing upstream go suite resource $resourcePath"))
    try new String(stream.readAllBytes(), StandardCharsets.UTF_8)
    finally stream.close()
  }
}
