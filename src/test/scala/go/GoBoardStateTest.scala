package strategygames.go

import org.specs2.mutable.Specification

import scala.util.Try

import cats.implicits._

import strategygames.Player
import strategygames.go.format.{ FEN, Forsyth }
import strategygames.go.oracle.{ GoOracle, GoOracleGame }
import strategygames.go.variant.{ Go19x19, Go9x9, Variant }

class GoBoardStateTest extends Specification with GoRulesTestSupport {

  import GoBoardStateTest._

  private val opening = List("a1", "b2")

  private val resumptions = List(
    (List("pass"), "pass"),
    (List("pass"), "e5"),
    (List("pass", "pass"), "pass"),
    (List("pass", "pass"), "ss:"),
    (List("pass", "pass"), "e5")
  )

  "the position state a go board carries" should {

    "agree with the uci log at every ply the oracle replays" in {
      disagreements.take(disagreementReportLimit) must beEmpty
    }
  }

  "a board resumed from a fen" should {

    "take its pass run and its settlement from the fen alone" in {
      (resumed(passCount = 0).consecutivePasses === 0) and
        (resumed(passCount = 0).deadStonesSelected === false) and
        (resumed(passCount = 1).consecutivePasses === 1) and
        (resumed(passCount = 1).deadStonesSelected === false) and
        (resumed(passCount = 2).consecutivePasses === 2) and
        (resumed(passCount = 2).deadStonesSelected === false) and
        (resumed(passCount = 3).consecutivePasses === 0) and
        (resumed(passCount = 3).deadStonesSelected === true)
    }

    "write that pass run back out unchanged" in {
      passCounts.map(passCount => Forsyth.exportBoardFen(resumed(passCount)).fenPassCount) === passCounts
    }

    "count none of those passes as a ply" in {
      passCounts.map(passCount => resumedPlies(passCount)) === passCounts.map(_ => pliesOfResumedFen)
    }

    "count its plies from the full turn it names and the player it hands the move to" in {
      forall(plyAccounting) { case (turn, fullTurnCount, plies) =>
        pliesOfFen(turn, fullTurnCount) === plies
      }
    }

    "reach an even ply count for black to move and an odd one for white" in {
      plyAccounting.map { case (turn, fullTurnCount, _) =>
        (turn, pliesOfFen(turn, fullTurnCount) % 2)
      } === List(("b", 0), ("w", 1), ("b", 0), ("w", 1), ("b", 0))
    }

    "leave a pass count it has no reading of out of the position state" in {
      (resumed(passCount = 4).consecutivePasses === 0) and
        (resumed(passCount = 4).deadStonesSelected === false) and
        (Forsyth.exportBoardFen(resumed(passCount = 4)).fenPassCount === 0)
    }

    "take its komi and its ko point from the fen, not from the variant" in {
      (resumed(passCount = 0).komi === 0.0) and
        (Go19x19.komi === 7.5) and
        (resumed(passCount = 0).ko === None) and
        (Forsyth.<<@(Go19x19, FEN(koFen)).map(_.board.ko) === Some(Pos.fromKey("d4")))
    }
  }

  "a game resumed from a fen and then played on" should {

    "reach the fen the same game played straight through reaches" in {
      forall(resumptions) { case (passes, action) =>
        fenOf(playingFrom(fenOf(playing(Go9x9, opening ++ passes)), List(action))) ===
          fenOf(playing(Go9x9, opening ++ passes ++ List(action)))
      }
    }

    "move the pass count by the action it was given and nothing else" in {
      val atOnePass = fenOf(playing(Go9x9, opening ++ List("pass")))
      (atOnePass.fenPassCount === 1) and
        (fenOf(playingFrom(atOnePass, List("pass"))).fenPassCount === 2) and
        (fenOf(playingFrom(atOnePass, List("e5"))).fenPassCount === 0)
    }
  }

  "a settlement recorded on a board" should {

    "survive whatever action follows it" in {
      val settled = Board.init(Go19x19).settled
      (settled.deadStonesSelected === true) and
        (settled.passed.deadStonesSelected === true) and
        (settled.stonePlaced.deadStonesSelected === true) and
        (Forsyth.exportBoardFen(settled.passed).fenPassCount === 3) and
        (Forsyth.exportBoardFen(settled.stonePlaced).fenPassCount === 3)
    }

    "be written back out by a fen that states it" in {
      (resumed(passCount = 3).deadStonesSelected === true) and
        (Forsyth.exportBoardFen(resumed(passCount = 3)).fenPassCount === 3)
    }

    "leave the replay refusing a further drop" in {
      dropOntoSettledFenReplayed.isFailure === true
    }
  }

  "a board initialised from a variant alone" should {

    "take that variant's komi" in {
      (Board.init(Go9x9).komi === 5.5) and (Board.init(Go19x19).komi === 7.5)
    }
  }

  "a board size's neighbour table" should {

    "clip the cardinal neighbours of a corner and an edge to the size" in {
      (Board.Dim9x9.neighbours(Pos.A1.index) === List(Pos.B1, Pos.A2)) and
        (Board.Dim9x9.neighbours(Pos.I1.index) === List(Pos.H1, Pos.I2)) and
        (Board.Dim19x19.neighbours(Pos.I1.index) === List(Pos.H1, Pos.J1, Pos.I2)) and
        (Board.Dim19x19.neighbours(Pos.S19.index) === List(Pos.R19, Pos.S18))
    }

    "hold nothing for a point the size does not have" in {
      Board.BoardSize.all.forall(size =>
        Pos.all.filterNot(size.validPos.contains).forall(pos => size.neighbours(pos.index).isEmpty)
      ) === true
    }

    "name only points of the size, two to four of them, and name them mutually" in {
      Board.BoardSize.all.forall { size =>
        size.validPos.forall { pos =>
          val neighbours = size.neighbours(pos.index)
          neighbours.forall(size.validPos.contains) &&
          neighbours.size >= 2 && neighbours.size <= 4 &&
          neighbours.forall(neighbour => size.neighbours(neighbour.index).contains(pos))
        }
      } === true
    }
  }
}

object GoBoardStateTest {

  private val disagreementReportLimit = 25

  private val passAction = "pass"

  val passCounts: List[Int] = List(0, 1, 2, 3)

  private val resumedFenWithoutPassCount =
    "19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19[SSSSSSSSSSssssssssss] b - 0 0 0 0 0"

  private val koFen =
    "19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19[SSSSSSSSSSssssssssss] b d4 0 75 0 0 75 0 21"

  val pliesOfResumedFen: Int = 40

  def resumed(passCount: Int): Board =
    Forsyth
      .<<@(Go19x19, resumedFen(passCount))
      .getOrElse(sys.error(s"unreadable resumed fen for pass count ${passCount}"))
      .board

  def resumedPlies(passCount: Int): Int =
    Forsyth
      .<<<@(Go19x19, resumedFen(passCount))
      .getOrElse(sys.error(s"unreadable resumed fen for pass count ${passCount}"))
      .plies

  private def resumedFen(passCount: Int): FEN =
    FEN(s"${resumedFenWithoutPassCount} ${passCount} ${pliesOfResumedFen / 2 + 1}")

  val plyAccounting: List[(String, Int, Int)] =
    List(("b", 1, 0), ("w", 1, 1), ("b", 2, 2), ("w", 2, 3), ("b", 30, 58))

  def pliesOfFen(turn: String, fullTurnCount: Int): Int =
    Forsyth
      .<<<@(
        Go19x19,
        FEN(
          "19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19" +
            s"[SSSSSSSSSSssssssssss] ${turn} - 0 0 0 0 0 0 ${fullTurnCount}"
        )
      )
      .getOrElse(sys.error(s"unreadable fen for ${turn} at full turn ${fullTurnCount}"))
      .plies

  private val settledFen = resumedFen(3)

  private val dropOntoSettledFen = Vector(Vector("s@d4"))

  private val playerAfterSettledFenDrop = Player.P2

  lazy val dropOntoSettledFenReplayed: Try[Any] =
    Try(
      Replay.gameFromUciStrings(dropOntoSettledFen, playerAfterSettledFenDrop, settledFen.some, Go19x19)
    )

  lazy val disagreements: List[String] =
    GoOracle.load().flatMap(game => prefixDisagreements(game) ++ sweptDisagreements(game))

  private def prefixDisagreements(game: GoOracleGame): List[String] =
    (0 to game.actionStrs.size).toList.flatMap { played =>
      val actions = game.actionStrs.take(played)
      boardDisagreements(
        s"${game.name} prefix ply ${played}",
        replayedFromUci(game, actions).situation,
        actions
      )
    }

  private def sweptDisagreements(game: GoOracleGame): List[String] = {
    val (init, swept, _) = Replay.gameWithUciWhileValid(
      game.actionStrs.map(Vector(_)).toVector,
      startPlayerOf(game),
      activePlayerAfter(game, game.actionStrs.size),
      initialFenOf(game),
      variantOf(game)
    )
    boardDisagreements(s"${game.name} swept ply 0", init.situation, Nil) ++
      swept.map(_._1).zipWithIndex.flatMap { case (state, index) =>
        boardDisagreements(
          s"${game.name} swept ply ${index + 1}",
          state.situation,
          game.actionStrs.take(index + 1)
        )
      }
  }

  private def replayedFromUci(game: GoOracleGame, actions: List[String]): Game =
    Replay
      .gameFromUciStrings(
        actions.map(Vector(_)).toVector,
        activePlayerAfter(game, actions.size),
        game.initialFen.map(FEN(_)),
        variantOf(game)
      )
      .valueOr(error => sys.error(s"go board state replay of ${game.name}: ${error}"))

  private def variantOf(game: GoOracleGame): Variant =
    Variant(game.variantKey).getOrElse(sys.error(s"unknown go variant: ${game.variantKey}"))

  private def initialFenOf(game: GoOracleGame): FEN =
    game.initialFen.map(FEN(_)).getOrElse(variantOf(game).initialFen)

  private def startPlayerOf(game: GoOracleGame): Player =
    initialFenOf(game).player.getOrElse(Player.P1)

  private def activePlayerAfter(game: GoOracleGame, turns: Int): Player =
    Player.fromTurnCount(turns + startPlayerOf(game).fold(0, 1))

  private def boardDisagreements(
      named: String,
      situation: Situation,
      played: List[String]
  ): List[String] = {
    val board = situation.board
    List(
      disagreed(named, "playerToMove", board.playerToMove, situation.player),
      disagreed(named, "passState", passStateOf(board), passStateLoggedBy(played)),
      disagreed(named, "canSelectSquares", situation.canSelectSquares, canSelectSquaresLoggedBy(played)),
      disagreed(
        named,
        "isSubsequentPassWarning",
        situation.isSubsequentPassWarning,
        isSubsequentPassWarningLoggedBy(played)
      )
    ).flatten
  }

  private def disagreed[A](named: String, field: String, carried: A, stated: A): Option[String] =
    if (carried == stated) None
    else Some(s"${named} ${field}: board carries ${carried}, oracle states ${stated}")

  private def passStateOf(board: Board): Int =
    if (board.deadStonesSelected) 3 else board.consecutivePasses.min(2)

  private val passesSettlingTheGame = 4

  private def trailingPasses(played: List[String]): Int =
    played.reverse.takeWhile(_ == passAction).length

  private def settledBy(played: List[String]): Boolean =
    played.lastOption.exists(_.startsWith("ss:")) || trailingPasses(played) >= passesSettlingTheGame

  private def passStateLoggedBy(played: List[String]): Int =
    if (settledBy(played)) 3 else trailingPasses(played).min(2)

  private def canSelectSquaresLoggedBy(played: List[String]): Boolean =
    isSubsequentPassWarningLoggedBy(played) && trailingPasses(played) % 2 == 0

  private def isSubsequentPassWarningLoggedBy(played: List[String]): Boolean =
    !settledBy(played) && trailingPasses(played) >= 2
}
