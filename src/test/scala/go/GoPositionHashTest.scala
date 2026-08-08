package strategygames.go

import org.specs2.mutable.Specification

import strategygames.Player
import strategygames.go.format.FEN
import strategygames.go.oracle.{ GoOracle, GoOracleGame }
import strategygames.go.variant.{ Go9x9, Variant }

class GoPositionHashTest extends Specification with GoRulesTestSupport {

  private val mismatchReportLimit = 5

  private val gameEndingInASettlement = List("d4", "f4", "e6", "pass", "pass", "ss:")

  private val threeStones = List(P1 -> "d4", P2 -> "f4", P1 -> "d6")

  private val threeStonesHashOfRecord = 0x429d389bed64743cL

  "the empty board" should {
    "hash to nothing, because only stones are hashed" in {
      Hash.positionHash(Board.init(Go9x9)) === 0L
    }
    "start its history at that one position" in {
      (Board.init(Go9x9).history.positionCount === 1) and
        (Board.init(Go9x9).history.currentPosition === Some(0L))
    }
  }

  "the zobrist table the position hash is drawn from" should {
    "hash three stones to the value of record, which no change may move without rewriting every stored history" in {
      Hash.positionHash(boardOf(Go9x9, threeStones)) === threeStonesHashOfRecord
    }
    "hash a played game reaching those stones to the very same value" in {
      Hash.positionHash(playing(Go9x9, List("d4", "f4", "d6")).board) === threeStonesHashOfRecord
    }
  }

  "two move orders reaching the same stones" should {
    val oneOrder   = playing(Go9x9, List("d4", "f4", "d6", "f6"))
    val otherOrder = playing(Go9x9, List("d6", "f6", "d4", "f4"))
    "recompute to the same hash" in {
      Hash.positionHash(oneOrder.board) === Hash.positionHash(otherOrder.board)
    }
    "have recorded the same hash while they were played" in {
      oneOrder.situation.history.currentPosition === otherOrder.situation.history.currentPosition
    }
  }

  "a pass" should {
    val beforePass = playing(Go9x9, List("d4", "f4"))
    val afterPass  = playingOn(beforePass, List("pass"))
    "leave the position it recorded last untouched" in {
      afterPass.situation.history.currentPosition === beforePass.situation.history.currentPosition
    }
    "record no position of its own" in {
      afterPass.situation.history.positionCount === beforePass.situation.history.positionCount
    }
  }

  "a settlement" should {
    "restart the history from the settled position" in {
      playing(Go9x9, gameEndingInASettlement).situation.history.positionCount === 1
    }
    "restart it too when the game is rebuilt action at a time from its uci" in {
      replayedPerPly(Go9x9, gameEndingInASettlement).situation.history.positionCount === 1
    }
  }

  "a fen load" should {
    "start the history at the loaded position" in {
      val loaded = situationFrom(fenOf(playing(Go9x9, List("d4", "f4"))))
      (loaded.history.positionCount === 1) and
        (loaded.history.currentPosition === Some(Hash.positionHash(loaded.board)))
    }
  }

  "a position that the history holds" should {
    "be reported as having occurred, whether it is the newest or an older one" in {
      val played  = playing(Go9x9, List("d4", "f4", "d6"))
      val history = played.situation.history
      (0 until history.positionCount).forall(index => history.hasOccurred(history.positionAt(index))) === true
    }
    "not be confused with a position the history has never held" in {
      playing(Go9x9, List("d4", "f4")).situation.history.hasOccurred(
        Hash.positionHash(playing(Go9x9, List("d4", "f4", "d6")).board)
      ) === false
    }
  }

  "every ply of every game in the go oracle" should {
    "carry a recorded hash equal to a full recompute of its own board" in {
      oracleGames.flatMap(recordedHashMismatchesIn).take(mismatchReportLimit) must beEmpty
    }
    "amount to the whole corpus, rather than a silently empty sweep" in {
      (oracleGames.size === 85) and (oracleGames.map(_.actionStrs.size + 1).sum === 16207)
    }
  }

  "the batch replay path, which never materialises the positions it skips over" should {
    "record no position history at all, a gap this states rather than endorses" in {
      val actions = List("d4", "f4", "d6")
      (batchReplayed(Go9x9, actions).situation.history.positionCount === 0) and
        (replayedPerPly(Go9x9, actions).situation.history.positionCount === actions.size + 1)
    }
  }

  private lazy val oracleGames = GoOracle.load()

  private def boardOf(variant: Variant, stones: List[(Player, String)]): Board =
    Board(stones.map { case (player, key) => pointAt(key) -> Piece(player, Role.defaultRole) }, variant)

  private def recordedHashMismatchesIn(game: GoOracleGame): List[String] =
    situationsOf(game).zipWithIndex.flatMap { case (situation, ply) =>
      mismatchAt(game.name, ply, situation)
    }

  private def mismatchAt(name: String, ply: Int, situation: Situation): Option[String] = {
    val recorded   = situation.history.currentPosition
    val recomputed = Hash.positionHash(situation.board)
    if (recorded == Some(recomputed)) None
    else Some(s"${name} ply ${ply}: recorded ${recorded}, recomputed ${recomputed}")
  }

  private def situationsOf(game: GoOracleGame): List[Situation] = {
    val variant              = variantOf(game)
    val initialFen           = game.initialFen.map(FEN(_)).getOrElse(variant.initialFen)
    val startPlayer          = initialFen.player.getOrElse(Player.P1)
    val (init, plies, error) = Replay.gameWithUciWhileValid(
      game.actionStrs.map(Vector(_)).toVector,
      startPlayer,
      Player.fromTurnCount(game.actionStrs.size + startPlayer.fold(0, 1)),
      initialFen,
      variant
    )
    error.foreach(message => sys.error(s"go oracle replay of ${game.name}: ${message}"))
    init.situation :: plies.map(_._1.situation)
  }

  private def variantOf(game: GoOracleGame): Variant =
    Variant(game.variantKey).getOrElse(sys.error(s"unknown go variant: ${game.variantKey}"))

  private def replayedPerPly(variant: Variant, actions: List[String]): Game =
    Replay
      .gameFromUciStringsPerPly(
        actions.map(action => Vector(uciStringOf(action))).toVector,
        Player.fromTurnCount(actions.size),
        None,
        variant
      )
      .valueOr(error => sys.error(s"go replay of ${actions.mkString(" ")}: ${error}"))

  private def batchReplayed(variant: Variant, actions: List[String]): Game =
    Replay
      .gameFromUciStrings(
        actions.map(action => Vector(uciStringOf(action))).toVector,
        Player.fromTurnCount(actions.size),
        None,
        variant
      )
      .valueOr(error => sys.error(s"go batch replay of ${actions.mkString(" ")}: ${error}"))

  private def uciStringOf(action: String): String =
    if (action == "pass" || action.startsWith("ss:")) action else s"${Role.defaultRole.forsyth}@${action}"
}
