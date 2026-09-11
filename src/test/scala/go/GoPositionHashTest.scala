package strategygames.go

import org.specs2.mutable.Specification

import strategygames.Player
import strategygames.go.variant.{ Go13x13, Go19x19, Go9x9, Variant }

class GoPositionHashTest extends Specification with GoRulesTestSupport {

  private val gameEndingInASettlement = List("d4", "f4", "e6", "pass", "pass", "ss:")

  private val scriptedGames: List[(Variant, List[String])] = List(
    Go9x9   -> List("g5", "f5", "f4", "e4", "f6", "d5", "h5", "e6", "e5", "a1", "c9", "f5"),
    Go9x9   -> List("d1", "a2", "c2", "b2", "a1", "pass", "b1", "c1", "a1", "pass"),
    Go9x9   -> gameEndingInASettlement,
    Go13x13 -> List("a3", "a2", "b2", "b1", "c1", "e5", "a1"),
    Go19x19 -> List("c1", "c4", "b2", "b3", "d2", "d3", "c3", "c2", "pass", "c3")
  )

  private val threeStones = List(P1 -> "d4", P2 -> "f4", P1 -> "d6")

  private val threeStonesHashOfRecord = 0x429d389bed64743cL

  "the empty board" should {
    "hash to the turn mask of the player to move" in {
      (Hash.positionHash(Board.init(Go9x9), P1) === Hash.turnMask(P1)) and
        (Hash.positionHash(Board.init(Go9x9), P2) === Hash.turnMask(P2))
    }
    "hash to a different value for each player to move" in {
      Hash.positionHash(Board.init(Go9x9), P1) !== Hash.positionHash(Board.init(Go9x9), P2)
    }
    "start its history at the position its starting player moves from" in {
      (Board.init(Go9x9).history.positionCount === 1) and
        (Board.init(Go9x9).history.currentPosition === Some(Hash.positionHash(Board.init(Go9x9), P1)))
    }
  }

  "the zobrist table the position hash is drawn from" should {
    "hash three stones with P2 to move to the recorded value" in {
      Hash.positionHash(boardOf(Go9x9, threeStones), P2) === threeStonesHashOfRecord
    }
    "hash the same three stones with P1 to move to that value under the turn mask" in {
      Hash.positionHash(boardOf(Go9x9, threeStones), P1) === (threeStonesHashOfRecord ^ Hash.turnMask(P1))
    }
    "hash a played game reaching those stones to the very same value" in {
      Hash.positionHash(playing(Go9x9, List("d4", "f4", "d6")).board, P2) === threeStonesHashOfRecord
    }
  }

  "two move orders reaching the same stones with the same player to move" should {
    val oneOrder   = playing(Go9x9, List("d4", "f4", "d6", "f6"))
    val otherOrder = playing(Go9x9, List("d6", "f6", "d4", "f4"))
    "recompute to the same hash" in {
      oneOrder.situation.positionHash === otherOrder.situation.positionHash
    }
    "have recorded the same hash while they were played" in {
      oneOrder.situation.history.currentPosition === otherOrder.situation.history.currentPosition
    }
  }

  "the same stones with the other player to move" should {
    "hash to a different value" in {
      val played = playing(Go9x9, List("d4", "f4", "d6"))
      played.situation.positionHash !== Hash.positionHash(played.board, !played.situation.player)
    }
    "not be reported as having occurred" in {
      val played = playing(Go9x9, List("d4", "f4", "d6"))
      played.situation.history.hasOccurred(
        Hash.positionHash(played.board, !played.situation.player)
      ) === false
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
    "still move the position identity on, because the player to move changed" in {
      afterPass.situation.positionHash !== beforePass.situation.positionHash
    }
  }

  "a settlement" should {
    "restart the history from the settled position" in {
      playing(Go9x9, gameEndingInASettlement).situation.history.positionCount === 1
    }
    "restart it too when the game is rebuilt from its uci" in {
      replayedFromUci(Go9x9, gameEndingInASettlement).situation.history.positionCount === 1
    }
  }

  "a fen load" should {
    "start the history at the loaded position" in {
      val loaded = situationFrom(fenOf(playing(Go9x9, List("d4", "f4"))))
      (loaded.history.positionCount === 1) and
        (loaded.history.currentPosition === Some(loaded.positionHash))
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
        playing(Go9x9, List("d4", "f4", "d6")).situation.positionHash
      ) === false
    }
  }

  "a game whose hash is maintained one action at a time" should {
    "record, at every placing ply, a hash equal to a recompute of its board and player" in {
      scriptedGames.flatMap { case (variant, actions) =>
        recordedHashMismatchesIn(variant, actions)
      } must beEmpty
    }
    "sweep every ply of every script, rather than pass over an empty list" in {
      scriptedGames.map { case (variant, actions) => situationsOf(variant, actions).size }.sum ===
        scriptedGames.map { case (_, actions) => actions.size + 1 }.sum
    }
  }

  "the replay path" should {
    "record the same position history the played game records" in {
      val actions = List("d4", "f4", "d6")
      (replayedFromUci(Go9x9, actions).situation.history.positionCount === actions.size + 1) and
        (playing(Go9x9, actions).situation.history.positionCount === actions.size + 1) and
        (replayedFromUci(Go9x9, actions).situation.history.currentPosition ===
          playing(Go9x9, actions).situation.history.currentPosition)
    }
  }

  private def boardOf(variant: Variant, stones: List[(Player, String)]): Board =
    Board(stones.map { case (player, key) => pointAt(key) -> Piece(player, Role.defaultRole) }, variant)

  private def recordedHashMismatchesIn(variant: Variant, actions: List[String]): List[String] =
    situationsOf(variant, actions).zipWithIndex
      .filter { case (situation, _) => situation.board.consecutivePasses == 0 }
      .flatMap { case (situation, ply) => mismatchAt(actions.mkString(" "), ply, situation) }

  private def mismatchAt(name: String, ply: Int, situation: Situation): Option[String] = {
    val recorded   = situation.history.currentPosition
    val recomputed = situation.positionHash
    if (recorded == Some(recomputed)) None
    else Some(s"${name} ply ${ply}: recorded ${recorded}, recomputed ${recomputed}")
  }

  private def situationsOf(variant: Variant, actions: List[String]): List[Situation] = {
    val (init, plies, error) = Replay.gameWithUciWhileValid(
      actions.map(action => Vector(uciStringOf(action))).toVector,
      Player.P1,
      Player.fromTurnCount(actions.size),
      variant.initialFen,
      variant
    )
    error.foreach(message => sys.error(s"go replay of ${actions.mkString(" ")}: ${message}"))
    init.situation :: plies.map(_._1.situation)
  }

  private def replayedFromUci(variant: Variant, actions: List[String]): Game =
    Replay
      .gameFromUciStrings(
        actions.map(action => Vector(uciStringOf(action))).toVector,
        Player.fromTurnCount(actions.size),
        None,
        variant
      )
      .valueOr(error => sys.error(s"go replay of ${actions.mkString(" ")}: ${error}"))

  private def uciStringOf(action: String): String =
    if (action == "pass" || action.startsWith("ss:")) action else s"${Role.defaultRole.forsyth}@${action}"
}
