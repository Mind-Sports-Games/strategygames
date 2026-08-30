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
    "restart it too when the game is rebuilt from its uci" in {
      replayedFromUci(Go9x9, gameEndingInASettlement).situation.history.positionCount === 1
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

  "a game whose hash is maintained one action at a time" should {
    "carry, at every ply, a recorded hash equal to a full recompute of its own board" in {
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
    situationsOf(variant, actions).zipWithIndex.flatMap { case (situation, ply) =>
      mismatchAt(actions.mkString(" "), ply, situation)
    }

  private def mismatchAt(name: String, ply: Int, situation: Situation): Option[String] = {
    val recorded   = situation.history.currentPosition
    val recomputed = Hash.positionHash(situation.board)
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
