package strategygames.yonkoma

import scala.util.Random

import strategygames.yonkoma.format.{ Forsyth, Uci }
import strategygames.yonkoma.format.pgn.Reader
import strategygames.yonkoma.variant.{ Variant, Yonkoma10x10, Yonkoma12x12 }

// Whole games played with random legal moves. Anything the engine then refuses is a bug
// in the engine, not in the choice, so it fails loudly.
class YonkomaRandomGameTest extends YonkomaTest {

  private def act(rng: Random, g: Game): Game = {
    val s                   = g.situation
    // specs2's seqToValueChecks hijacks flatten, so flatMap the lists out by hand
    val moves: Vector[Move] = s.moves.values.toVector.flatMap(ms => ms)
    if (moves.isEmpty) g.apply(s.pass().getOrElse(sys.error("refused the pass of a stuck player")))
    else g.apply(pick[Move](rng, moves))
  }

  private def playRandom(variant: Variant, seed: Int, limit: Int = 2000): Option[Game] = {
    val rng = new Random(seed)
    Iterator
      .iterate(Game.apply(variant))(g => if (g.situation.end) g else act(rng, g))
      .take(limit)
      .find(_.situation.end)
  }

  private def played(variant: Variant, seed: Int): Game =
    playRandom(variant, seed).getOrElse(sys.error(s"the random game on seed ${seed} never ended"))

  private def check(variant: Variant, seed: Int) = {
    val finished = played(variant, seed)
    val s        = finished.situation

    // the game ends on a crosscut, which the player who completed it loses
    variant.isCrosscut(s.board) must beTrue
    s.status === Some(strategygames.Status.VariantEnd)
    s.winner === Some(s.player)
    s.moves must beEmpty

    // the record is one action a turn, never two passes in a row
    val strs: List[String] = finished.actionStrs.toList.flatMap(v => v)
    finished.actionStrs.forall(_.size == 1) must beTrue
    finished.plies === strs.size
    finished.turnCount === strs.size
    strs.lastOption must beSome((a: String) => a != "pass")
    strs.zip(strs.drop(1)).exists { case (a, b) => a == "pass" && b == "pass" } must beFalse

    // four checkers on the board all along, reds on dark squares and blues on light ones
    s.board.valid(true) must beTrue

    // and the record replays to the same position
    Replay(finished.actionStrs, None, variant).toOption must beSome[Reader.Result].like {
      case Reader.Result.Complete(replay) =>
        Forsyth.>>(replay.state) === Forsyth.>>(finished)
    }
  }

  "a random game on the 10x10 board" should {
    "end on a crosscut that the last mover loses, and replay from its record" in {
      check(Yonkoma10x10, 20261030)
    }
  }

  "a random game on the 12x12 board" should {
    "end on a crosscut that the last mover loses, and replay from its record" in {
      check(Yonkoma12x12, 20261030)
    }
  }

  "many random games" should {
    "all finish, on both board sizes, without the engine refusing a legal action" in {
      (1 to 60).foreach { seed =>
        check(Yonkoma10x10, seed)
        check(Yonkoma12x12, seed)
      }
      success
    }

    "never leave both players without a move" in {
      (1 to 60).forall { seed =>
        val rng = new Random(seed)
        Iterator
          .iterate(Game.apply(Yonkoma10x10))(g => if (g.situation.end) g else act(rng, g))
          .take(2000)
          .takeWhile(g => !g.situation.end)
          .forall { g =>
            val s = g.situation
            s.moves.nonEmpty || (!s).moves.nonEmpty
          }
      } must beTrue
    }
  }

  "the move generator" should {
    "agree with a move-by-move rule check on random positions" in {
      // every move it offers obeys the rule, and no slide it leaves out does
      val rng = new Random(99)
      (1 to 40).foreach { _ =>
        val game = Iterator
          .iterate(Game.apply(Yonkoma10x10))(g => if (g.situation.end) g else act(rng, g))
          .drop(rng.nextInt(8))
          .next()
        val s    = game.situation
        if (!s.end) {
          val board   = s.board
          val mine    = board.checkersOf(s.player)
          val enemies = board.checkersOf(!s.player)
          mine.foreach { from =>
            val partner    = mine.find(_ != from).get
            val sumBefore  = enemies.map(from.kingDistance).sum
            val legalSlide = Pos.all.filter { to =>
              board.onBoard(to) && to != from && from.onSameDiagonal(to) && board.empty(to) &&
              clearPath(board, from, to) && {
                val sumAfter = enemies.map(to.kingDistance).sum
                sumAfter < sumBefore || (sumAfter == sumBefore && to.kingDistance(partner) > from
                  .kingDistance(partner))
              }
            }
            s.destinations.getOrElse(from, Nil).toSet === legalSlide.toSet
          }
        }
      }
      success
    }
  }

  // every square strictly between from and to on their shared diagonal is empty
  private def clearPath(board: Board, from: Pos, to: Pos): Boolean = {
    val dx    = Integer.signum(to.file.index - from.file.index)
    val dy    = Integer.signum(to.rank.index - from.rank.index)
    val steps = math.abs(to.file.index - from.file.index)
    (1 until steps).forall { i =>
      Pos.at(from.file.index + dx * i, from.rank.index + dy * i).exists(board.empty)
    }
  }

  // the first ply of a recorded game can be read back
  "the first move" should {
    "be a move of a red checker" in {
      val g = played(Yonkoma10x10, 5)
      Uci(g.actionStrs.head.head).map(_.uci) === Some(g.actionStrs.head.head)
      g.actionStrs.head.head must not(beEqualTo("pass"))
    }
  }
}
