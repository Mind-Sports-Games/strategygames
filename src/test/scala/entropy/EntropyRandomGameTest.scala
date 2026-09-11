package strategygames.entropy

import scala.util.Random

import strategygames.entropy.format.Forsyth
import strategygames.entropy.format.pgn.Reader

// A whole game of entropy played with real choices, rather than the forced actions of
// EntropyRoundSwapTest: Chaos picks which square to place on, and Order actually slides
// counters about instead of passing every turn.
class EntropyRandomGameTest extends EntropyTest {

  // specs2 puts its own apply/count on Seq, so index by dropping rather than by apply
  private def pick[A](rng: Random, as: Seq[A]): A = as.drop(rng.nextInt(as.size)).head

  // One action, chosen at random from what the situation genuinely allows. Anything the
  // engine then refuses is a bug in the engine, not in the choice, so it fails loudly.
  private def act(rng: Random, g: Game): Game = {
    val s = g.situation
    if (s.mustDraw) {
      // a blind draw: uniform over what is left in the bag, so common colours come up more
      val bag: List[Role]  = s.board.variant.bag(s.board)
      val role: Role       = pick[Role](rng, bag)
      g.apply(s.drawCounter(role).getOrElse(sys.error(s"refused a legal draw of ${role}")))
    } else if (s.isChaos) {
      val drops: List[Drop] = s.dropsAsDrops
      g.apply(pick[Drop](rng, drops))
    } else {
      // specs2's seqToValueChecks hijacks flatten, so flatMap the lists out by hand
      val moves: Vector[Move] = s.moves.values.toVector.flatMap(ms => ms)
      // Order passes now and then, but mostly plays
      if (moves.isEmpty || rng.nextInt(8) == 0)
        g.apply(s.pass().getOrElse(sys.error("refused a legal pass")))
      else g.apply(pick[Move](rng, moves))
    }
  }

  // each round is 49 draws, 49 drops and at most 49 Order turns, so two rounds fit easily
  private def playRandom(seed: Int, limit: Int = 1000): Option[Game] = {
    val rng = new Random(seed)
    Iterator
      .iterate(Game.apply(variant.Entropy))(g => if (g.situation.end) g else act(rng, g))
      .take(limit)
      .find(_.situation.end)
  }

  private def played(seed: Int): Game =
    playRandom(seed).getOrElse(sys.error(s"the random game on seed ${seed} never ended"))

  "a full game of entropy, played with random legal actions" should {

    val finished   = played(20260911)
    val actionStrs = finished.actionStrs

    "play through both rounds and stop" in {
      finished.situation.board.round === 3
    }

    "end on the variant end, never on a timeout" in {
      finished.situation.status === Some(strategygames.Status.VariantEnd)
    }

    "leave the final board full and both bags empty" in {
      finished.situation.board.isFull must beTrue
      finished.situation.board.counterInPocket(P1) must beNone
      finished.situation.board.counterInPocket(P2) must beNone
    }

    "score both players, each having been Order for one round" in {
      val score = finished.situation.board.history.score
      score.p1 must be_>(0)
      score.p2 must be_>(0)
    }

    "decide the winner on the score alone" in {
      val score = finished.situation.board.history.score
      finished.situation.winner === (
        if (score.p1 == score.p2) None
        else Some(if (score.p1 > score.p2) P1 else P2)
      )
    }

    // specs2 puts its own count and flatten on Seq, so go the long way round
    val strs: List[String] = actionStrs.toList.flatMap(v => v)

    "place all 98 counters across the two rounds" in {
      strs.filter((a: String) => a.startsWith("draw-")).size === 98
      strs.filter((a: String) => a.contains('@')).size === 98
    }

    "have Order actually slide counters, not merely pass" in {
      val slides =
        strs.filter((a: String) => a.length == 4 && !a.contains('@') && a != "pass")
      slides.size must be_>(0)
    }

    "replay its own action strings back to the same position" in {
      val replayed = Replay(actionStrs, None, variant.Entropy)

      replayed.toOption must beSome[Reader.Result].like { case Reader.Result.Complete(replay) =>
        Forsyth.>>(replay.state) === Forsyth.>>(finished)
      }
    }
  }

  "many random games" should {

    "all reach a scored finish without the engine refusing a legal action" in {
      val finishes = (1 to 20).map(played)

      finishes.forall(_.situation.board.round == 3) must beTrue
      finishes.forall(_.situation.status == Some(strategygames.Status.VariantEnd)) must beTrue
      finishes.forall(g => g.situation.board.history.score.p1 > 0) must beTrue
      finishes.forall(g => g.situation.board.history.score.p2 > 0) must beTrue
    }
  }
}
