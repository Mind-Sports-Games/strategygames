package strategygames.fiveinarow

import scala.util.Random

import org.specs2.mutable.Specification

import strategygames.Status
import strategygames.fiveinarow.format.Forsyth
import strategygames.fiveinarow.format.pgn.Binary
import strategygames.fiveinarow.variant.Gomoku

class FiveInARowRandomGameTest extends Specification with FiveInARowTestSupport {

  // specs2 puts its own apply on Seq, so index by dropping rather than by apply
  private def pick[A](rng: Random, as: Seq[A]): A = as.drop(rng.nextInt(as.size)).head

  // drops cluster near the centre so that fives actually get made
  private def act(rng: Random, g: Game): Game = {
    val s      = g.situation
    val drops  = s.dropsAsDrops
    val nearby = drops.filter(d => (d.pos.file.index - 7).abs <= 3 && (d.pos.rank.index - 7).abs <= 3)
    rng.nextInt(6) match {
      case 0 if s.canSwap  => g.apply(s.swap().getOrElse(sys.error("refused a legal swap")))
      case 1 if s.canSwap2 => g.apply(s.swap2().getOrElse(sys.error("refused a legal swap2")))
      case _               => g.apply(pick(rng, if (nearby.nonEmpty) nearby else drops))
    }
  }

  private def played(seed: Int): Game = {
    val rng = new Random(seed)
    Iterator
      .iterate(start)(g => if (g.situation.end) g else act(rng, g))
      .take(300)
      .find(_.situation.end)
      .getOrElse(sys.error(s"the random game on seed ${seed} never ended"))
  }

  "random games of gomoku" should {

    val games = (1 to 40).map(seed => played(20261008 + seed))

    "always end on the variant end" in {
      games.forall(_.situation.status.contains(Status.VariantEnd)) must beTrue
    }

    "give the win to the seat holding the colour that made five" in {
      games.forall { g =>
        val b = g.situation.board
        g.situation.winner == b.variant.colourWithLine(b).map(b.seatOf)
      } must beTrue
    }

    "include swaps and swap2s among them" in {
      val all = games.flatMap(_.actionStrs.flatMap(t => t))
      all must contain("swap")
      all must contain("swap2")
    }

    "replay from the record to the same position" in {
      games.forall { g =>
        Replay
          .gameFromUciStrings(g.actionStrs.toList.flatMap(t => t), None, Gomoku)
          .toOption
          .map(r => Forsyth.>>(r).value)
          .contains(Forsyth.>>(g).value)
      } must beTrue
    }

    "regroup the record into the same turns from its binary form" in {
      games.forall { g =>
        Binary
          .writeActionStrs(g.actionStrs)
          .toOption
          .flatMap(bytes => Binary.readActionStrs(bytes.toList).toOption)
          .exists(_ == g.actionStrs)
      } must beTrue
    }
  }
}
