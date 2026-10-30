package strategygames.yonkoma

import scala.io.Source

import strategygames.yonkoma.format.Uci
import strategygames.yonkoma.variant.{ Variant, Yonkoma10x10, Yonkoma12x12 }

// Games played by an independent implementation of the rule sheet (a small Python
// program written from the rules, not from this code). Before every turn the fixture lists
// every legal move the reference found, so the generators are compared position by
// position, not only on the moves that happened to be played.
class YonkomaReferenceGamesTest extends YonkomaTest {

  case class ReferenceGame(
      variant: Variant,
      loser: strategygames.Player,
      turns: List[String],
      legal: List[Set[String]]
  )

  lazy val games: List[ReferenceGame] =
    Source
      .fromResource("yonkoma_reference_games.txt")
      .getLines()
      .filterNot(l => l.startsWith("#") || l.trim.isEmpty)
      .map { line =>
        val parts = line.split('|')
        ReferenceGame(
          variant = if (parts(0) == "12") Yonkoma12x12 else Yonkoma10x10,
          loser = if (parts(1) == "red") P1 else P2,
          turns = parts(2).split(' ').toList,
          legal =
            parts(3).split(';').toList.map(l => if (l.isEmpty) Set.empty[String] else l.split(',').toSet)
        )
      }
      .toList

  private def legalMoveStrings(s: Situation): Set[String] =
    s.moves.values.toList.flatMap(ms => ms).map(_.toUci.uci).toSet

  "the reference games" should {

    "be there" in {
      games.size === 240
      games.count(_.variant == Yonkoma10x10) === 120
      games.count(_.variant == Yonkoma12x12) === 120
    }

    "offer the same legal moves as the reference before every turn, and end the same way" in {
      games.foreach { game =>
        val start = Game.apply(game.variant)
        val end   = game.turns.zip(game.legal).foldLeft(start) { case (g, (turn, legal)) =>
          g.situation.end must beFalse
          legalMoveStrings(g.situation) === legal
          val uci = Uci(turn).getOrElse(sys.error(s"unreadable turn ${turn}"))
          g.apply(uci).toOption.getOrElse(sys.error(s"refused ${turn} in ${game.turns.mkString(" ")}"))._1
        }
        end.situation.end must beTrue
        end.situation.winner === Some(!game.loser)
        end.plies === game.turns.size
      }
      success
    }
  }
}
