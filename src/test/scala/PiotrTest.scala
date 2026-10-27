package strategygames

import org.specs2.mutable.Specification
import org.specs2.specification.core.Fragment

class PiotrTest extends Specification {

  private def keyed(all: List[(String, Char)]): List[(String, Option[Char])] =
    all.map { case (key, piotr) => key -> Some(piotr) }

  private def shared(all: List[(String, Char)]): List[(String, Option[Char])] =
    all.map { case (key, _) => key -> Piotr.lookup.get(key) }

  private val games: List[(String, List[(String, Char)])] = List(
    "chess"        -> chess.Pos.all.map(p => p.key -> p.piotr),
    "fairysf"      -> fairysf.Pos.all.map(p => p.key -> p.piotr),
    "samurai"      -> samurai.Pos.all.map(p => p.key -> p.piotr),
    "togyzkumalak" -> togyzkumalak.Pos.all.map(p => p.key -> p.piotr),
    "go"           -> go.Pos.all.map(p => p.key -> p.piotr),
    "backgammon"   -> backgammon.Pos.all.map(p => p.key -> p.piotr),
    "dameo"        -> dameo.Pos.all.map(p => p.key -> p.piotr),
    "entropy"      -> entropy.Pos.all.map(p => p.key -> p.piotr),
    "fiveinarow"   -> fiveinarow.Pos.all.map(p => p.key -> p.piotr)
  )

  "the shared piotr lookup" should {
    "give each square name one char" in {
      Piotr.lookup.values.toSet.size === Piotr.lookup.size
    }
    Fragment.foreach(games) { case (name, all) =>
      s"encode every $name square as $name always has" in {
        shared(all) === keyed(all)
      }
    }
  }
}
