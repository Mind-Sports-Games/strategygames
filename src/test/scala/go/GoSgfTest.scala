package strategygames.go

import org.specs2.mutable.Specification

import strategygames.go.format.{ FEN, Sgf }
import strategygames.go.variant.{ Go19x19, Go9x9 }

class GoSgfTest extends Specification with GoRulesTestSupport {

  private val nineByNineWithOneStone =
    FEN(s"9/9/9/9/4S4/9/9/9/9${pocket} w - 0 0 0 0 7.5 0 1")

  "rendering a go game as sgf" should {

    "place the stones of an initial fen the variant can hold" in {
      Sgf.actionStrsToOutput(Go9x9, Vector(Vector("s@a1")), Some(nineByNineWithOneStone)) ===
        ";B[ee]\n;W[ai]"
    }

    "refuse an initial fen whose board is a different size from the variant" in {
      Sgf.actionStrsToOutput(Go19x19, Vector(Vector("s@a1")), Some(nineByNineWithOneStone)) must
        throwAn[Exception]
    }
  }
}
