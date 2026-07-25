package strategygames.go.engine

import org.specs2.mutable.Specification

import strategygames.go.{ Api, Pos }
import strategygames.go.variant.{ Go13x13, Go19x19, Go9x9 }

class EngineMoveEncodingTest extends Specification {

  import GoEngineTestSupport._

  private val variantsBySize = List((9, Go9x9), (13, Go13x13), (19, Go19x19))

  "engine move encoding" should {
    "match Api.uciToMove for every point on every board size" in {
      forall(variantsBySize) { case (size, variant) =>
        forall(boardKeys(size)) { key =>
          engineMove(size, key) === Api.uciToMove(s"S@${key}", variant)
        }
      }
    }
    "match Api.passMove for every board size" in {
      forall(variantsBySize) { case (size, variant) =>
        GoState.initial(size).passMove === Api.passMove(variant)
      }
    }
    "round-trip every 19x19 key through Pos including a19 and s19" in {
      (Pos.fromKey("a19") must beSome) and
        (Pos.fromKey("s19") must beSome) and
        forall(boardKeys(19)) { key =>
          Api.moveToPos(engineMove(19, key), Go19x19) === Pos.fromKey(key)
        }
    }
  }
}
