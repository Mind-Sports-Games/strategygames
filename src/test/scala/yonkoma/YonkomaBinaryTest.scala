package strategygames.yonkoma

import strategygames.yonkoma.format.pgn.Binary

class YonkomaBinaryTest extends YonkomaTest {

  "binary encoding" should {

    "round-trip moves and passes" in {
      val actions = Vector(Vector("a1e5"), Vector("a10e6"), Vector("pass"), Vector("j10f6"), Vector("l12a1"))

      val bytes = Binary.writeActionStrs(actions)
      bytes.isSuccess must beTrue
      // two bytes a move, one byte a pass
      bytes.get.size === 9

      val back = Binary.readActionStrs(bytes.get.toList)
      back.isSuccess must beTrue
      back.get === actions
    }

    "write the squares of ranks 11 and 12, whose index needs the whole byte" in {
      val actions = Vector(Vector("a11l12"), Vector("l12a11"))
      val back    = Binary.readActionStrs(Binary.writeActionStrs(actions).get.toList)
      back.get === actions
    }

    "stop after the requested number of plies" in {
      val actions = Vector(Vector("a1e5"), Vector("pass"), Vector("j10f6"))
      val bytes   = Binary.writeActionStrs(actions).get.toList
      Binary.readActionStrs(bytes, 2).get === actions.take(2)
      Binary.readActionStrs(bytes, 0).get === Vector.empty
    }

    "refuse an action it cannot write" in {
      Binary.writeMove("a1@b2").isFailure must beTrue
      Binary.writeMove("m1a1").isFailure must beTrue
    }

    "refuse a byte that is neither a square nor a pass" in {
      Binary.readActionStrs(List(200.toByte, 1.toByte)).isFailure must beTrue
      Binary.readActionStrs(List(1.toByte)).isFailure must beTrue
    }
  }
}
