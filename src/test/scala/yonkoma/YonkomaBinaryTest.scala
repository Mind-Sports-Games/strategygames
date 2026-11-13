package strategygames.yonkoma

import strategygames.yonkoma.format.pgn.Binary

class YonkomaBinaryTest extends YonkomaTest {

  "binary encoding" should {

    "round-trip moves and passes" in {
      val actions = Vector(Vector("a1e5"), Vector("a10e6"), Vector("pass"), Vector("j10f6"), Vector("l12a1"))

      val bytes = Binary.writeActionStrs(actions)
      bytes.isSuccess must beTrue
      // two bytes an action
      bytes.get.size === 10

      val back = Binary.readActionStrs(bytes.get.toList)
      back.isSuccess must beTrue
      back.get === actions
    }

    "write the squares of ranks 11 and 12, whose index needs the whole byte" in {
      val actions = Vector(Vector("a11l12"), Vector("l12a11"))
      val back    = Binary.readActionStrs(Binary.writeActionStrs(actions).get.toList)
      back.get === actions
    }

    "write a move as its two square indexes and a pass as two bytes that are no square" in {
      Binary.writeMove("pass").get === List(0xff.toByte, 0xff.toByte)
      Binary.writeMove("a1e5").get === List(Pos.A1.index.toByte, Pos.E5.index.toByte)
      Binary.writeMove("l12a11").get === List(143.toByte, 120.toByte)
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

    "refuse a square off the board, half a pass, or a cut-off action" in {
      Binary.readActionStrs(List(144.toByte, 1.toByte)).isFailure must beTrue
      Binary.readActionStrs(List(1.toByte, 200.toByte)).isFailure must beTrue
      Binary.readActionStrs(List(0xff.toByte, 1.toByte)).isFailure must beTrue
      Binary.readActionStrs(List(1.toByte, 0xff.toByte)).isFailure must beTrue
      Binary.readActionStrs(List(1.toByte)).isFailure must beTrue
    }
  }
}
