package strategygames.yonkoma

import strategygames.yonkoma.format.pgn.Binary

class YonkomaBinaryTest extends YonkomaTest {

  "binary encoding" should {

    "round-trip moves and passes" in {
      val actions = Vector(Vector("a1e5"), Vector("a10e6"), Vector("pass"), Vector("j10f6"), Vector("l12a1"))

      val bytes = Binary.writeActionStrs(actions)
      bytes.isSuccess must beTrue
      // three bytes a move, one byte a pass
      bytes.get.size === 13

      val back = Binary.readActionStrs(bytes.get.toList)
      back.isSuccess must beTrue
      back.get === actions
    }

    "write the squares of ranks 11 and 12, whose index needs the whole byte" in {
      val actions = Vector(Vector("a11l12"), Vector("l12a11"))
      val back    = Binary.readActionStrs(Binary.writeActionStrs(actions).get.toList)
      back.get === actions
    }

    "put the action type in the two high bits of the first byte, as go and entropy do" in {
      Binary.writeMove("pass").get === List(0x00.toByte)
      Binary.writeMove("a1e5").get === List(0x40.toByte, Pos.A1.index.toByte, Pos.E5.index.toByte)
      Binary.writeMove("l12a11").get === List(0x40.toByte, 143.toByte, 120.toByte)
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

    "refuse an action type it does not know, a square off the board, or a cut-off move" in {
      Binary.readActionStrs(List(0x80.toByte)).isFailure must beTrue
      Binary.readActionStrs(List(0xc0.toByte, 1.toByte, 2.toByte)).isFailure must beTrue
      Binary.readActionStrs(List(0x40.toByte, 200.toByte, 1.toByte)).isFailure must beTrue
      Binary.readActionStrs(List(0x40.toByte, 1.toByte)).isFailure must beTrue
    }
  }
}
