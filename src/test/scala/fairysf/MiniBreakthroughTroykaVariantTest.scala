package strategygames.fairysf

class MiniBreakthroughTroykaVariantTest extends FairySFTest {

  "MiniBreakthroughTroyka" should {

    "not have winner from start position" in {
      val position = Api.positionFromVariant(
        variant.MiniBreakthroughTroyka
      ) // @TODO: do not use API in test not related to API
      val position2 = position.makeMoves(
        List("a2b3", "e4d3", "b3a4", "d3e2")
      )

      position2.gameEnd must_== false
      println(position2.pieceMap)

      val pos3 = position2.makeMoves(List("a4b5"))
      println(pos3.gameResult)
      println(pos3.isImmediateGameEnd)

      pos3.gameEnd must_== true

      // val pos4 = pos3.makeMoves(List("e2d1")) // this is supposed to not be a possible move
      // pos4.gameEnd must_== true
    }
  }
}
