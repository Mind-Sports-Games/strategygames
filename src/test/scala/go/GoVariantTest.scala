package strategygames.go

import strategygames.format.{ FEN => StratFen, Forsyth => StratForsyth, Uci => StratUci }
import strategygames.variant.{ Variant => StratVariant }
import variant.Go9x9

class Go9x9VariantTestIsometry extends strategygames.chess.ChessTest {
  "Test Every move can be loaded from fen" in {
    val gameFamily   = Go9x9.gameFamily
    val lib          = gameFamily.gameLogic
    val stratVariant = StratVariant(lib, Go9x9.key).get

    // captures small full game with deadstones
    _testEveryMoveLoadFenIsometry(lib, StratFen(lib, Go9x9.initialFen.value), stratVariant)(
      List(
        "s@g3",
        "s@c7",
        "s@f2",
        "s@e5",
        "s@e1",
        "s@d4",
        "s@h4",
        "s@i1",
        "s@i5",
        "s@c5",
        "s@d5",
        "s@d6",
        "pass",
        "pass",
        "ss:i1"
      ).map(uciStr => StratUci(lib, gameFamily, uciStr).get)
    ) must beValid.like(gameData => {
      val fen1 = StratForsyth.>>(lib, gameData.game)
      val fen2 = StratForsyth.>>(lib, gameData.fenGame)
      fen1 must_== fen2
    })
  }

  "Test Every move can be loaded from fen" in {
    val gameFamily   = Go9x9.gameFamily
    val lib          = gameFamily.gameLogic
    val stratVariant = StratVariant(lib, Go9x9.key).get

    // go with 3 kos
    _testEveryMoveLoadFenIsometry(lib, StratFen(lib, Go9x9.initialFen.value), stratVariant)(
      List(
        "s@b8",
        "s@b7",
        "s@c9",
        "s@c6",
        "s@d8",
        "s@d7",
        "s@f8",
        "s@f7",
        "s@g9",
        "s@g6",
        "s@h8",
        "s@h7",
        "s@b2",
        "s@b3",
        "s@c1",
        "s@c4",
        "s@d2",
        "s@d3",
        "s@f2",
        "s@f3",
        "s@g1",
        "s@g4",
        "s@h2",
        "s@h3",
        "s@c7",
        "s@c2",
        "s@g3",
        "s@g8",
        "s@g7",
        "s@c8",
        "s@c3",
        "s@g2",
        "s@c7",
        "s@c2",
        "s@g3"
      ).map(uciStr => StratUci(lib, gameFamily, uciStr).get)
    ) must beValid.like(gameData => {
      val fen1 = StratForsyth.>>(lib, gameData.game)
      val fen2 = StratForsyth.>>(lib, gameData.fenGame)
      fen1 must_== fen2
    })
  }
}
