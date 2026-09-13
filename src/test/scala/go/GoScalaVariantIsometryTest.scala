package strategygames.go

import strategygames.format.{ FEN => StratFen, Forsyth => StratForsyth, Uci => StratUci }
import strategygames.variant.{ Variant => StratVariant }
import variant.Go9x9

class GoScalaVariantIsometryTest extends strategygames.chess.ChessTest {

  "Test Every move of a scala go game can be loaded from fen" in {
    val gameFamily   = Go9x9.gameFamily
    val lib          = gameFamily.gameLogic
    val stratVariant = StratVariant(lib, Go9x9.key).get

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
    ).toOption must beSome.like { case gameData =>
      val fen1 = StratForsyth.>>(lib, gameData.game)
      val fen2 = StratForsyth.>>(lib, gameData.fenGame)
      fen1 === fen2
    }
  }
}
