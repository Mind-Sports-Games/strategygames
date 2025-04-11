package strategygames.abalone

import strategygames.abalone.format.{FEN, Forsyth}
import strategygames.abalone.variant.Abalone
import strategygames.{Game => StratGame, GameLogic => StratGameLogic, format => StratFormat, variant => StratVariant}

class AbaloneForsythTest extends AbaloneTest {
  "sitFromFen" should {
    val fenFromVariant = Abalone.initialFen
    val sitFromFen     = Forsyth.<<(fenFromVariant)

    "create the situation of Belgian Daisy" in {
      val board = Board(fenFromVariant.pieces(Abalone), History(), Abalone)
      sitFromFen.get.board.pieces must_== board.pieces
      sitFromFen.get.player must_== P1
    }
  }

  "fen encoding from board after 3 plies" should {
    val fenFromVariant = Abalone.initialFen
    val sitFromFen     = (Forsyth << fenFromVariant).get
    val board          = Board(fenFromVariant.pieces(Abalone), History(), Abalone)
    val stratGame      = StratGame.apply(StratGameLogic(7), StratVariant.Variant.default(StratGameLogic(7)))
    val game           = Game(sitFromFen)
    val game2          = next(game, 0, 0, 3, 3)
    val game3          = next(game2, 4, 8, 4, 5)
    val game4          = next(game3, 1, 1, 4, 4)

    "getFen_board describes the updated piecemap" in {
      Forsyth.getFen_board(board) must_== "ss1SS/sssSSS/1ss1SS1/8/9/8/1SS1ss1/SSSsss/SS1ss"
      Forsyth.getFen_board(sitFromFen.board) must_== "ss1SS/sssSSS/1ss1SS1/8/9/8/1SS1ss1/SSSsss/SS1ss"
      Forsyth.getFen_board(game.situation.board) must_== "ss1SS/sssSSS/1ss1SS1/8/9/8/1SS1ss1/SSSsss/SS1ss"
      Forsyth.getFen_board(game4.board) must_== "1s1SS/sssSSS/1ss1SS1/3s4/4S4/3S4/1SS1ss1/S1Ssss/1S1ss"
    }

    "Forsyth.>> from Abalone namespace describes correctly the board, score, player and number of plies" in {
      Forsyth.>>(sitFromFen) must_== FEN("ss1SS/sssSSS/1ss1SS1/8/9/8/1SS1ss1/SSSsss/SS1ss 0 0 b 0 1")
      Forsyth.>>(game) must_== FEN("ss1SS/sssSSS/1ss1SS1/8/9/8/1SS1ss1/SSSsss/SS1ss 0 0 b 0 1")
      Forsyth.>>(game4) must_== FEN("1s1SS/sssSSS/1ss1SS1/3s4/4S4/3S4/1SS1ss1/S1Ssss/1S1ss 0 0 w 3 2")
    }

    "Forsyth.>>(StratGameLogic, StratGame) forwards to the expected one implemented in Abalone namespace" in {
      StratFormat.Forsyth.>>(StratGameLogic(7), stratGame).value must_== FEN(
        "ss1SS/sssSSS/1ss1SS1/8/9/8/1SS1ss1/SSSsss/SS1ss 0 0 b 0 1"
      ).value
    }
  }
}
