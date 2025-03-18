package strategygames.abalone

import org.specs2.matcher.ValidatedMatchers

import format.{ FEN, Forsyth }

import strategygames.{
  format => StratFormat,
  variant => StratVariant,
  Game => StratGame,
  GameLogic => StratGameLogic,
  Score
}

class AbaloneForsythTest extends AbaloneTest with ValidatedMatchers {
  "situationFromFen" should {
    val fenFromVariant   = variant.Abalone.initialFen
    val situationFromFen = Forsyth.<<(fenFromVariant)

    "create the situation of Belgian Daisy" in {
      val board = Board(fenFromVariant.pieces(variant.Abalone.boardType), History(score = Score(0, 0)), variant.Abalone)
      situationFromFen.get.board.pieces must_== board.pieces
      situationFromFen.get.player must_== P1
    }
  }

  "fen encoding from board after 3 ply" should {
    val fenFromVariant   = variant.Abalone.initialFen
    val situationFromFen = (Forsyth << fenFromVariant).get
    val board            = Board(fenFromVariant.pieces(variant.Abalone.boardType), History(score = Score(0, 0)), variant.Abalone)
    val game             = Game.apply(board.variant)
    val stratGame        = StratGame.apply(StratGameLogic(7), StratVariant.Variant.default(StratGameLogic(7)))
    val validMoves       = game.board.variant.validMoves(situationFromFen)
    val game2            = game.apply(validMoves(new Pos(0, 0))(1))
    val validMoves2      = game2.board.variant.validMoves(game2.situation)
    val game3            = game2.apply(validMoves2(new Pos(4, 8))(1))
    val validMoves3      = game3.board.variant.validMoves(game3.situation)
    val game4            = game3.apply(validMoves3(new Pos(1, 1))(2))
    game4.board.variant.validMoves(game4.situation)

    "boardPart() describes the updated piecemap" in {
      Forsyth.getFen_board(board) must_== "SS1ss/SSSsss/1SS1ss1/8/9/8/1ss1SS1/sssSSS/ss1SS"
      Forsyth.getFen_board(situationFromFen.board) must_== "SS1ss/SSSsss/1SS1ss1/8/9/8/1ss1SS1/sssSSS/ss1SS"
      Forsyth.getFen_board(game.situation.board) must_== "SS1ss/SSSsss/1SS1ss1/8/9/8/1ss1SS1/sssSSS/ss1SS"
      Forsyth.getFen_board(game4.board) must_== "1S1ss/S1Ssss/1SS1ss1/3S4/4S4/3s4/1ss1SS1/sssSSS/1s1SS"
    }

    "Forsyth.>> from Abalone namespace describes correctly the board, score, player and number of ply" in {
      Forsyth.>>(game4) must_== FEN("1S1ss/S1Ssss/1SS1ss1/3S4/4S4/3s4/1ss1SS1/sssSSS/1s1SS 0 0 w 3 2")
      Forsyth.>>(situationFromFen) must_== FEN("SS1ss/SSSsss/1SS1ss1/8/9/8/1ss1SS1/sssSSS/ss1SS 0 0 b 0 1")
      Forsyth.>>(game) must_== FEN("SS1ss/SSSsss/1SS1ss1/8/9/8/1ss1SS1/sssSSS/ss1SS 0 0 b 0 1")
    }

    "Forsyth.>>(StratGameLogic, StratGame) forwards to the expected one implemented in Abalone namespace" in {
      StratFormat.Forsyth.>>(StratGameLogic(7), stratGame).value must_== FEN(
        "SS1ss/SSSsss/1SS1ss1/8/9/8/1ss1SS1/sssSSS/ss1SS 0 0 b 0 1"
      ).value
    }
  }
}