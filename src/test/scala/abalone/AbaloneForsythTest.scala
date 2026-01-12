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
      val board = Board(fenFromVariant.pieces, History(score = Score(0, 0)), variant.Abalone)
      situationFromFen.get.board.pieces === board.pieces
      situationFromFen.get.player === P1
    }
  }

  "fen encoding from board after 3 ply" should {
    val fenFromVariant   = variant.Abalone.initialFen
    val situationFromFen = (Forsyth << fenFromVariant).get
    val board            = Board(fenFromVariant.pieces, History(score = Score(0, 0)), variant.Abalone)
    val game             = Game.apply(board.variant)
    val stratGame        = StratGame.apply(StratGameLogic(7), StratVariant.Variant.default(StratGameLogic(7)))
    val validMoves       = game.board.variant.validMoves(situationFromFen)
    val game2            = game.apply(validMoves(Pos.A1)(1))
    val validMoves2      = game2.board.variant.validMoves(game2.situation)
    val game3            = game2.apply(validMoves2(Pos.E9)(1))
    val validMoves3      = game3.board.variant.validMoves(game3.situation)
    val game4            = game3.apply(validMoves3(Pos.B2)(2))
    game4.board.variant.validMoves(game4.situation)

    "boardPart() describes the updated piecemap" in {
      Forsyth.boardPart(board) === "ss1SS/sssSSS/1ss1SS1/8/9/8/1SS1ss1/SSSsss/SS1ss"
      Forsyth.boardPart(situationFromFen.board) === "ss1SS/sssSSS/1ss1SS1/8/9/8/1SS1ss1/SSSsss/SS1ss"
      Forsyth.boardPart(game.situation.board) === "ss1SS/sssSSS/1ss1SS1/8/9/8/1SS1ss1/SSSsss/SS1ss"
      Forsyth.boardPart(game4.board) === "1s1SS/sssSSS/1ss1SS1/3s4/4S4/3S4/1SS1ss1/S1Ssss/1S1ss"
    }

    "Forsyth.>> from Abalone namespace describes correctly the board, score, player and number of ply" in {
      Forsyth.>>(game4) === FEN("1s1SS/sssSSS/1ss1SS1/3s4/4S4/3S4/1SS1ss1/S1Ssss/1S1ss 0 0 w 3 2")
      Forsyth.>>(situationFromFen) === FEN("ss1SS/sssSSS/1ss1SS1/8/9/8/1SS1ss1/SSSsss/SS1ss 0 0 b 0 1")
      Forsyth.>>(game) === FEN("ss1SS/sssSSS/1ss1SS1/8/9/8/1SS1ss1/SSSsss/SS1ss 0 0 b 0 1")
    }

    "Forsyth.>>(StratGameLogic, StratGame) forwards to the expected one implemented in Abalone namespace" in {
      (StratFormat.Forsyth.>>(StratGameLogic(7), stratGame)).value === FEN(
        "ss1SS/sssSSS/1ss1SS1/8/9/8/1SS1ss1/SSSsss/SS1ss 0 0 b 0 1"
      ).value
    }
  }
}
