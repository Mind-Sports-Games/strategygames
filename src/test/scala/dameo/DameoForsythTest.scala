package strategygames.dameo

import org.specs2.matcher.ValidatedMatchers

import format.{ FEN, Forsyth }

// import strategygames.{
//   format => StratFormat,
//   variant => StratVariant,
//   Game => StratGame,
//   GameLogic => StratGameLogic,
//   Score
// }

class DameoForsythTest extends DameoTest with ValidatedMatchers {

  "situationFromFen" should {
    val fenFromVariant   = variant.Dameo.initialFen
    val situationFromFen = Forsyth.<<(fenFromVariant)

    "create the starting setup Situation" in {
      val board = Board(fenFromVariant.pieces, variant.Dameo)
      situationFromFen.get.board.pieces must_== board.pieces
      situationFromFen.get.player must_== P1
    }

    "export the starting Situation correctly" in {
      Forsyth.>>(situationFromFen.get).value must_==variant.Dameo.initialFen.value
    }
  }

  "situationPlusFromFen" should {
    val fenFromVariant   = variant.Dameo.initialFen
    val situationPlusFromFen = Forsyth.<<<(fenFromVariant)

    "create the starting setup as SituationPlus" in {
      val board = Board(fenFromVariant.pieces, variant.Dameo)
      situationPlusFromFen.get.situation.board.pieces must_== board.pieces
      situationPlusFromFen.get.situation.player must_== P1

      /* TODO check fullturncount, turncount and plies after some moves*/
    }
  }

  "fen with ghosts and kings" should {
    val fen = FEN("W:Wa5.k,b2.k,c4:Ba8.g,e8.p,f6,f7.k:H0:F1")

    "count the ghosts" in {
      Forsyth.countGhosts(fen) must_== 2
    }

    "count the kings" in {
      Forsyth.countKings(fen) must_== 3
    }

    "export the Situation correctly" in {
      val situation = Forsyth.<<(fen)
      Forsyth.>>(situation.get).value must_==fen.value
    }
  }

  // "fen encoding from board after 3 ply" should {
  //   val fenFromVariant   = variant.Dameo.initialFen
  //   val situationFromFen = (Forsyth << fenFromVariant).get
  //   val board            = Board(fenFromVariant.pieces, History(score = Score(0, 0)), variant.Dameo)
  //   val game             = Game.apply(board.variant)
  //   val stratGame        = StratGame.apply(StratGameLogic(7), StratVariant.Variant.default(StratGameLogic(7)))
  //   val validMoves       = game.board.variant.validMoves(situationFromFen)
  //   val game2            = game.apply(validMoves(Pos.A1)(1))
  //   val validMoves2      = game2.board.variant.validMoves(game2.situation)
  //   val game3            = game2.apply(validMoves2(Pos.E9)(1))
  //   val validMoves3      = game3.board.variant.validMoves(game3.situation)
  //   val game4            = game3.apply(validMoves3(Pos.B2)(2))
  //   game4.board.variant.validMoves(game4.situation)

  //   "Forsyth.>> from Dameo namespace describes correctly the board, score, player and number of ply" in {
  //     Forsyth.>>(game4) must_== FEN("1s1SS/sssSSS/1ss1SS1/3s4/4S4/3S4/1SS1ss1/S1Ssss/1S1ss 0 0 w 3 2")
  //     Forsyth.>>(situationFromFen) must_== FEN("ss1SS/sssSSS/1ss1SS1/8/9/8/1SS1ss1/SSSsss/SS1ss 0 0 b 0 1")
  //     Forsyth.>>(game) must_== FEN("ss1SS/sssSSS/1ss1SS1/8/9/8/1SS1ss1/SSSsss/SS1ss 0 0 b 0 1")
  //   }

  //   "Forsyth.>>(StratGameLogic, StratGame) forwards to the expected one implemented in Dameo namespace" in {
  //     (StratFormat.Forsyth.>>(StratGameLogic(7), stratGame)).value must_== FEN(
  //       "ss1SS/sssSSS/1ss1SS1/8/9/8/1SS1ss1/SSSsss/SS1ss 0 0 b 0 1"
  //     ).value
  //   }
  // }
}
