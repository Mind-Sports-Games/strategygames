package strategygames.go

import org.specs2.matcher.ValidatedMatchers
import org.specs2.mutable.Specification

import strategygames.Player
import strategygames.go.format.FEN

class GoSituationTest extends Specification with ValidatedMatchers {

  "valid moves and drops in situation" should {
    val board     = Board.init(variant.Go19x19)
    val p1        = Player(true)
    val situation = Situation(board, p1)

    val moves = variant.Go19x19.validMoves(situation)
    val drops = variant.Go19x19.validDrops(situation)

    "have no moves" in {
      moves.size must_== 0
    }
    "all 361 drops" in {
      drops.size must_== 361
    }
  }

  "valid apiPosition fen from situation with moves" should {
    val game  = Game(variant.Go19x19)
    val drops = variant.Go19x19.validDrops(game.situation)

    val game1 = game.apply(drops(1))
    val fen   = game1.situation.board.apiPosition.fen

    "have differnt fen after drop" in {
      variant.Go19x19.initialFen.engineFen must_!= fen.engineFen
    }
  }

  "valid board and player fen from situation with moves" should {
    val game  = Game(variant.Go19x19)
    val drops = variant.Go19x19.validDrops(game.situation)
    val drop  = drops(1)

    val game1 = game.apply(drop)
    val fen   = game1.situation.board.apiPosition.fen

    "have correct fen after drop b1" in {
      "19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/1S17[SSSSSSSSSSssssssssss] w - 3610 65 65 2" must_== drop.after.apiPosition.fen.value
    }

    val boardFen = format.Forsyth.boardAndPlayer(game1.situation)
    "have differnt fen after drop" in {
      fen.value.split(' ').headOption must_== boardFen.split(' ').headOption
    }
    "and be equal to a specific fen" in {
      boardFen
        .split(' ')(0)
        .split('[')(0) must_== "19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/1S17"
    }
  }

  // creating game from position, i.e. with different starting fen
  "valid game when starting form handicapped position" should {
    val startingFen = FEN("9/9/2S3S2/9/9/9/9/9/9[SSSSSSSSSSssssssssss] w - 810 40 40 1")
    val situation   = strategygames.go.format.Forsyth.<<@(variant.Go9x9, startingFen)

    val game = Game(Some(variant.Go9x9), Some(startingFen))

    "have the same situation piecemap from game creation and using forsyth" in {
      game.situation.board.apiPosition.pieceMap must_== situation
        .map(_.board.apiPosition.pieceMap)
        .getOrElse {}
    }
    "have the same situation fen from game creation and using forsyth" in {
      game.situation.board.apiPosition.fen must_== situation
        .map(_.board.apiPosition.fen)
        .getOrElse(FEN(""))
    }

    val drops = variant.Go9x9.validDrops(game.situation)
    val drop  = drops(0)

    val game1 = game.apply(drop)
    "have the correct starting fen after a move" in {
      game1.situation.board.apiPosition.initialFen.value must_== "9/9/2S3S2/9/9/9/9/9/9[SSSSSSSSSSssssssssss] w - 810 40 40 1"
    }
    "and the correct current fen after a move" in {
      game1.situation.board.apiPosition.fen.value must_== "9/9/2S3S2/9/9/9/9/9/s8[SSSSSSSSSSssssssssss] b - 20 50 40 1"
    }

  }

  "valid fen from new game creation handicapped" should {
    val startingFen = FEN("9/9/2S3S2/9/9/9/9/9/9[SSSSSSSSSSssssssssss] w - 810 40 40 1")
    val game        = Game(Some(variant.Go9x9), Some(startingFen))

    val fen = strategygames.go.format.Forsyth.>>(game)
    "fen should match that of starting fen" in {
      startingFen must_== fen
    }
  }

  "valid end game after two passes and selecting dead stones" should {
    val game  = Game(variant.Go19x19)
    val drops = variant.Go19x19.validDrops(game.situation)

    val game_ply_1 = game.apply(drops(1))

    val drops_ply_2 = variant.Go19x19.validDrops(game_ply_1.situation)
    val game_ply_2  = game_ply_1.apply(drops_ply_2(1))

    val drops_ply_3 = variant.Go19x19.validDrops(game_ply_2.situation)
    val game_ply_3  = game_ply_2.apply(drops_ply_3(1))

    val drops_ply_4 = variant.Go19x19.validDrops(game_ply_3.situation)
    val game_ply_4  = game_ply_3.apply(drops_ply_4(1))

    val drops_ply_5 = variant.Go19x19.validDrops(game_ply_4.situation)
    val game_ply_5  = game_ply_4.apply(drops_ply_5(1))

    val drops_ply_6 = variant.Go19x19.validDrops(game_ply_5.situation)
    val game_ply_6  = game_ply_5.apply(drops_ply_6(1))

    val drops_ply_7 = variant.Go19x19.validDrops(game_ply_6.situation)
    val game_ply_7  = game_ply_6.apply(drops_ply_7(1))

    val pass_1     = variant.Go19x19.validPass(game_ply_7.situation)
    val game_ply_8 = game_ply_7.apply(pass_1)

    val pass_2     = variant.Go19x19.validPass(game_ply_8.situation)
    val game_ply_9 = game_ply_8.apply(pass_2)

    val squares: List[Pos] = List(Pos.B1, Pos.D1)
    val ss                 = variant.Go19x19.createSelectSquares(game_ply_9.situation, squares)
    val game_ply_10        = game_ply_9.apply(ss)

    "not be gameEnd after just two passes" in {
      game_ply_9.situation.end must_== false
      game_ply_9.situation.board.apiPosition.pieceMap.size must_== 7
      game_ply_9.situation.board.apiPosition.fen.value must_== "19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/1SsSsSsS11[SSSSSSSSSSssssssssss] w - 40 95 65 6"
    }

    "be gameEnd after final ss action" in {
      game_ply_10.situation.end must_== true
      game_ply_10.situation.board.apiPosition.pieceMap.size must_== 5
      game_ply_10.situation.board.apiPosition.fen.value must_== "19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/2s1sSsS11[SSSSSSSSSSssssssssss] w - 20 95 65 6"
    }

  }

  "valid end game after two passes replaying moves and then passing and selecting dead stones" should {
    val game  = Game(variant.Go19x19)
    val drops = variant.Go19x19.validDrops(game.situation)

    val game_ply_1 = game.apply(drops(1))

    val drops_ply_2 = variant.Go19x19.validDrops(game_ply_1.situation)
    val game_ply_2  = game_ply_1.apply(drops_ply_2(1))

    val drops_ply_3 = variant.Go19x19.validDrops(game_ply_2.situation)
    val game_ply_3  = game_ply_2.apply(drops_ply_3(1))

    val drops_ply_4 = variant.Go19x19.validDrops(game_ply_3.situation)
    val game_ply_4  = game_ply_3.apply(drops_ply_4(1))

    val drops_ply_5 = variant.Go19x19.validDrops(game_ply_4.situation)
    val game_ply_5  = game_ply_4.apply(drops_ply_5(1))

    val drops_ply_6 = variant.Go19x19.validDrops(game_ply_5.situation)
    val game_ply_6  = game_ply_5.apply(drops_ply_6(1))

    val drops_ply_7 = variant.Go19x19.validDrops(game_ply_6.situation)
    val game_ply_7  = game_ply_6.apply(drops_ply_7(1))

    val pass_1     = variant.Go19x19.validPass(game_ply_7.situation)
    val game_ply_8 = game_ply_7.apply(pass_1)

    val pass_2     = variant.Go19x19.validPass(game_ply_8.situation)
    val game_ply_9 = game_ply_8.apply(pass_2)

    val drops_ply_10 = variant.Go19x19.validDrops(game_ply_9.situation)
    val game_ply_10  = game_ply_9.apply(drops_ply_10(1))

    val drops_ply_11 = variant.Go19x19.validDrops(game_ply_10.situation)
    val game_ply_11  = game_ply_10.apply(drops_ply_11(1))

    val drops_ply_12 = variant.Go19x19.validDrops(game_ply_11.situation)
    val game_ply_12  = game_ply_11.apply(drops_ply_12(1))

    val pass_2_1    = variant.Go19x19.validPass(game_ply_12.situation)
    val game_ply_13 = game_ply_12.apply(pass_2_1)

    val pass_2_2    = variant.Go19x19.validPass(game_ply_13.situation)
    val game_ply_14 = game_ply_13.apply(pass_2_2)

    val squares: List[Pos] = List(Pos.B1, Pos.D1)
    val ss                 = variant.Go19x19.createSelectSquares(game_ply_14.situation, squares)
    val game_ply_15        = game_ply_14.apply(ss)

    "not be gameEnd after just two passes (both cases)" in {
      game_ply_9.situation.end must_== false
      game_ply_9.situation.board.apiPosition.pieceMap.size must_== 7
      game_ply_9.situation.board.apiPosition.fen.value must_== "19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/1SsSsSsS11[SSSSSSSSSSssssssssss] w - 40 95 65 6"
      game_ply_14.situation.end must_== false
      game_ply_14.situation.board.apiPosition.pieceMap.size must_== 10
    }

    "be gameEnd after final ss action" in {
      game_ply_15.situation.end must_== true
      game_ply_15.situation.board.apiPosition.pieceMap.size must_== 8
      game_ply_15.situation.board.apiPosition.fen.value must_== "19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/2s1sSsSsSs8[SSSSSSSSSSssssssssss] b - 30 115 65 9"
    }

    "have no more drops after game end" in {
      game_ply_15.situation.drops must_== None
    }
  }

  "two passes triggering select stones followed by a third pass doesn't trigger select stones, even number required" should {
    val game  = Game(variant.Go19x19)
    val drops = variant.Go19x19.validDrops(game.situation)

    val game_ply_1 = game.apply(drops(1))

    val drops_ply_2 = variant.Go19x19.validDrops(game_ply_1.situation)
    val game_ply_2  = game_ply_1.apply(drops_ply_2(1))

    val drops_ply_3 = variant.Go19x19.validDrops(game_ply_2.situation)
    val game_ply_3  = game_ply_2.apply(drops_ply_3(1))

    val drops_ply_4 = variant.Go19x19.validDrops(game_ply_3.situation)
    val game_ply_4  = game_ply_3.apply(drops_ply_4(1))

    val drops_ply_5 = variant.Go19x19.validDrops(game_ply_4.situation)
    val game_ply_5  = game_ply_4.apply(drops_ply_5(1))

    val drops_ply_6 = variant.Go19x19.validDrops(game_ply_5.situation)
    val game_ply_6  = game_ply_5.apply(drops_ply_6(1))

    val drops_ply_7 = variant.Go19x19.validDrops(game_ply_6.situation)
    val game_ply_7  = game_ply_6.apply(drops_ply_7(1))

    val pass_1     = variant.Go19x19.validPass(game_ply_7.situation)
    val game_ply_8 = game_ply_7.apply(pass_1)

    val pass_2     = variant.Go19x19.validPass(game_ply_8.situation)
    val game_ply_9 = game_ply_8.apply(pass_2)

    val pass_3      = variant.Go19x19.validPass(game_ply_9.situation)
    val game_ply_10 = game_ply_9.apply(pass_3)

    val pass_4      = variant.Go19x19.validPass(game_ply_10.situation)
    val game_ply_11 = game_ply_10.apply(pass_4)

    val squares: List[Pos] = List(Pos.B1, Pos.D1)
    val ss                 = variant.Go19x19.createSelectSquares(game_ply_11.situation, squares)
    val game_ply_12        = game_ply_11.apply(ss)

    "not be gameEnd after just two passes (both cases)" in {
      game_ply_9.situation.end must_== false
      game_ply_9.situation.board.apiPosition.pieceMap.size must_== 7
      game_ply_9.situation.board.apiPosition.fen.value must_== "19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/1SsSsSsS11[SSSSSSSSSSssssssssss] w - 40 95 65 6"
      game_ply_11.situation.end must_== false
      game_ply_11.situation.board.apiPosition.pieceMap.size must_== 7
    }

    "be gameEnd after final ss action" in {
      game_ply_12.situation.end must_== true
      game_ply_12.situation.board.apiPosition.pieceMap.size must_== 5
      game_ply_12.situation.board.apiPosition.fen.value must_== "19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/2s1sSsS11[SSSSSSSSSSssssssssss] w - 20 95 65 7"
    }

    "not have a ss action after single pass" in {
      game_ply_8.situation.canSelectSquares must_== false
      game_ply_10.situation.canSelectSquares must_== false
    }

    "have a ss action after double pass" in {
      game_ply_9.situation.canSelectSquares must_== true
      game_ply_11.situation.canSelectSquares must_== true
    }

  }
}
