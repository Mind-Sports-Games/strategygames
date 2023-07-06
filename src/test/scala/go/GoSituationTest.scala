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
      "19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/1S17[SSSSSSSSSSssssssssss] w - 361 6 6 2" must_== drop.after.apiPosition.fen.value
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
    val startingFen = FEN("9/9/2S3S2/9/9/9/9/9/9[SSSSSSSSSSssssssssss] w - 81 4 4 1")
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
      game1.situation.board.apiPosition.initialFen.value must_== "9/9/2S3S2/9/9/9/9/9/9[SSSSSSSSSSssssssssss] w - 81 4 4 1"
    }
    "and the correct current fen after a move" in {
      game1.situation.board.apiPosition.fen.value must_== "9/9/2S3S2/9/9/9/9/9/s8[SSSSSSSSSSssssssssss] b - 2 5 4 1"
    }

  }

  "valid fen from new game creation handicapped" should {
    val startingFen = FEN("9/9/2S3S2/9/9/9/9/9/9[SSSSSSSSSSssssssssss] w - 81 4 4 1")
    val game        = Game(Some(variant.Go9x9), Some(startingFen))

    val fen = strategygames.go.format.Forsyth.>>(game)
    "fen should match that of starting fen" in {
      startingFen must_== fen
    }
  }

}
