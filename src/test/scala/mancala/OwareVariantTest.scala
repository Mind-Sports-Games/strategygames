package strategygames.mancala

import org.specs2.matcher.ValidatedMatchers
import org.specs2.mutable.Specification

import strategygames.Player

class OwareVariantTest extends Specification with ValidatedMatchers {

  "valid moves in situation" should {
    val board     = Board.init(variant.Oware)
    val p1        = Player(true)
    val situation = Situation(board, p1)

    val moves = variant.Oware.validMoves(situation)
    // val m = situation.moves
    "be valid" in {
      moves.size must_== 6
    }
  }

  "valid moves in situation" should {
    val board     = Board.init(variant.Oware)
    val p1        = Player(true)
    val situation = Situation(board, p1)
    val moves     = variant.Oware.validMoves(situation)
    val uci       = moves.get(Pos.C1) match {
      case Some(move) => move.head.toUci.uci
      case None       => ""
    }
    "c1 goes to f2" in {
      uci must_== "c1f2"
    }
  }

  "pos in second rank" should {
    val pos      = Pos(6)
    val expected = Pos.at(5, 1)
    "be backwards, Pos(6) is f2" in {
      pos must_== expected
    }
  }

  "grandslam in oware" should {
    val fen      = "3,2S,1S,2S/1S,3,2S,3S 20 17 S"
    val position = Api.positionFromFen(fen)
    val newGame  = position.makeMoves(List(5))
    "have an initial valid fen" in {
      Api.validateFEN(fen) must_== true
    }
    "is allowed the grandslam move" in {
      position.legalMoves.contains(5) must_== true
    }
    "current pos is ongoing" in {
      position.gameEnd must_== false
    }
    "but does not capture pieces once moved" in {
      newGame.gameEnd must_== false
    }
    "hence new new fen is valid" in {
      Api.validateFEN(newGame.fen.value) must_== true
    }
    "and equal to 3,3S,2S,3S/1S,3,2S,1 20 17 N" in {
      newGame.fen.value must_== "3,3S,2S,3S/1S,3,2S,1 20 17 N"
    }
  }

  "grandslam in oware" should {
    val fen      = "1S,1,8S,2,3S/2S,1S,1S,2S,2S,2S 13 13 N"
    val position = Api.positionFromFen(fen)
    val newGame  = position.makeMoves(List(9))
    "have an initial valid fen" in {
      Api.validateFEN(fen) must_== true
    }
    "is allowed the grandslam move" in {
      position.legalMoves.contains(9) must_== true
    }
    "current pos is ongoing" in {
      position.gameEnd must_== false
    }
    "but does not capture pieces once moved" in {
      newGame.gameEnd must_== false
    }
    "hence new new fen is valid" in {
      Api.validateFEN(newGame.fen.value) must_== true
    }
    "and equal to 2S,1S,3,3S/3S,2S,2S,3S,3S,3S 13 13 S" in {
      newGame.fen.value must_== "2S,1S,3,3S/3S,2S,2S,3S,3S,3S 13 13 S"
    }
  }

  "grandslam in oware" should {
    val fen      = "5,1S/1S,4,1S 22 23 S"
    val position = Api.positionFromFen(fen)
    val newGame  = position.makeMoves(List(5))
    "have an initial valid fen" in {
      Api.validateFEN(fen) must_== true
    }
    "is allowed the grandslam move" in {
      position.legalMoves.contains(5) must_== true
    }
    "current pos is ongoing" in {
      position.gameEnd must_== false
    }
    "but does not capture pieces once moved" in {
      newGame.gameEnd must_== false
    }
    "hence new new fen is valid" in {
      Api.validateFEN(newGame.fen.value) must_== true
    }
    "and equal to 5,2S/1S,5 22 23 N" in {
      newGame.fen.value must_== "5,2S/1S,5 22 23 N"
    }
  }

  "cycle position in oware" should {
    val fen      = "1S,5/5,1S 23 23 S"
    val position = Api.positionFromFen(fen)
    val newGame  = position.makeMoves(List(5, 11, 0, 6, 1, 7, 2, 8, 3, 9, 4, 10))
    "have an initial valid fen" in {
      Api.validateFEN(fen) must_== true
    }
    "have legal moves..." in {
      position.legalMoves.contains(5) must_== true
    }
    "after cycle moves board fens are the same" in {
      newGame.fen.value.split(" ")(0) must_== fen.split(" ")(0)
    }
    "Ending cycle changes final fen scores" in {
      newGame.fen.value must_== fen.split(" ")(0) + " 24 24 S"
    }
    "and result in a draw" in {
      newGame.gameResult must_== GameResult.Draw()
    }
  }

  "not yet cycle position in oware" should {
    val fen      = "1S,5/5,1S 23 23 S"
    val position = Api.positionFromFen(fen)
    val newGame  = position.makeMoves(List(5, 11, 0, 6, 1, 7, 2, 8, 3, 9, 4))
    "have an initial valid fen" in {
      Api.validateFEN(fen) must_== true
    }
    "have legal moves..." in {
      position.legalMoves.contains(5) must_== true
    }
    "currently be ongoing game" in {
      position.gameResult must_== GameResult.Ongoing()
    }
    "after 1 move from cycle moves fens are not the same" in {
      newGame.fen.value must_!= fen
    }
    "and not yet result in a draw" in {
      newGame.gameResult must_== GameResult.Ongoing()
    }
  }

  "cycle position in oware" should {
    val fen      = "2S,5/5,2S 21 23 S"
    val position = Api.positionFromFen(fen)
    val newGame  = position.makeMoves(List(5, 11, 0, 6, 1, 7, 2, 8, 3, 9, 4, 10))
    "have an initial valid fen" in {
      Api.validateFEN(fen) must_== true
    }
    "have legal moves..." in {
      position.legalMoves.contains(5) must_== true
    }
    "detected end game cycle" in {
      position.gameResult must_== GameResult.VariantEnd()
    }
    "and made North winner" in {
      position.gameOutcome must_== -1000
    }
    "api allows more moves - cycle moves board fens are the same" in {
      newGame.fen.value.split(" ")(0) must_== fen.split(" ")(0)
    }
    "Ending cycle changes final fen scores" in {
      newGame.fen.value must_== fen.split(" ")(0) + " 23 25 S"
    }
    "and result in also game end" in {
      newGame.gameResult must_== GameResult.VariantEnd()
    }
    "and still a win for North" in {
      newGame.gameOutcome must_== -1000
    }
  }

  "Another cycle position in oware - part 1" should {
    val fen             = "4S,4S,4S,4S,4S,4S/4S,4S,4S,4S,4S,4S 0 0 S"
    val initialPosition = Api.positionFromFen(fen)
    val uciMoves        = List(
      "d1e2",
      "b2c1",
      "b1f2",
      "a2e1",
      "c1c2",
      "e2b1",
      "e1a1",
      "c2e1",
      "f1b1",
      "b2c1",
      "d1e2",
      "f2e1",
      "a1b1",
      "c2a1",
      "f1e2",
      "d2f2",
      "d1f2",
      "b2c1",
      "c1f2",
      "a2c2",
      "e1c2",
      "e2c2",
      "f1b2",
      "c2c1",
      "a1f1",
      "f2b2",
      "b1c2",
      "d2c1",
      "e1f2",
      "e2b2",
      "d1e2",
      "c2a2",
      "f1c2",
      "b2e1",
      "b1d2",
      "a2d1",
      "a1d1",
      "e2c2",
      "b1d1",
      "c2a2",
      "c1e1",
      "d2a2",
      "d1f2",
      "a2b1",
      "a1b1",
      "b2a1",
      "a1b1",
      "a2a1",
      "a1b1",
      "c2b2",
      "b1f1",
      "b2a2",
      "e1e2",
      "e2d2",
      "c1d1",
      "f2e2",
      "f1d2",
      "f2e2",
      "d1f1",
      "a2a1",
      "e1f1",
      "e2d2",
      "f1e2",
      "f2e2",
      "a1b1",
      "e2c2",
      "b1c1",
      "d2b2",
      "c1d1",
      "c2a2",
      "d1e1",
      "b2a1",
      "e1f1",
      "a2b1",
      "f1f2",
      "f2e2",
      "a1c1",
      "e2d2",
      "b1d1",
      "d2c2",
      "c1e1",
      "c2b2",
      "d1f1",
      "b2a2",
      "e1f2",
      "a2a1",
      "f1e2",
      "f2d2"
    )
    val moves           = uciMoves.dropRight(2).map(x => (Api.uciToMove(x)))
    val newGame         = initialPosition.makeMovesWithPrevious(moves, List[String]())
    "currently be ongoing game" in {
      newGame.gameResult must_== GameResult.Ongoing()
    }
    "game score is 0 as not ended" in {
      newGame.gameOutcome must_== 0
    }
  }

  "Another cycle position in oware - part 2" should {
    val fen             = "4S,4S,4S,4S,4S,4S/4S,4S,4S,4S,4S,4S 0 0 S"
    val initialPosition = Api.positionFromFen(fen)
    val uciMoves        = List(
      "d1e2",
      "b2c1",
      "b1f2",
      "a2e1",
      "c1c2",
      "e2b1",
      "e1a1",
      "c2e1",
      "f1b1",
      "b2c1",
      "d1e2",
      "f2e1",
      "a1b1",
      "c2a1",
      "f1e2",
      "d2f2",
      "d1f2",
      "b2c1",
      "c1f2",
      "a2c2",
      "e1c2",
      "e2c2",
      "f1b2",
      "c2c1",
      "a1f1",
      "f2b2",
      "b1c2",
      "d2c1",
      "e1f2",
      "e2b2",
      "d1e2",
      "c2a2",
      "f1c2",
      "b2e1",
      "b1d2",
      "a2d1",
      "a1d1",
      "e2c2",
      "b1d1",
      "c2a2",
      "c1e1",
      "d2a2",
      "d1f2",
      "a2b1",
      "a1b1",
      "b2a1",
      "a1b1",
      "a2a1",
      "a1b1",
      "c2b2",
      "b1f1",
      "b2a2",
      "e1e2",
      "e2d2",
      "c1d1",
      "f2e2",
      "f1d2",
      "f2e2",
      "d1f1",
      "a2a1",
      "e1f1",
      "e2d2",
      "f1e2",
      "f2e2",
      "a1b1",
      "e2c2",
      "b1c1",
      "d2b2",
      "c1d1",
      "c2a2",
      "d1e1",
      "b2a1",
      "e1f1",
      "a2b1",
      "f1f2",
      "f2e2",
      "a1c1",
      "e2d2",
      "b1d1",
      "d2c2",
      "c1e1",
      "c2b2",
      "d1f1",
      "b2a2",
      "e1f2",
      "a2a1",
      "f1e2",
      "f2d2"
    )
    val moves           = uciMoves.dropRight(2).map(Api.uciToMove)
    val newGame         = initialPosition.makeMovesWithPrevious(moves, List[String]())
    val cycleGame       = newGame.makeMovesWithPrevious(List(5, 6), uciMoves.dropRight(2))
    "currently be ongoing game" in {
      newGame.gameResult must_== GameResult.Ongoing()
    }
    "game score is 0 as not ended" in {
      newGame.gameOutcome must_== 0
    }
    "and result in a draw" in {
      cycleGame.gameResult must_== GameResult.VariantEnd()
    }
    "game score is win for North -1000" in {
      cycleGame.gameOutcome must_== -1000
    }
    "and game has ended" in {
      cycleGame.gameEnd must_== true
    }
    "but still legal moves for some reason" in {
      cycleGame.legalMoves must_== Array(0)
    }
  }

  "Cycle position in oware - what is result?" should {
    // should this test be a draw - have to ask experts, as position repeats while 2 remaining stones left on South Side.
    val fen             = "4S,4S,4S,4S,4S,4S/4S,4S,4S,4S,4S,4S 0 0 S"
    val initialPosition = Api.positionFromFen(fen)
    val uciMoves        = List(
      "e1d2",
      "c2b1",
      "f1b2",
      "d2c1",
      "d1e2",
      "a2f1",
      "b1d2",
      "b2f1",
      "a1d2",
      "f2d1",
      "c1a1",
      "a2c1",
      "e1f2",
      "c2b1",
      "a1e1",
      "b2b1",
      "b1d2",
      "e2d2",
      "f1b2",
      "a2c1",
      "e1e2",
      "f2b2",
      "c1e2",
      "d2c2",
      "d1d2",
      "c2a2",
      "e1f2",
      "b2a1",
      "a1e1",
      "d2c2",
      "e1f1",
      "c2b2",
      "d1e1",
      "b2a2",
      "f1b2",
      "a2c1",
      "e1f1",
      "e2c2",
      "f1f2",
      "d2b2",
      "b1f1",
      "b2a1",
      "f1f2",
      "c2a1",
      "e1f1",
      "f2e2",
      "a1b1",
      "a2b1",
      "d1e1",
      "b2a2",
      "f1f2",
      "a2a1",
      "e1f1",
      "f2e2",
      "f1f2",
      "e2c2",
      "c1d1",
      "d2c2",
      "d1e1",
      "c2a2",
      "e1f1",
      "f2e2",
      "f1f2",
      "a2a1",
      "a1b1",
      "f2e2",
      "b1c1",
      "e2c2",
      "c1d1",
      "d2c2",
      "d1e1",
      "c2a2",
      "e1f1",
      "b2a1",
      "a1b1",
      "a2b1",
      "f1f2",
      "f2e2",
      "a1b1",
      "e2d2",
      "b1c1",
      "d2c2",
      "c1d1",
      "c2b2",
      "d1e1",
      "b2a2",
      "e1f1",
      "a2a1"
    )
    val moves           = uciMoves.dropRight(1).map(Api.uciToMove)
    val newGame         = initialPosition.makeMovesWithPrevious(moves, List[String]())
    val cycleGame       = newGame.makeMovesWithPrevious(List(11), uciMoves.dropRight(1))
    "currently be ongoing game" in {
      newGame.gameResult must_== GameResult.Ongoing()
    }
    "game score is 0 as not ended" in {
      newGame.gameOutcome must_== 0
    }
    "after cycle move, result in a variant end" in {
      cycleGame.gameResult must_== GameResult.VariantEnd()
    }
    "game score is win for South 1000" in {
      cycleGame.gameOutcome must_== 1000
    }
    "and game has ended" in {
      cycleGame.gameEnd must_== true
    }
    "but still legal moves for some reason" in {
      cycleGame.legalMoves must_== Array(5)
    }
  }

}
