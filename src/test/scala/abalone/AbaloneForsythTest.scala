package abalone

import org.specs2.matcher.ValidatedMatchers

import strategygames.Score
import strategygames.abalone.format.{ FEN, Forsyth }
import strategygames.abalone.{ Board, Game, History, Pos, P1, variant }

class AbaloneForsythTest extends AbaloneTest with ValidatedMatchers {

    "situationFromFen" should {
        val fenFromVariant      = variant.Abalone.initialFen
        val situationFromFen    = Forsyth<<(fenFromVariant)

        "create the situation of Belgian Daisy" in {
            val board = Board(fenFromVariant.pieces, History(score = Score(0, 0)), variant.Abalone)
            situationFromFen.get.board.pieces must_== board.pieces
            situationFromFen.get.player must_== P1
        }
    }

    "fen encoding from board after 3 ply" should {
        val fenFromVariant      = variant.Abalone.initialFen
        val situationFromFen    = (Forsyth<<(fenFromVariant)).get
        val board = Board(fenFromVariant.pieces, History(score = Score(0, 0)), variant.Abalone)
        val game = Game.apply(board.variant)
        val validMoves = game.board.variant.validMoves(situationFromFen)
        val game2 = game.apply(validMoves(Pos.A1)(1))
        val validMoves2 = game2.board.variant.validMoves(game2.situation)
        val game3 = game2.apply(validMoves2(Pos.E9)(1))
        val validMoves3 = game3.board.variant.validMoves(game3.situation)
        val game4 = game3.apply(validMoves3(Pos.B2)(2))
        game4.board.variant.validMoves(game4.situation)

        "describe the updated piecemap" in {
            Forsyth.boardPart(board) must_== "SS1ss/SSSsss/1SS1ss1/8/9/8/1ss1SS1/sssSSS/ss1SS"
            Forsyth.boardPart(game4.board) must_== "1S1ss/SSSsss/1SS1ss1/3S4/4s4/3s4/1ss1SS1/s1sSSS/1s1SS"
        }

        "describe the board, score, player and number of ply" in {
            Forsyth>>(situationFromFen) must_== FEN("SS1ss/SSSsss/1SS1ss1/8/9/8/1ss1SS1/sssSSS/ss1SS 0 0 b 0 0")
            Forsyth>>(game4.situation) must_== FEN("1S1ss/SSSsss/1SS1ss1/3S4/4s4/3s4/1ss1SS1/s1sSSS/1s1SS 0 0 w 3 1")
        }
    }
}