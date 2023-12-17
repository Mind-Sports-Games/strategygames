package strategygames.chess

import strategygames.chess.variant.Monster

class MonsterVariantTest extends ChessTest {

  "Monster chess" should {

    "Black be in check two moves away" in {
      import Pos._
      val game        = fenToGame(Monster.initialFen, Monster)
      val successGame = game flatMap (_.playMoves(
        E2 -> E4,
        E4 -> E5,
        F7 -> F6,
        E1 -> E2,
        E5 -> F6
      ))
      successGame must beValid.like { case game =>
        // normally 21 moves are available to due to the kingSafety in Monster its only 4
        game.situation.moves.values.flatten.size must_== 4
      }
    }

    "White have to work towards getting out of check if it requires two moves" in {
      import Pos._
      val game        = fenToGame(Monster.initialFen, Monster)
      val successGame = game flatMap (_.playMoves(
        F2 -> F4,
        E1 -> F1,
        G7 -> G5,
        F4 -> G5,
        F1 -> G1,
        E7 -> E6,
        G5 -> G6,
        G6 -> H7,
        D8 -> G5,
        G1 -> H1,
        E2 -> E3,
        H8 -> H7
      ))
      successGame must beValid.like { case game =>
        // without kingSafety checks 8 moves are available
        game.situation.moves.values.flatten.size must_== 2
      }
    }

    "Black has to escape check rather than deliver checkmate" in {
      import Pos._
      val game        = fenToGame(Monster.initialFen, Monster)
      val successGame = game flatMap (
        _.playMoves(
          F2 -> F4,
          E1 -> F1,
          G7 -> G5,
          F4 -> G5,
          F1 -> G1,
          E7 -> E6,
          G5 -> G6,
          G6 -> H7,
          D8 -> G5,
          G1 -> H1,
          E2 -> E3,
          H8 -> H7,
          H1 -> G1,
          G1 -> F1,
          A7 -> A5,
          C2 -> C3,
          D2 -> D3,
          A5 -> A4,
          C3 -> C4,
          C4 -> C5,
          H7 -> G7,
          F1 -> G1,
          G1 -> H1,
          A4 -> A3,
          D3 -> D4,
          D4 -> D5,
          A8 -> A4,
          E3 -> E4,
          E4 -> E5,
          A4 -> F4,
          C5 -> C6,
          D5 -> D6
        )
      )
      successGame must beValid.like { case game =>
        game.situation.board.checkP1 must_== false
        game.situation.board.checkP2 must_== true
        // without disabling black checks, 9 moves are available
        game.situation.moves.values.flatten.size must_== 2
      }
    }

    "Must include multiple enpassant moves for Black" in {
      import Pos._
      val game        = fenToGame(Monster.initialFen, Monster)
      val successGame = game flatMap (_.playMoves(
        F2 -> F4,
        E1 -> F1,
        D7 -> D5,
        F4 -> F5,
        F1 -> F2,
        D5 -> D4,
        C2 -> C4,
        E2 -> E4
      ))
      successGame must beValid.like { case game =>
        game.situation.moves.values.flatten.size must_== 28
      }
    }

    "Must only allow enpassant on Whites first move in turn" in {
      import Pos._
      val game        = fenToGame(Monster.initialFen, Monster)
      val successGame = game flatMap (_.playMoves(
        F2 -> F4,
        E1 -> F1,
        D7 -> D5,
        F4 -> F5,
        F1 -> F2,
        D5 -> D4,
        C2 -> C4,
        E2 -> E4,
        G7 -> G5,
        F2 -> G2
      ))
      successGame must beValid.like { case game =>
        game.situation.moves.values.flatten.size must_== 12
      }
    }

    "Must now allow pawn move that sets up an enpassant checkmate" in {
      import Pos._
      val game        = fenToGame(Monster.initialFen, Monster)
      val successGame = game flatMap (_.playMoves(
        E2 -> E4,
        E4 -> E5,
        E7 -> E6,
        E1 -> E2,
        E2 -> E3,
        E8 -> E7,
        E3 -> E2,
        E2 -> E3
      ))
      successGame must beValid.like { case game =>
        game.situation.moves.values.flatten.pp("moves").size must_== 15
      }
    }

    "Must be checkmate with possible Queen or Knight promotion" in {
      import Pos._
      val game        = fenToGame(Monster.initialFen, Monster)
      val successGame = game flatMap (_.playMoves(
        D2 -> D4,
        E2 -> E4,
        A7 -> A5,
        D4 -> D5,
        E4 -> E5,
        A5 -> A4,
        E1 -> D1,
        D1 -> C1,
        H7 -> H5,
        C1 -> B2,
        C2 -> C4,
        E7 -> E6,
        D5 -> E6,
        E6 -> F7
        // E8 -> E7
      ))
      successGame must beValid.like { case game =>
        game.situation.checkMate must_== true
      }
    }

    // TODO Other tests Matt can add:
    // Castling cases (several in first 8 moves) http://localhost:9663/nvieBdKQaUtD
    // White Checkmates with King http://localhost:9663/nvieBdKQaUtD
    // Black Stalemate http://localhost:9663/pbJssxIP8bMy
  }
}
