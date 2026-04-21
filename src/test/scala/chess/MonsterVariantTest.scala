package strategygames.chess

import strategygames.format.{ FEN => StratFen, Forsyth => StratForsyth, Uci => StratUci }
import strategygames.variant.{ Variant => StratVariant }
import strategygames.chess.variant.Monster

class MonsterVariantTest extends ChessTest {

  "Monster chess" should {

    "Black be in check two moves away" in {
      import Pos._
      val game        = fenToGame(Monster.initialFen, Monster)
      val successGame = game.andThen((g: Game) => g.playMoves(
        E2 -> E4,
        E4 -> E5,
        F7 -> F6,
        E1 -> E2,
        E5 -> F6
      ))
      successGame .toOption must beSome.like { case game =>
        // normally 21 moves are available to due to the kingSafety in Monster its only 4
        game.situation.moves.values.flatten.size === 4
      }
    }

    "White have to work towards getting out of check if it requires two moves" in {
      import Pos._
      val game        = fenToGame(Monster.initialFen, Monster)
      val successGame = game.andThen((g: Game) => g.playMoves(
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
      successGame .toOption must beSome.like { case game =>
        // without kingSafety checks 8 moves are available
        game.situation.moves.values.flatten.size === 2
      }
    }

    "Black has to escape check rather than deliver checkmate" in {
      import Pos._
      val game        = fenToGame(Monster.initialFen, Monster)
      val successGame = game andThen (
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
      successGame .toOption must beSome.like { case game =>
        game.situation.board.checkP1 === false
        game.situation.board.checkP2 === true
        // without disabling black checks, 9 moves are available
        game.situation.moves.values.flatten.size === 2
      }
    }

    "Must include multiple enpassant moves for Black" in {
      import Pos._
      val game        = fenToGame(Monster.initialFen, Monster)
      val successGame = game.andThen((g: Game) => g.playMoves(
        F2 -> F4,
        E1 -> F1,
        D7 -> D5,
        F4 -> F5,
        F1 -> F2,
        D5 -> D4,
        C2 -> C4,
        E2 -> E4
      ))
      successGame .toOption must beSome.like { case game =>
        game.situation.moves.values.flatten.size === 28
      }
    }

    "Must only allow enpassant on Whites first move in turn" in {
      import Pos._
      val game        = fenToGame(Monster.initialFen, Monster)
      val successGame = game.andThen((g: Game) => g.playMoves(
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
      successGame .toOption must beSome.like { case game =>
        game.situation.moves.values.flatten.size === 12
      }
    }

    "Must not allow pawn move that sets up an enpassant checkmate" in {
      import Pos._
      val game        = fenToGame(Monster.initialFen, Monster)
      val successGame = game.andThen((g: Game) => g.playMoves(
        E2 -> E4,
        E4 -> E5,
        E7 -> E6,
        E1 -> E2,
        E2 -> E3,
        E8 -> E7,
        E3 -> E2,
        E2 -> E3
      ))
      successGame .toOption must beSome.like { case game =>
        game.situation.moves.values.flatten.size === 15
      }
    }

    "Must be checkmate with possible Queen or Knight promotion" in {
      import Pos._
      val game        = fenToGame(Monster.initialFen, Monster)
      val successGame = game.andThen((g: Game) => g.playMoves(
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
      successGame .toOption must beSome.like { case game =>
        game.situation.checkMate === true
      }
    }

    "Must be checkmate with White King two moves from capturing Black's King (and not stalemate)" in {
      import Pos._
      val game        = fenToGame(Monster.initialFen, Monster)
      val successGame = game.andThen((g: Game) => g.playMoves(
        E2 -> E4,
        E1 -> E2,
        F7 -> F5,
        E2 -> E3,
        E4 -> F5,
        E7 -> E5,
        E3 -> E4,
        E4 -> E5,
        G8 -> E7,
        F2 -> F3,
        F3 -> F4,
        G7 -> G5,
        F4 -> G5,
        E5 -> F6
      ))
      successGame .toOption must beSome.like { case game =>
        game.situation.checkMate === true
        game.situation.staleMate === false
      }
    }

    "Black can't castle when in check" in {
      import Pos._
      val game        = fenToGame(Monster.initialFen, Monster)
      val successGame = game.andThen((g: Game) => g.playMoves(
        F2 -> F4,
        F4 -> F5,
        G7 -> G6,
        E2 -> E4,
        D2 -> D4,
        F8 -> G7,
        C2 -> C4,
        E1 -> E2,
        G8 -> F6,
        E2 -> E3,
        E3 -> D3,
        B8 -> A6,
        F5 -> G6,
        D3 -> E3
      ))
      successGame .toOption must beSome.like { case game =>
        game.situation.moves.values.flatten.size === 3
      }
    }

    "Black can't castle through in check" in {
      import Pos._
      val game        = fenToGame(Monster.initialFen, Monster)
      val successGame = game.andThen((g: Game) => g.playMoves(
        F2 -> F4,
        F4 -> F5,
        G7 -> G6,
        E2 -> E4,
        D2 -> D4,
        F8 -> G7,
        C2 -> C4,
        E1 -> E2,
        G8 -> F6,
        E2 -> E3,
        E3 -> D3,
        B8 -> A6,
        F5 -> G6,
        D3 -> E3,
        H7 -> G6,
        E4 -> E5,
        E5 -> F6
      ))
      successGame .toOption must beSome.like { case game =>
        game.situation.moves.values.flatten.size === 26
      }
    }

    "Black Stalemate" in {
      import Pos._
      val game        = fenToGame(Monster.initialFen, Monster)
      val successGame = game andThen (
        _.playMoves(
          E2 -> E4,
          E1 -> E2,
          E7 -> E6,
          E2 -> E3,
          E3 -> F4,
          F8 -> A3,
          F4 -> G5,
          G5 -> H5,
          D7 -> D6,
          H5 -> G6,
          G6 -> G7,
          E8 -> D7,
          G7 -> H8,
          H8 -> H7,
          D7 -> C6,
          H7 -> G8,
          G8 -> F7,
          C6 -> B6,
          F7 -> E8,
          E8 -> D8,
          B6 -> A5,
          D8 -> C8,
          C8 -> B7,
          A5 -> A4,
          B7 -> B8,
          B8 -> A8,
          A7 -> A5,
          A8 -> B8,
          B8 -> C7,
          A3 -> C5,
          C7 -> D6,
          D6 -> E6,
          C5 -> D6,
          E6 -> D6,
          F2 -> F4,
          A4 -> A3,
          F4 -> F5,
          F5 -> F6,
          A3 -> A4,
          F6 -> F7,
          D6 -> D5
        )
      )
      successGame .toOption must beSome.like { case game =>
        game.situation.checkMate === false
        game.situation.staleMate === true
      }
    }

    "Test Every move can be loaded from fen" in {
      val gameFamily   = Monster.gameFamily
      val lib          = gameFamily.gameLogic
      val stratVariant = StratVariant(lib, Monster.key).get

      _testEveryMoveLoadFenIsometry(lib, StratFen(lib, Monster.initialFen.value), stratVariant)(
        List(
          "e2e4",
          "e1e2",
          "e7e6",
          "e2e3",
          "e3f4",
          "f8a3",
          "f4g5",
          "g5h5",
          "d7d6",
          "h5g6",
          "g6g7",
          "e8d7",
          "g7h8",
          "h8h7",
          "d7c6",
          "h7g8",
          "g8f7",
          "c6b6",
          "f7e8",
          "e8d8",
          "b6a5",
          "d8c8",
          "c8b7",
          "a5a4",
          "b7b8",
          "b8a8",
          "a7a5",
          "a8b8",
          "b8c7",
          "a3c5",
          "c7d6",
          "d6e6",
          "c5d6",
          "e6d6",
          "f2f4",
          "a4a3",
          "f4f5",
          "f5f6",
          "a3a4",
          "f6f7",
          "d6d5"
        ).map(uciStr => StratUci(lib, gameFamily, uciStr).get)
      ) .toOption must beSome.like { case gameData =>
        val fen1 = StratForsyth.>>(lib, gameData.game)
        val fen2 = StratForsyth.>>(lib, gameData.fenGame)
        fen1 === fen2
      }
    }

  }
}
