package strategygames.chess

import cats.syntax.option._

import strategygames.{ P2, Status }
import strategygames.chess.variant.Atomic
import strategygames.chess.format.FEN

class AtomicVariantTest extends ChessTest {

  "Atomic chess" should {

    "Must explode surrounding non pawn pieces on capture" in {
      val fenPosition     = FEN("rnbqkbnr/1ppppp1p/p5p1/8/8/1P6/PBPPPPPP/RN1QKBNR w KQkq -")
      val maybeGame       = fenToGame(fenPosition, Atomic)
      val explodedSquares = List(Pos.H8, Pos.G8)
      val intactPawns     = List(Pos.F7, Pos.G6, Pos.H7)

      val explosionGame = maybeGame.andThen((g: Game) => g.playMoves((Pos.B2, Pos.H8)))

      explosionGame.toOption must beSome.like { case game =>
        explodedSquares.forall(pos => game.situation.board(pos).isEmpty) must beTrue
        intactPawns.forall(pos => game.situation.board(pos).isDefined) must beTrue
      }
    }

    "Must explode all surrounding non pawn pieces on capture (contrived situation)" in {
      val fenPosition     = FEN("k7/3bbn2/3rqn2/3qr3/8/7B/8/1K6 w - -")
      val maybeGame       = fenToGame(fenPosition, Atomic)
      val explodedSquares = List(Pos.D5, Pos.E5, Pos.D6, Pos.E6, Pos.F6, Pos.D7, Pos.E7, Pos.F7)

      val explosionGame = maybeGame.andThen((g: Game) => g.playMoves((Pos.H3, Pos.E6)))

      explosionGame.toOption must beSome.like { case game =>
        explodedSquares.forall(pos => game.situation.board(pos).isEmpty) must beTrue
      }
    }

    "Must explode all surrounding non pawn pieces on capture (contrived situation with bottom right position)" in {
      val fenPosition     = FEN("k7/3bbn2/3rqn2/4rq2/8/1B6/8/K7 w - -")
      val maybeGame       = fenToGame(fenPosition, Atomic)
      val explodedSquares = List(Pos.F5, Pos.E5, Pos.D6, Pos.E6, Pos.F6, Pos.D7, Pos.E7, Pos.F7)

      val explosionGame = maybeGame.andThen((g: Game) => g.playMoves((Pos.B3, Pos.E6)))

      explosionGame.toOption must beSome.like { case game =>
        explodedSquares.forall(pos => game.situation.board(pos).isEmpty) must beTrue
      }
    }

    "Not allow a king to capture a piece" in {
      val fenPosition = FEN("8/8/8/1k6/8/8/8/1Kr5 w - -")
      val maybeGame   = fenToGame(fenPosition, Atomic)

      val errorGame = maybeGame.andThen((g: Game) => g.playMoves((Pos.B1, Pos.C1)))

      errorGame.isInvalid must beTrue
    }

    "The game must end with the correct winner when a king explodes in the perimeter of a captured piece" in {
      val fenPosition = FEN("rnb1kbnr/ppp1pppp/8/3q4/8/7P/PPPP1PP1/RNBQKBNR b KQkq -")
      val maybeGame   = fenToGame(fenPosition, Atomic)

      val gameWin = maybeGame.andThen((g: Game) => g.playMoves((Pos.D5, Pos.D2)))

      gameWin.toOption must beSome.like { case winningGame =>
        winningGame.situation.end must beTrue
        winningGame.situation.variantEnd must beTrue
        winningGame.situation.winner must beSome.like { case winner =>
          winner == P2
        }
      }
    }

    "The game must end by a traditional checkmate (atomic mate)" in {
      val fenPosition = FEN("1k6/8/8/8/8/8/PP5r/K7 b - -")
      val maybeGame   = fenToGame(fenPosition, Atomic)

      val gameWin = maybeGame.andThen((g: Game) => g.playMoves((Pos.H2, Pos.H1)))

      gameWin.toOption must beSome.like { case winningGame =>
        winningGame.situation.end must beTrue
        winningGame.situation.variantEnd must beFalse
        winningGame.situation.winner must beSome.like { case player => player == P2 }
      }
    }

    "Must be a stalemate if a king could usually take a piece, but can't because it would explode" in {
      val positionFen = FEN("k7/8/1R6/8/8/8/8/5K2 w - -")
      val maybeGame   = fenToGame(positionFen, Atomic)

      val gameWin = maybeGame.andThen((g: Game) => g.playMoves((Pos.B6, Pos.B7)))

      gameWin.toOption must beSome.like { case game =>
        game.situation.moves
        game.situation.end must beTrue
        game.situation.staleMate must beTrue
      }
    }

    "It is stalemate if there are only two kings and two opposite square coloured bishops remaining" in {
      val positionFen = FEN("4K3/8/2b5/8/8/8/5B2/3k4 b - -")
      val game        = fenToGame(positionFen, Atomic)

      game.toOption must beSome.like { case game =>
        game.situation.end must beTrue
        game.situation.autoDraw must beTrue
        game.situation.winner must beNone
        game.situation.status must beSome.like { case status =>
          status == Status.Draw
        }
      }
    }

    "In atomic check, an opportunity at exploding the opponent's king takes priority over getting out of check" in {
      val positionFen    = FEN("k1K5/pp5R/8/8/3Q4/P7/1P6/2r5 w - -")
      val threatenedGame = fenToGame(positionFen, Atomic)

      threatenedGame.toOption must beSome.like { case game =>
        game.situation.check must beTrue
        game.situation.end must beFalse
        game.situation.winner must beNone
        game.situation.moves must haveKeys(Pos.D4, Pos.H7, Pos.C8)
        game.situation.moves.get(Pos.D4) must beSome.like { case moves =>
          // The queen can defend the king from check
          moves.find(_.dest == Pos.A7) must beSome

          // Or explode the opponent's king to win the game
          moves.find(_.dest == Pos.C4) must beSome
        }

        // The king cannot capture a piece in the perimeter of the opponent king, exploding itself
        game.situation.moves.get(Pos.C8) must beSome.like { case mvs =>
          mvs.forall(_.captures) must beFalse
        }

        // The rook cannot capture, as that would result in our own king exploding
        game.situation.moves.get(Pos.H7) must beSome.like { case mvs =>
          mvs.find(_.captures) must beNone
          // It can, however, defend the king
          mvs.find(_.dest == Pos.C7) must beSome
          mvs.size must beEqualTo(1)
        }
      }
    }

    "In atomic mate, an opportunity at exploding the opponent's king takes priority over getting out of mate" in {
      val positionFen = FEN("k1r5/pp5R/8/8/3Q4/8/PP6/K7 b - -")
      val game        = fenToGame(positionFen, Atomic)

      val mateThreatedGame = game.andThen((g: Game) => g.playMoves((Pos.C8, Pos.C1)))

      mateThreatedGame.toOption must beSome.like { case game =>
        game.situation.end must beFalse
        game.situation.winner must beNone
        game.situation.moves must haveKeys(Pos.D4, Pos.H7)
        game.situation.moves.values.forall(_.forall(_.captures)) must beTrue
      }
    }

    "In atomic chess a king may walk into a square that is in the perimeter of the opponent king since it can't capture" in {
      val positionFen = FEN("3k4/8/3K4/8/8/8/7r/8 w - -")
      val game        = fenToGame(positionFen, Atomic)

      val successGame = game.andThen((g: Game) => g.playMoves((Pos.D6, Pos.D7)))

      successGame.toOption must beSome.like { case game =>
        game.situation.board(Pos.D7) must beSome
        game.situation.check must beFalse
      }
    }

    "Draw on knight and king vs king" in {
      val position = FEN("8/1n6/8/8/8/8/k7/2K1b2R w - -")
      val game     = fenToGame(position, Atomic)

      val successGame = game.andThen((g: Game) => g.playMoves((Pos.H1, Pos.E1)))

      successGame.toOption must beSome.like { case game =>
        game.situation.end must beTrue
        game.situation.status must beSome.like { case status =>
          status == Status.Draw
        }
      }
    }

    "Draw on bishop and king vs king" in {
      val position = FEN("8/1b6/8/8/8/8/k7/2K1n2R w - -")
      val game     = fenToGame(position, Atomic)

      val successGame = game.andThen((g: Game) => g.playMoves((Pos.H1, Pos.E1)))

      successGame.toOption must beSome.like { case game =>
        game.situation.end must beTrue
        game.situation.status must beSome.like { case status =>
          status == Status.Draw
        }
      }
    }

    "Draw on a rook and king vs king" in {
      val position    = FEN("8/8/8/8/8/8/N4r2/5k1K b - -")
      val game        = fenToGame(position, Atomic)
      val successGame = game.andThen((g: Game) => g.playMoves((Pos.F2, Pos.A2)))
      successGame.toOption must beSome.like { case game =>
        game.situation.end must beTrue
        game.situation.status must beSome.like { case status =>
          status == Status.Draw
        }
      }
    }

    "Draw on a king vs a king" in {
      val position    = FEN("6r1/8/8/1k6/8/8/2K5/6R1 w - -")
      val game        = fenToGame(position, Atomic)
      val successGame = game.andThen((g: Game) => g.playMoves((Pos.G1, Pos.G8)))

      successGame.toOption must beSome.like { case game =>
        game.situation.end must beTrue
        game.situation.status must beSome.like { case status =>
          status == Status.Draw
        }
      }
    }

    "It should not be possible to capture a piece resulting in your own king exploding" in {
      val position    = FEN("rnbqkbnr/pppNp1pp/5p2/3p4/8/8/PPPPPPPP/RNBQKB1R b KQkq - 1 3")
      val game        = fenToGame(position, Atomic)
      val failureGame = game.andThen((g: Game) => g.playMoves((Pos.D8, Pos.D7)))

      failureGame.isInvalid must beTrue
    }

    "In an en-passant capture, the pieces surrounding the pawn's destination are exploded along with the pawn" in {
      val position  = FEN("4k3/2pppb1p/3r1r2/3P1b2/8/8/1K6/4NB2 b - -")
      val game      = fenToGame(position, Atomic)
      val validGame = game.andThen((g: Game) => g.playMoves((Pos.E7, Pos.E5), (Pos.D5, Pos.E6)))

      validGame.toOption must beSome.like { case game =>
        game.board(Pos.E6) must beNone // The pawn that captures during en-passant should explode
        // Every piece surrounding the en-passant destination square that is not a pawn should be empty
        Atomic
          .surroundingPositions(Pos.E6)
          .forall(pos => game.board(pos).isEmpty || pos == Pos.E7 || pos == Pos.D7) must beTrue
      }
    }

    "Verify it is not possible to walk into check" in {
      val position = FEN("rnbqkbnr/ppp1pppp/8/3pN3/8/8/PPPPPPPP/RNBQKB1R b KQkq - 1 2")
      val game     = fenToGame(position, Atomic)

      val failureGame = game.andThen((g: Game) => g.playMoves((Pos.E8, Pos.D7)))

      failureGame.isInvalid must beTrue
    }

    "Verify that a king can move into what would traditionally be check when touching the opponent king" in {
      val position = FEN("r1bq1bnr/pppp1ppp/5k2/4p3/4P1K1/8/PPPP1PPP/RNBQ1B1R b - - 5 6")
      val game     = fenToGame(position, Atomic)

      val successGame = game.andThen((g: Game) => g.playMoves((Pos.F6, Pos.F5)))

      successGame.isValid === true
    }

    "After kings have been touching, and one moves away, a king that was protected is under attack again" in {
      val position = FEN("r1bq1bnr/pppp1ppp/5k2/4p3/4P1K1/8/PPPP1PPP/RNBQ1B1R b - - 5 6")
      val game     = fenToGame(position, Atomic)

      val successGame = game.andThen((g: Game) => g.playMoves((Pos.F6, Pos.F5), (Pos.G4, Pos.H3)))

      successGame.toOption must beSome.like { case game =>
        game.situation.check must beTrue
      }
    }

    "Can move into discovered check in order to explode the opponent's king" in {
      val position = FEN("R2r2k1/1p2ppbp/8/6p1/2p5/5P1N/P2Pn1PP/2B1K2R b K - 3 19")
      val game     = fenToGame(position, Atomic)

      val successGame = game.andThen((g: Game) => g.playMoves((Pos.D8, Pos.D2)))

      successGame.toOption must beSome.like { case game =>
        game.situation.end must beTrue
        game.situation.winner must beSome.like { case player =>
          player == P2
        }
      }
    }

    "It must be possible to remove yourself from check by exploding a piece next to the piece threatening the king" in {
      val position = FEN("5k1r/p1ppq1pp/5p2/1B6/1b3P2/2P5/PP4PP/RNB1K2R w KQ - 0 12")
      val game     = fenToGame(position, Atomic)

      val successGame = game.andThen((g: Game) => g.playMoves((Pos.B5, Pos.D7)))

      successGame.toOption must beSome.like { case game =>
        game.situation.check must beFalse
      }
    }

    "It should not be possible to explode a piece, exploding a piece next to it which would result in a check" in {
      val position = FEN("r1b1k2r/pp1pBppp/2p1p2n/q3P3/B2P4/2N2Q2/PPn2PPP/R3K1NR w KQkq - 9 11")
      val game     = fenToGame(position, Atomic)

      val failureGame = game.andThen((g: Game) => g.playMoves((Pos.A4, Pos.C2)))

      failureGame.isInvalid must beTrue
    }

    "Game is not a draw when the last piece a player has other than their king is a pawn that is blocked by a mobile piece" in {
      val position = FEN("3Q4/2b2k2/5P2/8/8/8/6K1/8 b - - 0 57")
      val game     = fenToGame(position, Atomic)

      val successGame = game.andThen((g: Game) => g.playMoves((Pos.C7, Pos.D8)))

      successGame.toOption must beSome.like { case game =>
        game.situation.end must beFalse
      }
    }

    // This would probably be harmless, but there might be a use case where the count of available moves matters,
    // or similar, so best to code defensively.
    "There are no repeated moves in the list of available moves for the situation" in {

      // Situation where the queen can capture a pawn to both win and remove itself from check
      val position    = FEN("k1r5/pp5Q/8/8/8/8/PP6/2K5 w - -")
      val successGame = fenToGame(position, Atomic)

      successGame.toOption must beSome.like { case game =>
        val moves = game.situation.moves.get(Pos.H7)

        moves must beSome.like { case queenMoves =>
          queenMoves.size must beEqualTo(queenMoves.toSet.size)
        }
      }

    }

    "End move regression" in {
      import Pos._
      "from init" in {
        val game        = fenToGame(format.Forsyth.initial, Atomic)
        val successGame = game.andThen((g: Game) => g.playMoves(
          E2 -> E4,
          D7 -> D5,
          G1 -> F3,
          D5 -> E4,
          F1 -> B5,
          D8 -> D2
        ))
        successGame.toOption must beSome.like { case game =>
          game.situation.variantEnd must beTrue
        }
      }
      "from position" in {
        val game        = fenToGame(FEN("rnbqkbnr/ppp1pppp/8/1B6/8/8/PPPP1PPP/RNBQK2R b KQkq - 1 1"), Atomic)
        val successGame = game.andThen((g: Game) => g.playMoves(D8 -> D2))
        successGame.toOption must beSome.like { case game =>
          game.situation.variantEnd must beTrue
        }
      }

    }

    "Not escaping a check that would blow up both kings" in {
      val position = FEN("rnbq1bnr/pp1pp1pp/8/2pk1p2/3K1P2/P6P/1PPPP1P1/RNBQ1BNR b - - 0 6")
      val game     = fenToGame(position, Atomic)

      val newGame = game.andThen((g: Game) => g.playMoves(
        Pos.A7 -> Pos.A6
      ))

      newGame.isValid === true
    }

    "Identify that a player does not have sufficient material to win when they only have a king" in {
      val position = FEN("8/8/8/8/7p/2k4q/2K3P1/8 w - - 19 54")
      val game     = fenToGame(position, Atomic)

      game.toOption must beSome.like { case game =>
        game.situation.end must beFalse
      }

      val drawGame = game.andThen((g: Game) => g.playMoves(Pos.G2 -> Pos.H3))

      drawGame.toOption must beSome.like { case game =>
        game.situation.opponentHasInsufficientMaterial must beTrue
      }

    }

    "An automatic draw in a closed position with only kings and pawns which cannot move" in {
      val position     = FEN("8/8/6p1/3K4/6P1/2k5/8/8 w - -")
      val originalGame = fenToGame(position, Atomic)

      val game = originalGame.andThen((g: Game) => g.playMoves(Pos.G4 -> Pos.G5))

      game.toOption must beSome.like { case game =>
        game.situation.autoDraw must beTrue
        game.situation.end must beTrue
      }

    }

    "Not draw inappropriately on bishops vs bishops (where an explosion taking out the king is possible)" in {
      val position = FEN("B2BBBB1/7P/8/8/8/8/3kb3/4K3 w - - 1 53")
      val game     = fenToGame(position, Atomic)
      val newGame  = game.andThen((g: Game) => g.playMove(
        Pos.H7,
        Pos.H8,
        Bishop.some
      ))

      newGame.toOption must beSome.like { case game =>
        game.situation.end must beFalse
      }
    }

    "Not draw inappropriately on two bishops (of both square players)" in {
      val position = FEN("8/5k2/8/8/8/8/4pK2/5b2 b - - 1 44")
      val game     = fenToGame(position, Atomic)
      val newGame  = game.andThen((g: Game) => g.playMove(
        Pos.E2,
        Pos.E1,
        Bishop.some
      ))

      newGame.toOption must beSome.like { case game =>
        game.situation.end must beFalse
      }
    }

    "Not draw inappropriately on bishop and knight" in {
      val position = FEN("8/5k2/8/8/8/8/4pK2/5b2 b - - 1 44")
      val game     = fenToGame(position, Atomic)
      val newGame  = game.andThen((g: Game) => g.playMove(
        Pos.E2,
        Pos.E1,
        Knight.some
      ))

      newGame.toOption must beSome.like { case game =>
        game.situation.end must beFalse
      }
    }

    "Not draw inappropriately on three bishops (of both square players)" in {
      val position = FEN("8/5k2/8/8/8/8/4pKB1/5B2 b - - 1 44")
      val game     = fenToGame(position, Atomic)
      val newGame  = game.andThen((g: Game) => g.playMove(
        Pos.E2,
        Pos.E1,
        Bishop.some
      ))

      newGame.toOption must beSome.like { case game =>
        game.situation.end must beFalse
      }
    }

    "Not draw inappropriately on three bishops (of both square players)" in {
      val position = FEN("8/5k2/8/8/8/8/4pKB1/6B1 b - - 1 44")
      val game     = fenToGame(position, Atomic)
      val newGame  = game.andThen((g: Game) => g.playMove(
        Pos.E2,
        Pos.E1,
        Bishop.some
      ))

      newGame.toOption must beSome.like { case game =>
        game.situation.end must beFalse
      }
    }

    "Not draw inappropriately on two bishops and a knight" in {
      val position = FEN("8/5k2/8/8/8/8/4pKB1/6N1 b - - 1 44")
      val game     = fenToGame(position, Atomic)
      val newGame  = game.andThen((g: Game) => g.playMove(
        Pos.E2,
        Pos.E1,
        Bishop.some
      ))

      newGame.toOption must beSome.like { case game =>
        game.situation.end must beFalse
      }
    }

    "Not draw inappropriately on two bishops and a knight" in {
      val position = FEN("8/5k2/8/8/8/8/4pKN1/6B1 b - - 1 44")
      val game     = fenToGame(position, Atomic)
      val newGame  = game.andThen((g: Game) => g.playMove(
        Pos.E2,
        Pos.E1,
        Bishop.some
      ))

      newGame.toOption must beSome.like { case game =>
        game.situation.end must beFalse
      }
    }

    "Not draw inappropriately on two knights and a bishop" in {
      val position = FEN("8/5k2/8/8/8/8/4pKN1/6N1 b - - 1 44")
      val game     = fenToGame(position, Atomic)
      val newGame  = game.andThen((g: Game) => g.playMove(
        Pos.E2,
        Pos.E1,
        Bishop.some
      ))

      newGame.toOption must beSome.like { case game =>
        game.situation.end must beFalse
      }
    }

    "Not draw inappropriately on three knights (of two players)" in {
      val position = FEN("8/5k2/8/8/8/8/4pKN1/6N1 b - - 1 44")
      val game     = fenToGame(position, Atomic)
      val newGame  = game.andThen((g: Game) => g.playMove(
        Pos.E2,
        Pos.E1,
        Knight.some
      ))

      newGame.toOption must beSome.like { case game =>
        game.situation.end must beFalse
      }
    }

    "Not draw inappropriately on three knights (of two players)" in {
      val position = FEN("8/5k2/8/8/8/8/4pKN1/6n1 b - - 1 44")
      val game     = fenToGame(position, Atomic)
      val newGame  = game.andThen((g: Game) => g.playMove(
        Pos.E2,
        Pos.E1,
        Knight.some
      ))

      newGame.toOption must beSome.like { case game =>
        game.situation.end must beFalse
      }
    }

    "Not draw inappropriately on three knights (of the same player)" in {
      val position = FEN("8/5k2/8/8/8/8/4pKn1/6n1 b - - 1 44")
      val game     = fenToGame(position, Atomic)
      val newGame  = game.andThen((g: Game) => g.playMove(
        Pos.E2,
        Pos.E1,
        Knight.some
      ))

      newGame.toOption must beSome.like { case game =>
        game.situation.end must beFalse
      }
    }

    "Allow castling with touching kings and rook shielding final attack" in {
      val position = FEN("8/8/8/8/8/8/4k3/R3K2r w Q - 0 1")
      val game     = fenToGame(position, Atomic)
      val newGame  = game.andThen((g: Game) => g.playMove(Pos.E1, Pos.C1))

      newGame.toOption must beSome.like { case game =>
        game.board(Pos.C1) must beEqualTo(King.p1.some)
        game.board(Pos.D1) must beEqualTo(Rook.p1.some)
      }
    }

    "Disallow castling through atomic check" in {
      val position  = FEN("8/8/8/8/8/8/5k2/R3K2r w Q - 0 1")
      val game      = fenToGame(position, Atomic)
      val errorGame = game.andThen((g: Game) => g.playMove(Pos.E1, Pos.C1))
      errorGame.isInvalid === true
    }

    "Disallow castling into atomic check" in {
      val position  = FEN("4k3/8/8/8/8/8/8/rR2K3 w Q - 0 1")
      val game      = fenToGame(position, Atomic)
      val errorGame = game.andThen((g: Game) => g.playMove(Pos.E1, Pos.B1))
      errorGame.isInvalid === true
    }

    "An automatic draw in a closed position with kings, pawns and a pawnitized bishop" in {
      val position = FEN("8/8/2k1p3/5p2/4PP2/1b6/4K3/8 w - - 0 1")
      val game     = fenToGame(position, Atomic)
      val newGame  = game.andThen((g: Game) => g.playMove(Pos.E4, Pos.E5))

      newGame.toOption must beSome.like { case game =>
        game.situation.autoDraw must beTrue
        game.situation.end must beTrue
      }
    }

    "Not draw inappropriately on blocked pawns with a non-pawnitized bishop" in {
      val position = FEN("8/8/2k5/5p2/8/2b2P2/8/3K4 w - - 0 1")
      val game     = fenToGame(position, Atomic)
      val newGame  = game.andThen((g: Game) => g.playMove(Pos.F3, Pos.F4))

      newGame.toOption must beSome.like { case game =>
        game.situation.autoDraw must beFalse
        game.situation.end must beFalse
      }
    }

    "Not draw inappropriately if both sides have a pawnitized bishop" in {
      val position = FEN("6bk/4B2p/8/7P/4K3/8/8/8 w - - 0 1")
      val game     = fenToGame(position, Atomic)
      val newGame  = game.andThen((g: Game) => g.playMove(Pos.H5, Pos.H6))

      newGame.toOption must beSome.like { case game =>
        game.situation.autoDraw must beFalse
        game.situation.end must beFalse
      }
    }

    "Checkmate overrides closed position" in {
      val position = FEN("8/8/b1p5/kpP5/p3K3/PP6/8/8 w - - 0 1")
      val game     = fenToGame(position, Atomic)
      val newGame  = game.andThen((g: Game) => g.playMove(Pos.B3, Pos.B4))

      newGame.toOption must beSome.like { case game =>
        game.situation.autoDraw must beFalse
        game.situation.end must beTrue
      }
    }
  }
}
