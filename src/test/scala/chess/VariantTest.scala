package strategygames.chess

import cats.syntax.option._

import strategygames.Status
import strategygames.chess.format.FEN
import strategygames.chess.Pos._
import strategygames.chess.variant._

class VariantTest extends ChessTest {

  val board = makeBoard

  "standard" should {

    "position pieces correctly" in {
      Standard.pieces must havePairs(
        A1 -> (Rook - White),
        B1 -> (Knight - White),
        C1 -> (Bishop - White),
        D1 -> (Queen - White),
        E1 -> (King - White),
        F1 -> (Bishop - White),
        G1 -> (Knight - White),
        H1 -> (Rook - White),
        A2 -> (Pawn - White),
        B2 -> (Pawn - White),
        C2 -> (Pawn - White),
        D2 -> (Pawn - White),
        E2 -> (Pawn - White),
        F2 -> (Pawn - White),
        G2 -> (Pawn - White),
        H2 -> (Pawn - White),
        A7 -> (Pawn - Black),
        B7 -> (Pawn - Black),
        C7 -> (Pawn - Black),
        D7 -> (Pawn - Black),
        E7 -> (Pawn - Black),
        F7 -> (Pawn - Black),
        G7 -> (Pawn - Black),
        H7 -> (Pawn - Black),
        A8 -> (Rook - Black),
        B8 -> (Knight - Black),
        C8 -> (Bishop - Black),
        D8 -> (Queen - Black),
        E8 -> (King - Black),
        F8 -> (Bishop - Black),
        G8 -> (Knight - Black),
        H8 -> (Rook - Black)
      )
    }

    "Identify insufficient mating material when called (bishop)." in {
      val position = FEN("krq5/bqqq4/qqr5/1qq5/8/8/8/3qB2K b - -")
      val game     = fenToGame(position, Standard)

      game should beValid.like { case game =>
        game.board.materialImbalance must_== -91
        game.situation.opponentHasInsufficientMaterial must beTrue
      }
    }

    "Identify sufficient mating material when called (bishop)." in {
      val position = FEN("8/7B/K7/2b5/1k6/8/8/8 b - -")
      val game     = fenToGame(position, Standard)

      game should beValid.like { case game =>
        game.board.materialImbalance must_== 0
        game.situation.opponentHasInsufficientMaterial must beFalse
      }
    }

    "Identify insufficient mating material when called (knight)." in {
      val position = FEN("8/3k4/2q5/8/8/K1N5/8/8 b - -")
      val game     = fenToGame(position, Standard)

      game should beValid.like { case game =>
        game.board.materialImbalance must_== -6
        game.situation.opponentHasInsufficientMaterial must beTrue
      }
    }
  }

  "chess960" should {

    "position pieces correctly" in {
      Chess960.pieces must havePair(A2 -> (Pawn - White))
    }

    "initialize the board with castling rights" in {
      Board.init(Chess960).history.castles must_== Castles.all
    }
  }

  "kingOfTheHill" should {
    "detect win" in {
      "not" in {
        Game(
          """
PPk
K
""".kingOfTheHill,
          White
        ).situation.end must beFalse
      }
      "regular checkMate" in {
        val game = Game(
          """
PP
K  r
""".kingOfTheHill,
          White
        )

        game.situation.end must beTrue
        game.situation.winner must beSome.like { case color =>
          color == Black
        }
      }
      "centered black king" in {
        val sit = Game(
          """
   k

PP
   K
""".kingOfTheHill,
          White
        ).situation
        sit.end must beTrue
        sit.winner must beSome.like { case color =>
          color == Black
        }

      }
    }

    "initialize the board with castling rights" in {
      Board.init(KingOfTheHill).history.castles must_== Castles.all
    }
  }

  "threeCheck" should {
    "detect win" in {
      "not" in {
        Game(
          """
PPk
K
""".threeCheck,
          White
        ).situation.end must beFalse
      }
      "regular checkMate" in {
        val game = Game(
          """
PP
K  r
""".threeCheck,
          White
        )
        game.situation.end must beTrue
        game.situation.winner must beSome.like { case color =>
          color == Black
        }
      }
      "1 check" in {
        val game = Game(Board init ThreeCheck)
          .playMoves(
            E2 -> E4,
            E7 -> E6,
            D2 -> D4,
            F8 -> B4
          )
          .toOption
          .get
        game.situation.end must beFalse
      }
      "2 checks" in {
        val game = Game(Board init ThreeCheck)
          .playMoves(
            E2 -> E4,
            E7 -> E6,
            D2 -> D4,
            F8 -> B4,
            C2 -> C3,
            B4 -> C3
          )
          .toOption
          .get
        game.situation.end must beFalse
      }
      "3 checks" in {
        val game = Game(Board init ThreeCheck)
          .playMoves(
            E2 -> E4,
            E7 -> E6,
            D2 -> D4,
            F8 -> B4,
            C2 -> C3,
            B4 -> C3,
            B1 -> C3,
            D8 -> H4,
            A2 -> A3,
            H4 -> F2
          )
          .toOption
          .get
        game.situation.end must beTrue

        game.situation.winner must beSome.like { case color =>
          color == Black
        }
      }
    }

    "Not force a draw when there is insufficient mating material" in {
      val position = FEN("8/6K1/8/8/8/8/k6p/8 b - - 1 39")
      val game     = fenToGame(position, ThreeCheck)

      val successGame = game flatMap (_.playMove(Pos.H2, Pos.H1, Knight.some))

      successGame must beValid.like { case game =>
        game.situation.end must beFalse
      }
    }

    "Force a draw when there are only kings remaining" in {
      val position = FEN("8/6K1/8/8/8/8/k7/8 b - -")
      val game     = fenToGame(position, ThreeCheck)

      game must beValid.like { case game =>
        game.situation.end must beTrue
        game.situation.status must beEqualTo(Status.Draw.some)
      }
    }

    "initialize the board with castling rights" in {
      Board.init(KingOfTheHill).history.castles must_== Castles.all
    }
  }

"fiveCheck" should {
    "detect win" in {
      "not" in {
        Game(
          """
PPk
K
""".fiveCheck,
          White
        ).situation.end must beFalse
      }
      "regular checkMate" in {
        val game = Game(
          """
PP
K  r
""".fiveCheck,
          White
        )
        game.situation.end must beTrue
        game.situation.winner must beSome.like { case color =>
          color == Black
        }
      }
      "1 check" in {
        val game = Game(Board init FiveCheck)
          .playMoves(
            E2 -> E4,
            E7 -> E6,
            D2 -> D4,
            F8 -> B4
          )
          .toOption
          .get
        game.situation.end must beFalse
      }
      "2 checks" in {
        val game = Game(Board init FiveCheck)
          .playMoves(
            E2 -> E4,
            E7 -> E6,
            D2 -> D4,
            F8 -> B4,
            C2 -> C3,
            B4 -> C3
          )
          .toOption
          .get
        game.situation.end must beFalse
      }
      "3 checks" in {
        val game = Game(Board init FiveCheck)
          .playMoves(
            E2 -> E4,
            E7 -> E6,
            D2 -> D4,
            F8 -> B4,
            C2 -> C3,
            B4 -> C3,
            D1 -> D2,
            C3 -> D2
          )
          .toOption
          .get
        game.situation.end must beFalse
      }
      "4 checks" in {
        val game = Game(Board init FiveCheck)
          .playMoves(
            E2 -> E4,
            E7 -> E6,
            D2 -> D4,
            F8 -> B4,
            C2 -> C3,
            B4 -> C3,
            D1 -> D2,
            C3 -> D2,
            E1 -> D2,
            D8 -> G5
          )
          .toOption
          .get
        game.situation.end must beFalse
      }
      "5 checks" in {
        val game = Game(Board init FiveCheck)
          .playMoves(
            E2 -> E4,
            E7 -> E6,
            D2 -> D4,
            F8 -> B4,
            C2 -> C3,
            B4 -> C3,
            D1 -> D2,
            C3 -> D2,
            E1 -> D2,
            D8 -> G5,
            F2 -> F4,
            G5 -> F4
          )
          .toOption
          .get
        game.situation.end must beTrue

        game.situation.winner must beSome.like { case color =>
          color == Black
        }
      }
    }

    "Not force a draw when there is insufficient mating material" in {
      val position = FEN("8/6K1/8/8/8/8/k6p/8 b - - 1 39")
      val game     = fenToGame(position, FiveCheck)

      val successGame = game flatMap (_.playMove(Pos.H2, Pos.H1, Knight.some))

      successGame must beValid.like { case game =>
        game.situation.end must beFalse
      }
    }

    "Force a draw when there are only kings remaining" in {
      val position = FEN("8/6K1/8/8/8/8/k7/8 b - -")
      val game     = fenToGame(position, FiveCheck)

      game must beValid.like { case game =>
        game.situation.end must beTrue
        game.situation.status must beEqualTo(Status.Draw.some)
      }
    }
  }

  "racingKings" should {
    "call it stalemate when there is no legal move" in {
      val position = FEN("8/8/8/8/3K4/8/1k6/b7 b - - 5 3")
      val game     = fenToGame(position, RacingKings)

      game must beValid.like { case game =>
        game.situation.end must beTrue
        game.situation.staleMate must beTrue
      }
    }

    "should not draw because of insufficient material" in {
      val position = FEN("8/8/8/8/5K2/8/2k5/8 w - - 0 1")
      val game     = fenToGame(position, RacingKings)

      game must beValid.like { case game =>
        game.situation.end must beFalse
        game.situation.staleMate must beFalse
      }
    }

    "should recognize a king in the goal" in {
      "white" in {
        val position = FEN("2K5/8/6k1/8/8/8/8/Q6q w - - 0 1")
        val game     = fenToGame(position, RacingKings)

        game must beValid.like { case game =>
          game.situation.end must beTrue
          game.situation.winner must beSome.like { case color =>
            color == White
          }
        }
      }

      "black" in {
        val position = FEN("6k1/8/8/8/8/2r5/1KB5/2B5 w - - 0 1")
        val game     = fenToGame(position, RacingKings)

        game must beValid.like { case game =>
          game.situation.end must beTrue
          game.situation.winner must beSome.like { case color =>
            color == Black
          }
        }
      }
    }

    "should give black one more move" in {
      "when white is in the goal" in {
        val position = FEN("2K5/5k2/8/8/8/8/8/8 b - - 0 1")
        val game     = fenToGame(position, RacingKings)

        game must beValid.like { case game =>
          game.situation.end must beFalse
        }
      }

      "but not if it does not matter anyway" in {
        val position = FEN("2K5/8/2n1nk2/8/8/8/8/4r3 b - - 0 1")
        val game     = fenToGame(position, RacingKings)

        game must beValid.like { case game =>
          game.situation.end must beTrue
          game.situation.winner must beSome.like { case color =>
            color == White
          }
        }
      }
    }

    "should call it a draw with both kings in the goal" in {
      val position = FEN("2K2k2/8/8/8/8/1b6/1b6/8 w - - 0 1")
      val game     = fenToGame(position, RacingKings)

      game must beValid.like { case game =>
        game.situation.end must beTrue
        game.situation.status must beEqualTo(Status.Draw.some)
      }
    }

    "initialize the board without castling rights" in {
      Board.init(RacingKings).history.castles.isEmpty must beTrue
    }
  }

  "antichess" should {
    "initialize the board without castling rights" in {
      Board.init(Antichess).history.castles.isEmpty must beTrue
    }

    "calculate material imbalance" in {
      val position = FEN("8/p7/8/8/2B5/b7/PPPK2PP/RNB3NR w - - 1 16")
      val game     = fenToGame(position, Antichess)

      game must beValid.like { case game =>
        game.situation.board.materialImbalance must_== -20
      }
    }
  }

  "horde" should {
    "initialize the board with black castling rights" in {
      Board.init(Horde).history.castles must_== Castles("kq")
    }
  }
}
