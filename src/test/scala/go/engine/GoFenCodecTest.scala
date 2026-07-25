package strategygames.go.engine

import org.specs2.mutable.Specification

import strategygames.go.format.Forsyth
import strategygames.go.variant.{ Go13x13, Go19x19, Go9x9 }

class GoFenCodecTest extends Specification {

  import GoEngineTestSupport._

  private val nineByNineKomi     = 5.5
  private val nineteenByNineKomi = 7.5

  private def renderAfter(size: Int, komi: Double, keys: List[String]): String =
    GoFen.render(keys.foldLeft(GoGame.initial(size, komi)) { (game, key) =>
      game.play(if (key == "pass") game.state.passMove else engineMove(size, key))
    })

  private def reparsed(fen: String): String =
    GoFen.parse(fen).map(GoFen.render).getOrElse(s"parse failed: $fen")

  "initial fen emit" should {
    "match the 9x9 variant constant" in {
      GoFen.render(GoGame.initial(9, nineByNineKomi)) === Go9x9.initialFen.value
    }
    "match the 13x13 variant constant" in {
      GoFen.render(GoGame.initial(13, nineteenByNineKomi)) === Go13x13.initialFen.value
    }
    "match the 19x19 variant constant" in {
      GoFen.render(GoGame.initial(19, nineteenByNineKomi)) === Go19x19.initialFen.value
    }
    "match Forsyth.initial" in {
      GoFen.render(GoGame.initial(19, nineteenByNineKomi)) === Forsyth.initial.value
    }
  }

  "fen emit after scripted 19x19 openings" should {
    "match the recorded fen after one drop" in {
      renderAfter(19, nineteenByNineKomi, List("a1")) ===
        "19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/S18[SSSSSSSSSSssssssssss] w - 3610 75 0 0 75 0 1"
    }
    "match the recorded fen after two drops" in {
      renderAfter(19, nineteenByNineKomi, List("a1", "k1")) ===
        "19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/S9s8[SSSSSSSSSSssssssssss] b - 10 85 0 0 75 0 2"
    }
    "match the recorded fen after three drops" in {
      renderAfter(19, nineteenByNineKomi, List("a1", "k1", "a2")) ===
        "19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/S18/S9s8[SSSSSSSSSSssssssssss] w - 20 85 0 0 75 0 2"
    }
    "match the recorded fen after four drops" in {
      renderAfter(19, nineteenByNineKomi, List("a1", "k1", "a2", "a3")) ===
        "19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/s18/S18/S9s8[SSSSSSSSSSssssssssss] b - 20 95 0 0 75 0 3"
    }
    "match the recorded fen of an interleaved first rank" in {
      renderAfter(19, nineteenByNineKomi, List("a1", "g1", "d1", "h1", "e1")) ===
        "19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/S2SS1ss11[SSSSSSSSSSssssssssss] w - 30 95 0 0 75 0 3"
    }
    "match the recorded fen of a split first rank" in {
      renderAfter(19, nineteenByNineKomi, List("a1", "h1", "e1", "b1")) ===
        "19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/Ss2S2s11[SSSSSSSSSSssssssssss] b - 20 95 0 0 75 0 3"
    }
  }

  "fen parse" should {
    "round trip every emitted initial fen" in {
      forall(List((9, nineByNineKomi), (13, nineteenByNineKomi), (19, nineteenByNineKomi))) {
        case (size, komi) =>
          val fen = GoFen.render(GoGame.initial(size, komi))
          reparsed(fen) === fen
      }
    }
    "round trip every 9x9 handicap board" in {
      forall(1 to 9) { handicap =>
        val fen = s"${Go9x9.boardFenFromHandicap(handicap)}[SSSSSSSSSSssssssssss] w - 0 55 0 0 55 0 1"
        reparsed(reparsed(fen)) === reparsed(fen)
      }
    }
    "preserve the handicap stones, turn and komi" in {
      forall(1 to 9) { handicap =>
        val fen  = s"${Go9x9.boardFenFromHandicap(handicap)}[SSSSSSSSSSssssssssss] w - 0 55 0 0 55 0 1"
        val game = GoFen.parse(fen).toOption
        (game.map(_.state.turn) === Some("w")) and
          (game.map(_.komiTenths) === Some(55)) and
          (game.map(g => (0 until 81).count(g.state.stoneOwnerAt(_) == GoState.BlackPlayer)) ===
            Some(handicap))
      }
    }
    "round trip a mid game position with captures and passes" in {
      val fen =
        "9/9/9/9/9/9/9/9/1s7[SSSSSSSSSSssssssssss] w - 0 865 3 1 55 1 4"
      reparsed(fen) === fen
    }
    "read the legacy nine field form and emit the ten field form" in {
      reparsed("9/9/9/9/9/9/9/9/9[SSSSSSSSSSssssssssss] b - 0 55 0 0 55 1") === Go9x9.initialFen.value
    }
    "reject a fen with the wrong number of fields" in {
      GoFen.parse("badfen") must beLeft(GoFenError.UnexpectedFieldCount(1, "badfen"))
    }
    "reject an unsupported board size" in {
      val fen = "10/10/10/10/10/10/10/10/10/10[SSSSSSSSSSssssssssss] b - 0 55 0 0 55 0 1"
      GoFen.parse(fen) must beLeft(GoFenError.UnsupportedBoardSize(10, fen))
    }
    "reject a row that does not fill the board" in {
      GoFen.parse("9/9/9/9/9/9/9/9/8[SSSSSSSSSSssssssssss] b - 0 55 0 0 55 0 1") must beLeft(
        GoFenError.MalformedBoardRow("8", 9, 8)
      )
    }
    "reject a row that overflows the board" in {
      GoFen.parse("9/9/9/9/9/9/9/9/9S[SSSSSSSSSSssssssssss] b - 0 55 0 0 55 0 1") must beLeft(
        GoFenError.MalformedBoardRow("9S", 9, 10)
      )
    }
    "reject an unknown stone symbol" in {
      GoFen.parse("9/9/9/9/9/9/9/9/8X[SSSSSSSSSSssssssssss] b - 0 55 0 0 55 0 1") must beLeft(
        GoFenError.UnknownStoneSymbol('X', "8X")
      )
    }
    "reject an unknown turn symbol" in {
      GoFen.parse("9/9/9/9/9/9/9/9/9[SSSSSSSSSSssssssssss] x - 0 55 0 0 55 0 1") must beLeft(
        GoFenError.UnknownTurnSymbol("x")
      )
    }
    "reject a non numeric komi" in {
      GoFen.parse("9/9/9/9/9/9/9/9/9[SSSSSSSSSSssssssssss] b - 0 55 0 0 komi 0 1") must beLeft(
        GoFenError.NonNumericField("komi", "komi")
      )
    }
    "reject a ko point whose file is off the board" in {
      GoFen.parse("9/9/9/9/9/9/9/9/9[SSSSSSSSSSssssssssss] b z9 0 55 0 0 55 0 1") must beLeft(
        GoFenError.MalformedKoPoint("z9")
      )
    }
    "reject a ko point whose rank is off the board" in {
      GoFen.parse("9/9/9/9/9/9/9/9/9[SSSSSSSSSSssssssssss] b a10 0 55 0 0 55 0 1") must beLeft(
        GoFenError.MalformedKoPoint("a10")
      )
    }
    "accept a ko point inside the board and forbid the recapture it names" in {
      val state = GoFen
        .parse("9/9/9/9/9/9/9/9/9[SSSSSSSSSSssssssssss] b c3 0 55 0 0 55 0 1")
        .fold(error => sys.error(s"unparsable go fen: $error"), _.state)
      (state.simpleKoMove === Some(engineMove(9, "c3"))) and
        (state.isLegal(engineMove(9, "c3")) === false) and
        (state.legalMoves.toList must not(contain(engineMove(9, "c3")))) and
        (state.legalMoves.length === 81)
    }
  }

  "fen ply accounting" should {
    "recover the ply count from the full move number and turn" in {
      forall(List(("b", 1, 0), ("w", 1, 1), ("b", 2, 2), ("w", 2, 3), ("b", 30, 58))) {
        case (turn, fullMove, plyCount) =>
          GoFen
            .parse(s"9/9/9/9/9/9/9/9/9[SSSSSSSSSSssssssssss] $turn - 0 55 0 0 55 0 $fullMove")
            .toOption
            .map(_.plyCount) === Some(plyCount)
      }
    }
  }
}
