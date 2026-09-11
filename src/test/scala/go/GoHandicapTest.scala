package strategygames.go

import org.specs2.mutable.Specification

import strategygames.Player
import strategygames.go.format.FEN
import strategygames.go.variant.Go9x9

class GoHandicapTest extends Specification with GoRulesTestSupport {

  private def handicapFenOf(handicap: Int): FEN =
    FEN(s"${Go9x9.boardFenFromHandicap(handicap)}${pocket} w - 0 55 0 0 55 0 1")

  private val fourStoneSetup = Go9x9.fenFromSetupConfig(4, 55)

  "every 9x9 handicap board" should {
    "keep its stones through a fen round trip" in {
      forall(1 to 9) { handicap =>
        fenOf(situationFrom(handicapFenOf(handicap))).board === handicapFenOf(handicap).board
      }
    }
    "settle on a fen that renders back to itself" in {
      forall(1 to 9) { handicap =>
        val rendered = fenOf(situationFrom(handicapFenOf(handicap)))
        fenOf(situationFrom(rendered)) === rendered
      }
    }
  }

  "a handicap setup config" should {
    "lay out four stones and hand the first move to white" in {
      fourStoneSetup.value === s"9/9/2S3S2/9/9/9/2S3S2/9/9${pocket} w - 40 55 0 0 55 0 1"
    }
    "report the handicap it was built with" in {
      fourStoneSetup.handicap === Some(4)
    }
    "start the game with white to move" in {
      Game(Some(Go9x9), Some(fourStoneSetup)).situation.player === Player.P2
    }
  }

  "the cosmetic score fields of a setup fen" should {
    "be replaced by the real area score on the first render" in {
      val started = Game(Some(Go9x9), Some(fourStoneSetup))
      (fourStoneSetup.player1Score === 40) and (fenOf(started).player1Score === 810)
    }
  }

  "handicap stones" should {
    "count towards area scoring like any other stone" in {
      val started = Game(Some(Go9x9), Some(fourStoneSetup))
      (fenOf(started).player1Score === 810) and (fenOf(started).player2Score === 55)
    }
    "keep the board they set up once the opponent has answered" in {
      fenOf(playingFrom(fourStoneSetup, List("a1"))).value ===
        s"9/9/2S3S2/9/9/9/2S3S2/9/s8${pocket} b - 40 65 0 0 55 0 2"
    }
  }
}
