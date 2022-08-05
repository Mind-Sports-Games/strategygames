package strategygames.draughts
import strategygames.Player
import strategygames.draughts.Pos64

import cats.data.Validated._
import org.specs2.matcher.ValidatedMatchers
import org.specs2.mutable.Specification
import scala.concurrent.{ Await, Future }
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt

import format.Uci

class RussianDraughtsTest extends Specification with ValidatedMatchers {

  // https://playstrategy.dev/JqkbmbEX
  val moves = List(
    "2420",
    "1116",
    "2011",
    "0716",
    "2217",
    "0913",
    "2522",
    "0609",
    "2218",
    "1322",
    "2215",
    "3025",
    "0913",
    "2724",
    "0509",
    "2522",
    "0811",
    "2420",
    "1519",
    "2218",
    "1317",
    "2114",
    "1405",
    "1924",
    "2027",
    "0206",
    "2724",
    "1620",
    "1815",
    "1019", // capture choice single instead of double
    "2415",
    "1508",
    "0411",
    "3227",
    "2024",
    "2720",
    "1116",
    "2011",
    "0609",
    "0514",
    "0105",
    "3127",
    "0307",
    "1102",
    "1216",
    "0220",
    "0509",
    "1405"
  )

  val pdnMoves = List(
    "24-20",
    "11-16",
    "20x11",
    "7x16",
    "22-17",
    "9-13",
    "25-22",
    "6-9",
    "22-18",
    "13x22",
    "22x15",
    "30-25",
    "9-13",
    "27-24",
    "5-9",
    "25-22",
    "8-11",
    "24-20",
    "15-19",
    "22-18",
    "13-17",
    "21x14",
    "14x5",
    "19-24",
    "20x27",
    "2-6",
    "27-24",
    "16-20",
    "18-15",
    "10x19",
    "24x15",
    "15x8",
    "4x11",
    "32-27",
    "20-24",
    "27x20",
    "11-16",
    "20x11",
    "6-9",
    "5x14",
    "1-5",
    "31-27",
    "3-7",
    "11x2",
    "12-16",
    "2x20",
    "5-9",
    "14x5"
  )

  def toMove(uci: String): format.Uci.Move = {
    format.Uci.Move(uci) match {
      case Some(m) => m
      case None    => sys.error("Unable to parse move")

    }
  }

  def move(s: Situation, uci: String): Situation =
    s.move(toMove(uci)) match {
      case Valid(move) => move.situationAfter
      case Invalid(e)  => sys.error(s"Move is invalid for position: ${e}")
    }

  "Russian Draughts game " should {
    "end in win for P1" in {
      val s = moves
        .foldLeft(Situation(variant.Russian))((sit, uci) => move(sit, uci))

      s.winner must_== Some(Player.P1)
    }
  }

  "Russian Draughts game " should {
    "be valid in due to capture choice" in {
      val s = moves
        .take(29)
        .foldLeft(Situation(variant.Russian))((sit, uci) => move(sit, uci))

      s.validMoves.contains(Pos64.posAt(moves(29).take(2).toInt).getOrElse(Pos64(1, 1))) must_== true
    }
    "be invalid in brazillian due to capture choice" in {
      val s = moves
        .take(29)
        .foldLeft(Situation(variant.Brazilian))((sit, uci) => move(sit, uci))

      s.validMoves.contains(Pos64.posAt(moves(29).take(2).toInt).getOrElse(Pos64(1, 1))) must_== false
    }
    "be invalid when playing a non possible move " in {
      val future = Future {
        moves
          .take(29)
          .:+("2420")
          .foldLeft(Situation(variant.Russian))((sit, uci) => move(sit, uci))
      }

      Await.result(future, 1 seconds) must throwA[RuntimeException]
    }
    "be invalid when playing a non possible move for variant " in {
      val future = Future {
        moves
          .take(30)
          .foldLeft(Situation(variant.Brazilian))((sit, uci) => move(sit, uci))
      }

      Await.result(future, 1 seconds) must throwA[RuntimeException]
    }
  }

  "Russian Draughts game " should {
    "be replayable" in {
      val variantGame                                                          = variant.Russian
      val initialFen                                                           = variant.Russian.initialFen
      val x: (DraughtsGame, List[(DraughtsGame, Uci.WithSan)], Option[String]) =
        Replay.gameMoveWhileValid(pdnMoves, initialFen, variantGame)

      val gameAfterMoves = x._2.last._1

      gameAfterMoves.situation.winner must_== Some(Player.P1)

    }
    "but not replayable for brazilian variant" in {
      val variantGame = variant.Brazilian
      val initialFen  = variant.Brazilian.initialFen
      val future      = Future { Replay.gameMoveWhileValid(pdnMoves, initialFen, variantGame) }

      Await.result(future, 1 seconds) must throwA[RuntimeException]
    }
  }

}
