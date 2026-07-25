package strategygames.go

import org.specs2.mutable.Specification

import strategygames.go.format.FEN
import strategygames.go.variant.Go9x9

class GoBatchEntryTest extends Specification {

  private val variant = Go9x9
  private val start   = variant.initialFen
  private val opening = List("S@a1", "s@b2")

  private def fromFen(fen: FEN): Api.Position =
    Api.positionFromVariantNameAndFEN(variant.key, fen.value)

  private def perPly(fen: FEN, ucis: List[String]): Api.Position = fromFen(fen).makeMoves(ucis)

  private def batch(fen: FEN, ucis: List[String]): Api.Position =
    Api.positionFromVariantStartingFenAndMoves(variant, fen, ucis)

  private def positions(fen: FEN, ucis: List[String]): Vector[Api.Position] =
    Api.positionsFromVariantStartingFenAndMoves(variant, fen, ucis)

  private def resumedAfter(ucis: List[String]): FEN = perPly(start, ucis).fen

  private def messageOf(building: => Any): String =
    try { building; "nothing was thrown" }
    catch { case error: Throwable => error.getMessage }

  "a game resumed from a fen counting one to three passes" should {

    val resumptions = List(
      (List("pass"), "pass"),
      (List("pass"), "ss:"),
      (List("pass", "pass"), "pass"),
      (List("pass", "pass"), "ss:")
    )

    "reach the fen the same game played straight through reaches" in {
      resumptions
        .map { case (passes, action) =>
          batch(resumedAfter(opening ++ passes), List(action)).fen must
            beEqualTo(perPly(start, opening ++ passes ++ List(action)).fen)
        }
        .reduce(_ and _)
    }

    "reach the fen the per ply path reaches from the same resume point" in {
      resumptions
        .map { case (passes, action) =>
          val resumed = resumedAfter(opening ++ passes)
          batch(resumed, List(action)).fen must beEqualTo(perPly(resumed, List(action)).fen)
        }
        .reduce(_ and _)
    }

    "advance the pass count by the action given and nothing else" in {
      val resumedAtOnePass = resumedAfter(opening ++ List("pass"))
      (resumedAtOnePass.fenPassCount must beEqualTo(1)) and
        (batch(resumedAtOnePass, List("pass")).fen.fenPassCount must beEqualTo(2)) and
        (batch(resumedAtOnePass, List("ss:")).fen.fenPassCount must beEqualTo(3))
    }
  }

  "a batch entry given no actions at all" should {
    "return the parsed starting position unchanged" in {
      val resumed = resumedAfter(opening ++ List("pass"))
      (batch(resumed, Nil).fen must beEqualTo(fromFen(resumed).fen)) and
        (batch(resumed, Nil).legalActions.toList must beEqualTo(fromFen(resumed).legalActions.toList)) and
        (batch(resumed, Nil).pieceMap must beEqualTo(fromFen(resumed).pieceMap))
    }
  }

  "a game resumed from a fen whose dead stones are already settled" should {

    val settled = resumedAfter(opening ++ List("pass", "pass", "ss:"))

    "refuse a further action on the batch path, naming the ply" in {
      messageOf(batch(settled, List("pass"))) must
        beEqualTo(s"Action pass at ply 0 offered to a finished ${variant.key} game")
    }

    "refuse a further action on the per ply path too" in {
      perPly(settled, List("pass")) must throwAn[Exception]
    }
  }

  "a fold that has already settled its dead stones" should {
    "refuse the action offered after the settlement, naming its ply" in {
      messageOf(batch(start, opening ++ List("pass", "pass", "ss:", "pass"))) must
        beEqualTo(s"Action pass at ply 5 offered to a finished ${variant.key} game")
    }
  }

  "a score read after a pass terminated fold" should {
    "equal the score before the passes, and the per ply path's" in {
      val passes = opening ++ List("pass", "pass")
      (batch(start, passes).fenScore must beEqualTo(batch(start, opening).fenScore)) and
        (batch(start, passes).fenScore must beEqualTo(perPly(start, passes).fenScore))
    }
  }

  "an action the batch entry cannot accept" should {

    "name the ply of an illegal move and the action itself" in {
      messageOf(batch(start, List("S@a1", "s@a1"))) must
        startWith(s"Illegal action s@a1 at ply 1 for ${variant.key}: legal actions ")
    }

    "name the ply of an unreadable action" in {
      messageOf(batch(start, List("S@a1", "garbage"))) must
        beEqualTo(s"Unreadable action garbage at ply 1 for ${variant.key}")
    }

    "name the ply of a drop the board has no square for" in {
      messageOf(batch(start, List("s@n4"))) must
        beEqualTo(s"Drop s@n4 at ply 0 names no square of ${variant.key}")
    }

    "refuse a dead stone key that is not a coordinate" in {
      batch(start, opening ++ List("pass", "pass", "ss:zz")) must throwAn[Exception]
    }
  }

  "the per ply batch entry" should {

    val script = opening ++ List("s@c3", "pass", "S@d4", "pass", "pass", "ss:c3")

    val built    = positions(start, script)
    val expected = (0 to script.size).map(played => perPly(start, script.take(played))).toList

    "give one position per ply, the starting position first" in {
      (built.size must beEqualTo(script.size + 1)) and
        (built.head.fen must beEqualTo(start))
    }

    "agree with the per ply path on every fen" in {
      built.map(_.fen.value).toList must beEqualTo(expected.map(_.fen.value))
    }

    "agree with the per ply path on every stone map" in {
      built.map(_.pieceMap).toList must beEqualTo(expected.map(_.pieceMap))
    }
  }
}
