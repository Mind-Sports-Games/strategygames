package strategygames.go

import org.specs2.mutable.Specification

import strategygames.ActionStrs
import strategygames.go.format.{ FEN, Forsyth }
import strategygames.go.variant.{ Go13x13, Go9x9 }

class GoPlyAtFenTest extends Specification with GoRulesTestSupport {

  private val opening: List[String] = List("s@e5", "s@c3", "s@g7", "s@c7", "pass", "pass")

  private def actionStrs(actions: List[String]): ActionStrs = actions.map(Vector(_)).toVector

  // the rules support plays by point key, where `Replay` reads uci
  private def asKeys(ucis: List[String]): List[String] = ucis.map(_.stripPrefix("s@"))

  private def fenAfter(actions: List[String], variant: strategygames.go.variant.Variant = Go9x9): FEN =
    fenOf(playing(variant, asKeys(actions)))

  private def plyOf(atFen: FEN, actions: List[String] = opening): Option[Int] =
    Replay.plyAtFen(actionStrs(actions), None, Go9x9, atFen).toOption

  "the ply a fen is reached at" should {

    "be 0 for the position the game starts from" in
      plyOf(Go9x9.initialFen) === Some(0)

    "count one ply per action" in
      forall(opening.indices.toList)(played => plyOf(fenAfter(opening.take(played + 1))) === Some(played + 1))

    "be the last ply for the position the game ends at" in
      plyOf(fenAfter(opening)) === Some(opening.size)

    "be refused for a position the game never reaches" in
      plyOf(fenAfter(List("s@a1"))) === None

    "be refused for a fen that is not a go position at all" in
      plyOf(FEN("badfen")) === None

    "be refused for a fen of the wrong board size" in
      plyOf(Go13x13.initialFen) === None
  }

  "a nine field fen, which predates the pass count" should {

    val tenField  = Go9x9.initialFen
    val nineField = FEN(tenField.value.split(' ').patch(8, Nil, 1).mkString(" "))

    "still name a board this variant understands" in {
      (nineField.oldFenSytle === true) and
        (Forsyth.<<@(Go9x9, nineField).isDefined === true)
    }

    "be found at the same ply as the ten field form it means" in {
      (plyOf(nineField) === plyOf(tenField)) and (plyOf(nineField) === Some(0))
    }
  }

  "a game resumed from a handicap fen" should {

    val handicapFen = Go9x9.fenFromSetupConfig(4, 55)
    val played      = List("s@a1", "s@a3")

    def plyFromHandicap(atFen: FEN): Option[Int] =
      Replay.plyAtFen(actionStrs(played), Some(handicapFen), Go9x9, atFen).toOption

    "be 0 at the position it resumed from" in
      plyFromHandicap(handicapFen) === Some(0)

    "count from the resumed position rather than from an empty board" in
      plyFromHandicap(fenOf(playingFrom(handicapFen, asKeys(played)))) === Some(played.size)
  }

  "the wrapper" should {
    "reach the same ply for go as the go replay does" in
      strategygames.Replay
        .plyAtFen(
          strategygames.GameLogic.Go(),
          actionStrs(opening),
          None,
          strategygames.variant.Variant.Go(Go9x9),
          strategygames.format.FEN.Go(fenAfter(opening.take(3)))
        )
        .toOption === Some(3)
  }
}
