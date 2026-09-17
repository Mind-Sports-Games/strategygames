package strategygames.go

import org.specs2.mutable.Specification

import strategygames.go.format.Uci
import strategygames.go.opening.{ Ecopening, EcopeningDB }
import strategygames.go.variant.{ Go13x13, Go19x19, Go9x9, Variant }

class GoEcopeningTest extends Specification with GoRulesTestSupport {

  private val variantsByGrouping: Map[String, Variant] =
    Map("go9x9" -> Go9x9, "go13x13" -> Go13x13, "go19x19" -> Go19x19)

  private val ecosByVariant: List[(Variant, String)] =
    List(Go9x9 -> "A00", Go13x13 -> "B00", Go19x19 -> "C00")

  private def variantOf(opening: Ecopening): Variant =
    variantsByGrouping.getOrElse(
      opening.variantGrouping,
      sys.error(s"unknown go variant grouping ${opening.variantGrouping}")
    )

  private def boardsPlayed(variant: Variant, keys: List[String]): List[Board] =
    Replay
      .boardsFromUci(keys.flatMap(key => Uci(s"s@${key}")), Some(variant.initialFen), variant)
      .getOrElse(sys.error(s"unreplayable go opening ${keys.mkString(" ")}"))

  private def openingOf(boards: List[Board]): Option[Ecopening] =
    boards.reverseIterator
      .flatMap(board => EcopeningDB.allByFen.get(format.Forsyth.exportBoard(board)))
      .nextOption()

  "every opening the database holds" should {
    "be keyed by the board state its variant's starting board exports" in
      forall(EcopeningDB.all) { opening =>
        opening.fen === format.Forsyth.exportBoard(Board.init(variantOf(opening)))
      }
    "be found again by that key" in
      forall(EcopeningDB.all)(opening => EcopeningDB.allByFen.get(opening.fen) === Some(opening))
  }

  "the boards a played go game passes through" should {
    "name the starting position of the size they were played on" in
      forall(ecosByVariant) { case (variant, eco) =>
        openingOf(boardsPlayed(variant, List("d4", "f4", "e6"))).map(_.eco) === Some(eco)
      }
    "name it from the starting board alone, before a stone is placed" in
      forall(ecosByVariant) { case (variant, eco) =>
        openingOf(boardsPlayed(variant, Nil)).map(_.eco) === Some(eco)
      }
    "name nothing once the starting board is no longer among them" in
      openingOf(boardsPlayed(Go9x9, List("d4", "f4")).drop(1)) === None
  }

  "Ecopening.fromGame" should {
    "name the starting position of the default variant" in
      Ecopening.fromGame(Vector(Vector("s@d4"), Vector("s@f4"))).map(_.eco) === Some("C00")
    "name it from a game that has not started" in
      Ecopening.fromGame(Vector.empty).map(_.eco) === Some("C00")
  }
}
