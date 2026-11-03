package strategygames

import org.specs2.mutable.Specification

import strategygames.{ Game, GameLogic }
import strategygames.format.Forsyth
import strategygames.opening.{ Ecopening, EcopeningDB }
import strategygames.variant.Variant

class FairySFEcopeningTest extends Specification {

  private val gameLogic = GameLogic.FairySF()
  private val amazons   = Variant.wrap(strategygames.fairysf.variant.Amazons)

  private val sensibleVariants: List[Variant] =
    Variant.openingSensibleVariants(gameLogic).toList.sortBy(_.key)

  private def openingsOf(variant: Variant): List[Ecopening] =
    EcopeningDB.all(gameLogic).filter(_.variantGrouping == variant.key)

  // the key a game is looked up by: the first field of its fen
  private def boardKey(game: Game): String =
    Forsyth.>>(gameLogic, game).value.split(' ').head

  private def continuations(opening: Ecopening): List[Ecopening] =
    EcopeningDB.all(gameLogic).filter(opening.possibleContinuation)

  private def play(game: Game, uci: String): Game =
    game.situation.actions
      .find(_.toUci.uci == uci)
      .map(game(_))
      .getOrElse(sys.error(s"${uci} is not legal"))

  // the starting position's moves are "", which splits to one empty move
  private def played(opening: Ecopening): List[String] = opening.moveList.filter(_.nonEmpty)

  private def start(variant: Variant): Game = Game(gameLogic, Some(variant), None)

  private def gameAfter(variant: Variant, opening: Ecopening): Game =
    played(opening).foldLeft(start(variant))(play)

  sensibleVariants.foreach { variant =>
    s"the ${variant.key} openings" should {
      "be keyed by the position their moves reach" in
        forall(openingsOf(variant)) { opening =>
          opening.fen === boardKey(gameAfter(variant, opening))
        }
      "include the starting position" in {
        EcopeningDB.allByFen(gameLogic).get(boardKey(start(variant))).map(played) === Some(Nil)
      }
      "be reached one action at a time from an opening one action shorter" in
        forall(openingsOf(variant).filter(_.lastMoveUci.nonEmpty)) { opening =>
          val parent = openingsOf(variant).find(o => played(o) == played(opening).init)
          parent.map(continuations(_).contains(opening)) === Some(true)
        }
    }
  }

  private val amazonsReplies: List[Ecopening] =
    EcopeningDB.all(gameLogic).filter(_.variantGrouping == "amazons-replies")

  private def continuationsAt(game: Game): Option[List[String]] =
    EcopeningDB.allByFen(gameLogic).get(boardKey(game)).map(continuations(_).map(_.lastMoveUci))

  // the common white first turns, each listed with its left-right mirror
  private val commonWhiteTurns: List[List[String]] = List(
    "g1g7 P@d7", "d1d7 P@g7", "g1g8 P@i6", "d1d8 P@b6", "g1g9 P@f9", "d1d9 P@e9",
    "g1g7 P@b7", "d1d7 P@i7", "j4e9 P@d9", "a4f9 P@g9", "d1i6 P@i7", "g1b6 P@b7",
    "g1g7 P@g9", "d1d7 P@d9", "g1g9 P@d9", "d1d9 P@g9", "g1g6 P@i6", "d1d6 P@b6"
  ).map(_.split(' ').toList)

  "the amazons openings" should {
    "offer both of white's opening moves from the start" in {
      continuationsAt(start(amazons)).map(_.toSet) === Some(Set("g1g7", "d1d7"))
    }
    "only ever offer white the arrow of its own opening" in {
      (continuationsAt(play(start(amazons), "g1g7")) === Some(List("P@d7"))) and
        (continuationsAt(play(start(amazons), "d1d7")) === Some(List("P@g7")))
    }
    "reply by mirroring white's turn through the centre of the board when that is legal" in {
      val afterWhite = List("g1g7", "P@b7").foldLeft(start(amazons))(play)
      (continuationsAt(afterWhite) === Some(List("d10d4"))) and
        (continuationsAt(play(afterWhite, "d10d4")) === Some(List("P@i4")))
    }
    "key black's replies by the position their moves reach" in
      forall(amazonsReplies)(opening => opening.fen === boardKey(gameAfter(amazons, opening)))
    "have one black reply, a move then an arrow, to each common white first turn" in
      forall(commonWhiteTurns) { turn =>
        val afterWhite = turn.foldLeft(start(amazons))(play)
        val replies    = continuationsAt(afterWhite).getOrElse(Nil)
        (replies.size === 1) and
          (continuationsAt(play(afterWhite, replies.head)).map(_.size) === Some(1))
      }
  }
}
