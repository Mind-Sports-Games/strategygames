package strategygames.abalone

import cats.data.Validated
import org.specs2.matcher.MatchResult
import org.specs2.mutable.Specification
import strategygames.abalone.format.{FEN, Forsyth, Uci}
import strategygames.abalone.variant.Variant
import strategygames.{Player, Score}

class AbaloneTest extends Specification with IAbaloneTest {
  val debug: Boolean = false

  def playUciList(game: Game, ucis: List[Uci]): Validated[String, Game] =
    ucis.foldLeft(Validated.valid(game): Validated[String, Game]) { (vg, action: Uci) =>
      vg.flatMap { g => g.apply(action).map(_._1) }
    }

  def playActionStrs(
      actionStrs: List[String],
      game: Option[Game] = None,
      variant: Option[Variant] = None
  ): Validated[String, Game] =
    playUciList(
      game.getOrElse(Game.apply(variant.getOrElse(Variant.default))),
      Uci.readList(actionStrs.mkString(" ")).getOrElse(List())
    )

  def checkFinalFen(
      variant: Variant,
      ucis: List[String],
      finalFen: String
  ): MatchResult[Any] = {
    checkFinalFenCore(variant, ucis, variant.initialFen, finalFen)
  }

  def checkFinalFen(
      variant: Variant,
      ucis: List[String],
      initialFen: String,
      finalFen: String
  ): MatchResult[Any] =
    checkFinalFenCore(variant, ucis, FEN(initialFen), finalFen)

  def checkFinalFenCore(
      variant: Variant,
      ucis: List[String],
      initialFen: FEN,
      finalFen: String
  ): MatchResult[Any] = {
    var game = new Game(
      Situation(
        board = Board(
          initialFen.pieces(variant),
          History(
            prevPlayer = initialFen.prevPlayer(variant),
            score = Score(initialFen.player1Score, initialFen.player2Score),
            halfMoveClock = initialFen.halfMovesSinceLastCapture(variant).getOrElse(0)
          ),
          variant
        ),
        player = initialFen.player.getOrElse(Player.P1)
      )
    )

    var i = game.plies
    ucis
      .map(uci =>
        uci match {
          case Uci.Move.moveR(orig0, orig1, dest0, dest1) =>
            Uci.Move.fromStrings(orig0 + orig1, dest0 + dest1).get
          case _                                          => sys.error(s"Unrecognized move: $uci.")
        }
      )
      //      .map(uci => Uci.Move.fromStrings(uci.substring(0, 2), uci.substring(2)).get)
      .foreach(m => {
        game = next(game, m.orig, m.dest)
        if (debug) {
          i += 1
          println(i.toString + " -> " + Forsyth.>>(game).toString)
        }
      })

    Forsyth.>>(game).value must_== finalFen
  }
}
