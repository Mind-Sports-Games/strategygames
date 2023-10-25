package strategygames.fairysf.format

import cats.data.Validated

import strategygames.fairysf.variant.Variant
import strategygames.fairysf.{ Action, Drop, Move, Replay }
import strategygames.format.{ Uci => StratGamesUci }
import strategygames.format.LexicalUci
import strategygames.{ ActionStrs, GameFamily, GameLogic }
import strategygames.fairysf.Api

object UciDump {

  // a2a4, b8c6
  def apply(replay: Replay): ActionStrs =
    replay.chronoActions.map(_.map(action(replay.setup.board.variant)))

  def apply(
      actionStrs: ActionStrs,
      initialFen: Option[FEN],
      variant: Variant
  ): Validated[String, ActionStrs] =
    if (actionStrs.isEmpty) Validated.valid(Nil)
    else Replay(actionStrs, initialFen, variant) andThen (_.valid) map apply

  def action(variant: Variant)(action: Action): String =
    action match {
      case m: Move =>
        m.castle.fold(m.toUci.lilaUci) {
          case ((kf, kt), (rf, _)) if kf == kt => kf.key + rf.key
          case ((kf, kt), _)                   => kf.key + kt.key
        }
      case d: Drop => d.toUci.lilaUci
    }

  // TODO: I'm not a big fan of the API that's resulted in the toFishnetUci and fromFishnetUci.
  //       It feels overly specific but I'm also not sure if it warrants the time to spend on
  //       cleaning it up right now. So I'll leave it for the review and see if we want to do it
  //       then.

  def toFishnetUci(gameFamily: GameFamily, moves: List[Uci]): String = gameFamily match {
    case GameFamily.Amazons() =>
      moves.toList
        .sliding(2, 2)
        .toList
        .flatMap(
          _ match {
            case List(Uci.Move(orig, dest, _), Uci.Drop(_, dest2)) =>
              Some(s"${orig}${dest},${dest}${dest2}")
            case _                                                 => None
          }
        )
        .mkString(" ")
    case _                    =>
      moves.map(_.fishnetUci).mkString(" ")
  }

  def fromFishnetUci(variant: Variant, moves: List[LexicalUci]): List[LexicalUci] =
    variant.gameFamily match {
      case GameFamily.Amazons() =>
        moves
          .flatMap(
            _.uci
              .split(",")
              .sliding(2, 2)
              .flatMap(both => {
                Uci(variant.gameFamily, both(1)) match {
                  case Some(uci) =>
                    List(both(0), s"P@${uci.origDest._2}")
                  case None      => sys.error(s"Unable to parse uci: ${both(1)}")
                }
              })
          )
          .flatMap(LexicalUci.apply)
      case _                    =>
        moves
    }
}
