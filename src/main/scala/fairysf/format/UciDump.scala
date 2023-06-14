package strategygames.fairysf.format

import cats.data.Validated

import strategygames.fairysf.variant.Variant
import strategygames.fairysf.{ MoveOrDrop, Replay }
import strategygames.{ Actions, GameFamily }

object UciDump {

  // a2a4, b8c6
  def apply(replay: Replay): Actions =
    replay.chronoActions.map(_.map(action(replay.setup.board.variant)))

  def apply(
      actions: Actions,
      initialFen: Option[FEN],
      variant: Variant
  ): Validated[String, Actions] =
    if (actions.isEmpty) Validated.valid(Nil)
    else Replay(actions, initialFen, variant) andThen (_.valid) map apply

  def action(variant: Variant)(mod: MoveOrDrop): String =
    mod match {
      case Left(m)  =>
        m.castle.fold(m.toUci.lilaUci) {
          case ((kf, kt), (rf, _)) if kf == kt => kf.key + rf.key
          case ((kf, kt), _)                   => kf.key + kt.key
        }
      case Right(d) => d.toUci.lilaUci
    }

  def fishnetUci(variant: Variant)(moves: List[Uci]): String = variant.gameFamily match {
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
}
