package strategygames.fairysf.format

import cats.data.Validated

import strategygames.fairysf.variant.Variant
import strategygames.fairysf.{ MoveOrDrop, Replay }
import strategygames.format.{ Uci => StratGamesUci }
import strategygames.format.LexicalUci
import strategygames.{ GameFamily, GameLogic }

object UciDump {

  // a2a4, b8c6
  def apply(replay: Replay): List[String] =
    replay.chronoMoves map move(replay.setup.board.variant)

  def apply(
      moves: Seq[String],
      initialFen: Option[FEN],
      variant: Variant
  ): Validated[String, List[String]] =
    if (moves.isEmpty) Validated.valid(Nil)
    else Replay(moves, initialFen, variant) andThen (_.valid) map apply

  def move(variant: Variant)(mod: MoveOrDrop): String =
    mod match {
      case Left(m)  =>
        m.castle.fold(m.toUci.lilaUci) {
          case ((kf, kt), (rf, _)) if kf == kt => kf.key + rf.key
          case ((kf, kt), _)                   => kf.key + kt.key
        }
      case Right(d) => d.toUci.lilaUci
    }

  // TODO: It should probably be the opposite of the one in the main UciDump
  def toFishnetUci(variant: Variant)(moves: List[Uci]): String = variant.gameFamily match {
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
