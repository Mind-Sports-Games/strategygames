package strategygames.togyzkumalak.format

import strategygames.Player
import strategygames.togyzkumalak.{ Piece, PieceMap, Pos, Stone, Tuzdik }

final case class FEN(value: String) extends AnyVal {

  override def toString = value

  def player: Option[Player] =
    value.split(' ').lift(3) flatMap (_.headOption) flatMap Player.apply

  def player1Score: Int = intFromFen(1).getOrElse(0)

  def player2Score: Int = intFromFen(2).getOrElse(0)

  def fullMove: Option[Int] = intFromFen(4)

  def ply: Option[Int] =
    fullMove map { fm =>
      fm * 2 - (if (player.exists(_.p1)) 2 else 1)
    }

  private def intFromFen(index: Int): Option[Int] =
    value.split(' ').lift(index).flatMap(_.toIntOption)

  private def width: Int = 9

  def mancalaStoneArray: Array[Int] =
    (
      value.split(' ')(0).split('/')(1).split(',')
        ++
          value.split(' ')(0).split('/')(0).split(',').reverse
    )
      .map(c =>
        c.toString() match {
          case x if 1 to width map (_.toString) contains x => Array.fill(x.toInt)(0)
          case x if x.length > 1                           => Array(c.dropRight(1).toInt)
          case _                                           => Array(-1)
        }
      )
      .flatten
      .toArray

  def pieces: PieceMap =
    mancalaStoneArray.zipWithIndex
      .filterNot { case (s, _) => s == 0 }
      .map { case (stones, index) =>
        Pos(index) -> (if (stones == -1) (Tuzdik, 1)
                       else (Stone, stones))
      }
      .flatMap {
        case (Some(pos), (r, c)) if r == Tuzdik => Some((pos -> Tuple2(Piece(!pos.player, r), c)))
        case (Some(pos), (r, c))                => Some((pos -> Tuple2(Piece(pos.player, r), c)))
        case _                                  => None
      }
      .toMap

  private def tuzdikPit(playerFen: Array[String]): Option[Int] = {
    val pit = playerFen
      .map(c =>
        c.toString() match {
          case x if 1 to width map (_.toString) contains x => Array.fill(x.toInt)(0)
          case x if x.length > 1                           => Array(0)
          case _                                           => Array(1)
        }
      )
      .flatten
      .toArray
      .indexOf(1)
    if (pit < 0) None else Some(pit)
  }

  // this isn't used anywhere (yet?)
  def tuzdikPits: Map[Player, Option[Int]] =
    Map(
      Player.P1 -> tuzdikPit(value.split(' ')(0).split('/')(1).split(',')),
      Player.P2 -> tuzdikPit(value.split(' ')(0).split('/')(0).split(',').reverse)
    )

  def initial = value == Forsyth.initial.value
}

object FEN {

  def clean(source: String): FEN = FEN(source.replace("_", " ").trim)
}
