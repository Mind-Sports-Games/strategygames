package strategygames.fairysf.format

import strategygames.{ GameFamily, Player }
import strategygames.fairysf.variant.Variant
import strategygames.fairysf.Api

final case class FEN(value: String) extends AnyVal {

  override def toString = value

  def fullMove: Option[Int] = value.split(' ').lift(FEN.fullMoveIndex).flatMap(_.toIntOption)

  // this only works for Othello because the fen uses w to mean p1 and b to mean p2
  def player: Option[Player] =
    value.split(' ').lift(1) flatMap (_.headOption) flatMap Player.apply

  def ply: Option[Int] =
    fullMove map { fm =>
      fm * 2 - (if (player.exists(_.p1)) 2 else 1)
    }

  def initial = value == Forsyth.initial.value

  def flipPlayer: FEN = copy(
    value = value
      .replace(" w ", " z ")
      .replace(" b ", " w ")
      .replace(" z ", " b ")
  )
}

object FEN {

  def clean(source: String): FEN = FEN(source.replace("_", " ").trim)

  def fishnetFen(variant: Variant)(fen: FEN) = variant.gameFamily match {
    case GameFamily.Amazons() =>
      FEN(Api.toFairySFFen("amazons", fen.value))
    case _                    =>
      fen
  }

  def fullMoveIndex: Int = 5

}
