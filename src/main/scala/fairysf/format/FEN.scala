package strategygames.fairysf.format

import strategygames.{ GameFamily, Player }
import strategygames.fairysf.variant.Variant
import strategygames.fairysf.Api

final case class FEN(value: String) extends AnyVal {

  override def toString = value

  def fullMove: Option[Int] = value.split(' ').lift(FEN.fullMoveIndex).flatMap(_.toIntOption)

  // this only works for Othello because the fen uses w to mean p1 and b to mean p2
  def player: Option[Player] =
    value.split(' ').lift(FEN.playerIndex) flatMap (_.headOption) flatMap Player.apply

  def invertPlayer: Option[FEN] =
    // This is safe because player function ensures there is an element at playerIndex when doing split(' ')
    player.map { p => FEN(value.split(' ').updated(FEN.playerIndex, (!p).letter.toString).mkString(" ")) }

  def ply: Option[Int] =
    fullMove map { fm =>
      fm * 2 - (if (player.exists(_.p1)) 2 else 1)
    }

  def boardPart(height: Option[Int] = None): String = {
    val noSGPockets = value.takeWhile(' ' !=).takeWhile('[' !=)
    height match {
      // handle fairySFFen where pocket pieces are after a final '/'
      case Some(height) if !value.contains('[') => noSGPockets.split("/", height + 1).init.mkString("/")
      case _                                    => noSGPockets
    }
  }

  def initial = value == Forsyth.initial.value
}

object FEN {

  def clean(source: String): FEN = FEN(source.replace("_", " ").trim)

  def fishnetFen(variant: Variant)(fen: FEN) = variant.gameFamily match {
    case GameFamily.Amazons() =>
      FEN(Api.toFairySFFen("amazons", fen.value))
    case _                    =>
      fen
  }

  def playerIndex: Int = 1

  def fullMoveIndex: Int = 5

}
