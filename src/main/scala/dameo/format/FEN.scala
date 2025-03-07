package strategygames.dameo.format

import strategygames.Player
import strategygames.dameo.PieceMap

//TODO Dameo, work out the FEN structure for a Dameo game.
//What do other platforms use?
//Is there a standard for Dameo?
//Are we able to use something similar to Draughts FENs (look in there)
//Or do we want to invent our own, and have it more similar to chess?
//Either way any FEN parsing wants to be done here. Look at other game logic FEN files
final case class FEN(value: String) extends AnyVal {

  override def toString = value

  def player: Option[Player] =
    value.split(':').lift(0) flatMap (_.headOption) flatMap Player.apply

  def initial = value == Forsyth.initial.value

  // TODO Dameo set
  def pieces: PieceMap = Map.empty
}

object FEN {

  def clean(source: String): FEN = FEN(source.replace("_", " ").trim)

  def fullMoveIndex: Int = 4

}
