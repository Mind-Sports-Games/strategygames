package strategygames.format

import strategygames.GameLib

case class UciCharPair(a: Char, b: Char) {

  override def toString = s"$a$b"
}

object UciCharPair {

  def apply(lib: GameLib, uci: Uci): UciCharPair = (lib, uci) match {
    case (GameLib.Draughts(), Uci.Draughts(uci))
      => strategygames.draughts.format.UciCharPair(uci)
    case (GameLib.Chess(), Uci.Chess(uci))
      => strategygames.chess.format.UciCharPair(uci)
  }

  // Unsure about these, probably will need them, but it's annoying to have such
  // specific methods for draughts. :(
  def apply(lib: GameLib, uci: Uci, ambiguity: Int): UciCharPair = (lib, uci) match {
    case (GameLib.Draughts(), Uci.Draughts(uci))
      => strategygames.draughts.format.UciCharPair(uci, ambiguity)
    case _ => sys.error("This method is only implemented for draughts")
  }
  def apply(lib: GameLib.Draughts, orig: Char, ambiguity: Int): UciCharPair =
    strategygames.draughts.format.UciCharPair(orig, ambiguity)

  def combine(lib: GameLib.Draughts, uci1: Uci, uci2: Uci): UciCharPair = (uci1, uci2) match {
    case (Uci.Draughts(uci1), Uci.Draughts(uci2))
      => strategygames.draughts.format.UciCharPair.combine(uci1, uci2)
  }

}
