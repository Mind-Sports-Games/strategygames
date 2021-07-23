package strategygames.format

import strategygames.GameLib

case class UciCharPair(a: Char, b: Char) {

  override def toString = s"$a$b"
}

object UciCharPair {

  def apply(lib: GameLib, uci: Uci): UciCharPair = (lib, uci) match {
    case (GameLib.Draughts(), uci: Uci.Draughts)
      => strategygames.draughts.format.UciCharPair(uci.unwrap)
    case (GameLib.Chess(), uci: Uci.Chess)
      => strategygames.chess.format.UciCharPair(uci.unwrap)
    case _ => sys.error("Mismatched gamelib and UciCharPair")
  }

  // Unsure about these, probably will need them, but it's annoying to have such
  // specific methods for draughts. :(
  def apply(lib: GameLib, uci: Uci, ambiguity: Int): UciCharPair = (lib, uci) match {
    case (GameLib.Draughts(), uci: Uci.Draughts)
      => strategygames.draughts.format.UciCharPair(uci.unwrap, ambiguity)
    case _ => sys.error("This method is only implemented for draughts")
  }
  def apply(lib: GameLib.Draughts, orig: Char, ambiguity: Int): UciCharPair =
    strategygames.draughts.format.UciCharPair(orig, ambiguity)

  def combine(lib: GameLib.Draughts, uci1: Uci, uci2: Uci): UciCharPair = (uci1, uci2) match {
    case (uci1: Uci.Draughts, uci2: Uci.Draughts)
      => strategygames.draughts.format.UciCharPair.combine(uci1.unwrap, uci2.unwrap)
    case _ => sys.error("This is not implemented for anything but draughts")
  }

}
