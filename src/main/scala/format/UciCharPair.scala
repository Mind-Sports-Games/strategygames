case class UciCharPair(a: Char, b: Char) {

  override def toString = s"$a$b"
}

object UciCharPair {

  def apply(lib: GameLib, uci: Uci): UciCharPair = (lib, uci) match {
    case (GameLib.Draughts(), Uci.Draughts(uci)) => draughts.UciCharPair(uci)
    case (GameLib.Chess(), Uci.Chess(uci))       => chess.UciCharPair(uci)
    case _                                       => sys.error("Invalid match between game lib and underlying types")
  }

  // Unsure about these, probably will need them, but it's annoying to have such
  // specific methods for draughts. :(
  def apply(lib: GameLib, uci: Uci, ambiguity: Int): UciCharPair = (lib, uci) match {
    case (GameLib.Draughts(), Uci.Draughts(uci)) => draughts.UciCharPair(uci, ambiguity)
    case _                                       => sys.error("This method is only implemented for draughts")
  }
  def apply(lib: GameLib, orig: Char, ambiguity: Int): UciCharPair = lib match {
    case GameLib.Draughts() => draughts.UciCharPair(orig, ambiguity)
    case _                  => sys.error("This method is only implemented for draughts")
  }

  def combine(lib: GameLib, uci1: Uci, uci2: Uci): UciCharPair = lib match {
    case GameLib.Draughts() => draughts.UciCharPair(orig, uci1, uci2)
    case _                  => sys.error("This method is only implemented for draughts")
  }

}
