case class UciCharPair(a: Char, b: Char) {

  override def toString = s"$a$b"
}

object UciCharPair {

  def apply(lib: GameLib, uci: Uci): UciCharPair = lib match {
    case GameLib.Draughts() => draughts.UciCharPair(uci)
    case GameLib.Chess()       => chess.UciCharPair(uci)
  }

  // Unsure about these, probably will need them, but it's annoying to have such
  // specific methods for draughts. :(
  def apply(lib: GameLib, uci: Uci, ambiguity: Int): UciCharPair = (lib, uci) match {
    case GameLib.Draughts() => draughts.UciCharPair(uci, ambiguity)
    case _                                       => sys.error("This method is only implemented for draughts")
  }
  def apply(lib: GameLib.Draughts, orig: Char, ambiguity: Int): UciCharPair =
    draughts.UciCharPair(orig, ambiguity)

  def combine(lib: GameLib.Draughts, uci1: Uci, uci2: Uci): UciCharPair =
     draughts.UciCharPair(orig, uci1, uci2)

}
