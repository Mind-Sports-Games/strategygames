package strategygames.format

import org.playstrategy.FairyStockfish.{ availablePieceChars, availablePromotablePieceChars, init };

// The name comes from here

// We need a more generalized, simple notion of UCI here.
// Without a variant we can't fully parse UCI and ensure it's validate it
// So instead we do a simpler validation here, which is based on simple
// structure of uci.
//
trait LexicalUci {
  val uci: String
}

object LexicalUci {

  init()

  def apply(s: String): Option[LexicalUci] = {
    if (validUci(s)) Some(LexicalUciImpl(s))
    else None
  }

  val availablePieces           = availablePieceChars().getString()
  val availablePromotablePieces = availablePromotablePieceChars().getString()

  def validRole(c: Char): Boolean = availablePieces.exists(c.==)
  def validPromotableRole(c: Char): Boolean =
    c == '+' || availablePromotablePieces.exists(c.==)
  def validFile(c: Char): Boolean = ('a' to 'i').exists(c.==)
  def validRank(s: String): Boolean =
    (s.length() == 1 && ('0' to '9').exists(s(0).==)) || (s == "10")
  def validSquare(s: String): Boolean =
    (s.nonEmpty && validFile(s(0))) &&
      ((s.length() == 2 && validRank(s.slice(1, 2))) ||
        (s.length() == 3 && validRank(s.slice(1, 3))))

  def validSquarePair(s: String): Boolean =
    s.length() match {
      case 4 => validSquare(s.slice(0, 2)) && validSquare(s.slice(2, 4))
      case 5 =>
        (validSquare(s.slice(0, 2)) && validSquare(s.slice(2, 5))) || (validSquare(
          s.slice(0, 3)
        ) && validSquare(s.slice(3, 5)))
      case 6 => validSquare(s.slice(0, 3)) && validSquare(s.slice(3, 6))
      case _ => false
    }

  def validUci(uci: String): Boolean =
    if (uci.length() < 4 || uci.length() > 7) false
    else if (uci == "0000") true
    else {
      val uciLower    = uci.toLowerCase()
      val isDrop      = validRole(uciLower(0)) && uciLower(1) == '@'
      val isPromotion = validPromotableRole(uciLower.last)
      (isDrop, isPromotion, uciLower.length()) match {
        // Drops
        case (true, false, 4) =>
          validRole(uciLower(0)) && validSquare(uciLower.slice(2, uciLower.length())) // P@b4
        // Promotions
        case (false, true, 5 | 6) =>
          validSquarePair(uciLower.slice(0, uciLower.length() - 1)) // d8d9+ | d8d9R | d8e9+
        // moves
        case (false, false, 4 | 5) => validSquarePair(uciLower) // d8d9 | d9d10

        // Bleh
        case _ => false
      }
    }

  private case class LexicalUciImpl(uci: String) extends LexicalUci
}
