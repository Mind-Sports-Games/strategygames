package strategygames.chess

import strategygames.Player

final case class Castles(
    p1KingSide: Boolean,
    p1QueenSide: Boolean,
    p2KingSide: Boolean,
    p2QueenSide: Boolean
) {

  def can(player: Player) = new Castles.Can(this, player)

  def without(player: Player) =
    player match {
      case P1 =>
        copy(
          p1KingSide = false,
          p1QueenSide = false
        )
      case P2 =>
        copy(
          p2KingSide = false,
          p2QueenSide = false
        )
    }

  def without(player: Player, side: Side) =
    (player, side) match {
      case (P1, KingSide)  => copy(p1KingSide = false)
      case (P1, QueenSide) => copy(p1QueenSide = false)
      case (P2, KingSide)  => copy(p2KingSide = false)
      case (P2, QueenSide) => copy(p2QueenSide = false)
    }

  def add(player: Player, side: Side) =
    (player, side) match {
      case (P1, KingSide)  => copy(p1KingSide = true)
      case (P1, QueenSide) => copy(p1QueenSide = true)
      case (P2, KingSide)  => copy(p2KingSide = true)
      case (P2, QueenSide) => copy(p2QueenSide = true)
    }

  override lazy val toString: String = {
    (if (p1KingSide) "K" else "") +
      (if (p1QueenSide) "Q" else "") +
      (if (p2KingSide) "k" else "") +
      (if (p2QueenSide) "q" else "")
  } match {
    case "" => "-"
    case n  => n
  }

  def toSeq = Array(p1KingSide, p1QueenSide, p2KingSide, p2QueenSide)

  def isEmpty = !(p1KingSide || p1QueenSide || p2KingSide || p2QueenSide)
}

object Castles {

  def apply(
      castles: (Boolean, Boolean, Boolean, Boolean)
  ): Castles =
    new Castles(
      p1KingSide = castles._1,
      p1QueenSide = castles._2,
      p2KingSide = castles._3,
      p2QueenSide = castles._4
    )

  def apply(str: String): Castles =
    new Castles(
      str contains 'K',
      str contains 'Q',
      str contains 'k',
      str contains 'q'
    )

  val all  = new Castles(true, true, true, true)
  val none = new Castles(false, false, false, false)
  def init = all

  final class Can(castles: Castles, player: Player) {
    def on(side: Side): Boolean =
      (player, side) match {
        case (P1, KingSide)  => castles.p1KingSide
        case (P1, QueenSide) => castles.p1QueenSide
        case (P2, KingSide)  => castles.p2KingSide
        case (P2, QueenSide) => castles.p2QueenSide
      }
    def any = on(KingSide) || on(QueenSide)
  }
}
