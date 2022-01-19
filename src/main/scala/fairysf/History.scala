package strategygames.fairysf
import strategygames.{ GameFamily }

import format.Uci
import variant.{ Shogi, Variant }

case class History(
    lastMove: Option[Uci] = None,
    positionHashes: PositionHash = Array.empty,
    halfMoveClock: Int = 0,
    variant: Variant = Shogi
){
    def withLastMove(m: Uci) = copy(lastMove = Option(m))

    override def toString = {
    s"${lastMove.fold("-")(_.uci)}"
  }
}

object History {

    def make(
        lastMove: Option[String], //a2a4
        variant: Variant
    ): History = 
        History(
            lastMove = lastMove flatMap (moveOrDrop => Uci.apply(variant.gameFamily, moveOrDrop)),
            variant = variant
        )
}
