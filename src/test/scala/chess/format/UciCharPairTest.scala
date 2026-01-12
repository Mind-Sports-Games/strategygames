package strategygames.chess
package format

import Pos._
import Uci._

class UciCharPairTest extends ChessTest {

  // println(UciCharPair.implementation.pos2charMap.toList.sortBy(_._2.toInt))
  // println(UciCharPair.implementation.promotion2charMap.toList.sortBy(_._2.toInt))
  // println(UciCharPair.implementation.dropRole2charMap.toList.sortBy(_._2.toInt).map(x => x._1 -> x._2.toInt))

  "char pair encoding" should {

    def conv(uci: Uci) = UciCharPair(uci).toString

    val allMoves = for {
      orig <- Pos.all
      dest <- Pos.all
    } yield Move(orig, dest)
    val allPairs = allMoves.map(conv)

    "regular moves" in {
      conv(Move(A1, B1)) === "#$"
      conv(Move(A1, A2)) === "#+"
      conv(Move(H7, H8)) === "Zb"
    }
    "unicity" in {
      allPairs.distinct.size === allMoves.size
    }
    "no void char" in {
      allPairs.count(_ contains UciCharPair.implementation.voidChar) === 0
    }
    "promotions" in {
      conv(Move(B7, B8, Option(Queen))) === "Td"
      conv(Move(B7, C8, Option(Queen))) === "Te"
      conv(Move(B7, C8, Option(Knight))) === "T}"
    }
    "drops" in {
      conv(Drop(Pawn, A1)).head === '#'
      conv(Drop(Pawn, A1)).tail.head.toInt === 143
      conv(Drop(Queen, H8)).head === 'b'
      conv(Drop(Queen, H8)).tail.head.toInt === 139
    }
  }
}
