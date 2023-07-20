package strategygames.go

import format.pgn.Binary
import org.specs2.matcher.ValidatedMatchers
import org.specs2.mutable.Specification

class GoBinaryTest extends Specification with ValidatedMatchers {

  import BinaryTestUtils._

  "Go write actions" should {
    val s_b4 = writeMove("s@b4")
    val s_a1 = writeMove("s@a1")

    "write single move s@b4" in {
      s_b4 must_== "01000000,00111010"
    }

    "write single move s@a1" in {
      s_a1 must_== "01000000,00000000"
    }

    val p = writeMove("pass")
    "write pass" in {
      p must_== "00000000"
    }
  }

  "Go read actions" should {
    val storedActions = "01000000,00111010,00000000,01000000,00000000,01000000,00100101"
    val moves         = readMoves(storedActions)

    "read moves " in {
      moves must_== List("s@b4", "pass", "s@a1", "s@s2")
    }
  }

}

object BinaryTestUtils {

  def showByte(b: Byte): String =
    "%08d" format {
      b & 0xff
    }.toBinaryString.toInt

  def writeMove(m: String): String =
    (Binary writeMove m).get map showByte mkString ","

  def readMove(m: String): String =
    readMoves(m).head

  def readMoves(m: String): List[String] =
    (Binary readMoves m.split(',').toList.map(parseBinary)).get

  def parseBinary(s: String): Byte = {
    var i    = s.length - 1
    var sum  = 0
    var mult = 1
    while (i >= 0) {
      s.charAt(i) match {
        case '1' => sum += mult
        case '0' =>
        case x   => sys error s"invalid binary literal: $x in $s"
      }
      mult *= 2
      i -= 1
    }
    sum.toByte
  }
}
