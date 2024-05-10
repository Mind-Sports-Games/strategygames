package strategygames.go

import format.pgn.Binary
import org.specs2.matcher.ValidatedMatchers
import org.specs2.mutable.Specification

class GoBinaryTest extends Specification with ValidatedMatchers {

  import BinaryTestUtils._

  "Go write actionStrs" should {
    val s_b4  = writeMove("s@b4")
    val s_a1  = writeMove("s@a1")
    val s_s19 = writeMove("s@s19")

    "write single move s@b4" in {
      s_b4 must_== "01000000,00111010"
    }

    "write single move s@a1" in {
      s_a1 must_== "01000000,00000000"
    }

    "write single move s@s19" in {
      s_s19 must_== "01000001,01101000"
    }

    val p = writeMove("pass")
    "write pass" in {
      p must_== "00000000"
    }

    val ss = writeMove("ss:a4")
    "write selectedSquares a4" in {
      ss must_== "10000000,00000001,10000000,00111001"
    }

    val ss_empty = writeMove("ss:")
    "write selectedSquares empty" in {
      ss_empty must_== "10000000,00000000"
    }

    val ss_3stones = writeMove("ss:a1,f11,r19")
    "write selectedSquares 3 dead stones" in {
      ss_3stones must_== "10000000,00000011,10000000,00000000,10000000,11000011,10000001,01100111"
    }

  }

  "Go read actionStrs" should {
    val storedActionStrs = "01000000,00111010,00000000,01000000,00000000,01000000,00100101"
    val moves            = readMoves(storedActionStrs)

    "read moves " in {
      moves must_== List("s@b4", "pass", "s@a1", "s@s2").map(List(_))
    }

    val storedActionStrs2 = "01000000,00111010,00000000,00000000,10000000,00000001,10000000,00111010"
    val moves2            = readMoves(storedActionStrs2)

    "read moves " in {
      moves2 must_== List("s@b4", "pass", "pass", "ss:b4").map(List(_))
    }

    val storedActionStrs3 =
      "01000000,00111010,01000000,00000000,01000000,00100101,00000000,00000000,10000000,00000010,10000000,00111010,10000000,00100101"
    val moves3            = readMoves(storedActionStrs3)

    "read moves " in {
      moves3 must_== List("s@b4", "s@a1", "s@s2", "pass", "pass", "ss:b4,s2").map(List(_))
    }

  }

}

object BinaryTestUtils {

  // TODO multiaction: This needs to interface with the new go/format/pgn/Binary
  // Which now takes and gives back ActionStrs

  def showByte(b: Byte): String =
    "%08d" format {
      b & 0xff
    }.toBinaryString.toInt

  def writeMove(m: String): String =
    (Binary writeMove m).get map showByte mkString ","

  def readMove(m: String): String =
    readMoves(m).flatten.head

  def readMoves(m: String): strategygames.ActionStrs =
    (Binary readActionStrs m.split(',').toList.map(parseBinary)).get

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
