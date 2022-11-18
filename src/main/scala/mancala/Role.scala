package strategygames.mancala

import strategygames.{ GameFamily, P1, P2, Player }

import cats.implicits._
import ornicar.scalalib.Zero

sealed trait Role {
  val forsyth: Char
  // lazy val forsythUpper: Char = forsyth.toUpper //this contradicts what the piece is now!
  lazy val pgn: Char              = forsyth
  lazy val name                   = toString
  lazy val groundName             = s"${forsyth}-piece"
  val binaryInt: Int
  lazy val hashInt: Int           = binaryInt
  lazy val valueOf: Option[Int]   = Option(1)
  lazy val gameFamily: GameFamily = GameFamily.Mancala()
  final def -(player: Player)     = Piece(player, this)
  final def p1                    = this - P1
  final def p2                    = this - P2
}

sealed trait PromotableRole extends Role

case object OneStone extends Role {
  val forsyth    = 'A'
  val binaryInt  = 1
}

case object TwoStone extends Role {
  val forsyth    = 'B'
  val binaryInt  = 2
}

case object ThreeStone extends Role {
  val forsyth    = 'C'
  val binaryInt  = 3
}

case object FourStone extends Role {
  val forsyth    = 'D'
  val binaryInt  = 4
}

case object FiveStone extends Role {
  val forsyth    = 'E'
  val binaryInt  = 5
}

case object SixStone extends Role {
  val forsyth    = 'F'
  val binaryInt  = 6
}

case object SevenStone extends Role {
  val forsyth    = 'G'
  val binaryInt  = 7
}

case object EightStone extends Role {
  val forsyth    = 'H'
  val binaryInt  = 8
}

case object NineStone extends Role {
  val forsyth    = 'I'
  val binaryInt  = 9
}

case object TenStone extends Role {
  val forsyth    = 'J'
  val binaryInt  = 10
}

case object ElevenStone extends Role {
  val forsyth    = 'K'
  val binaryInt  = 11
}

case object TwelveStone extends Role {
  val forsyth    = 'L'
  val binaryInt  = 12
}

case object ThirteenStone extends Role {
  val forsyth    = 'M'
  val binaryInt  = 13
}

case object FourteenStone extends Role {
  val forsyth    = 'N'
  val binaryInt  = 14
}

case object FifteenStone extends Role {
  val forsyth    = 'O'
  val binaryInt  = 15
}

case object SixteenStone extends Role {
  val forsyth    = 'P'
  val binaryInt  = 16
}

case object SeventeenStone extends Role {
  val forsyth    = 'Q'
  val binaryInt  = 17
}

case object EighteenStone extends Role {
  val forsyth    = 'R'
  val binaryInt  = 18
}

case object NineteenStone extends Role {
  val forsyth    = 'S'
  val binaryInt  = 19
}

case object TwentyStone extends Role {
  val forsyth    = 'T'
  val binaryInt  = 20
}

case object TwentyOneStone extends Role {
  val forsyth    = 'U'
  val binaryInt  = 21
}

case object TwentyTwoStone extends Role {
  val forsyth    = 'V'
  val binaryInt  = 22
}

case object TwentyThreeStone extends Role {
  val forsyth    = 'W'
  val binaryInt  = 23
}

case object TwentyFourStone extends Role {
  val forsyth    = 'X'
  val binaryInt  = 24
}

case object TwentyFiveStone extends Role {
  val forsyth    = 'Y'
  val binaryInt  = 25
}

case object TwentySixStone extends Role {
  val forsyth    = 'Z'
  val binaryInt  = 26
}

case object TwentySevenStone extends Role {
  val forsyth    = 'a'
  val binaryInt  = 27
}

case object TwentyEightStone extends Role {
  val forsyth    = 'b'
  val binaryInt  = 28
}

case object TwentyNineStone extends Role {
  val forsyth    = 'c'
  val binaryInt  = 29
}

case object ThirtyStone extends Role {
  val forsyth    = 'd'
  val binaryInt  = 30
}

case object ThirtyOneStone extends Role {
  val forsyth    = 'e'
  val binaryInt  = 31
}

case object ThirtyTwoStone extends Role {
  val forsyth    = 'f'
  val binaryInt  = 32
}

case object ThirtyThreeStone extends Role {
  val forsyth    = 'g'
  val binaryInt  = 33
}

case object ThirtyFourStone extends Role {
  val forsyth    = 'h'
  val binaryInt  = 34
}

case object ThirtyFiveStone extends Role {
  val forsyth    = 'i'
  val binaryInt  = 35
}

case object ThirtySixStone extends Role {
  val forsyth    = 'j'
  val binaryInt  = 36
}

case object ThirtySevenStone extends Role {
  val forsyth    = 'k'
  val binaryInt  = 37
}

case object ThirtyEightStone extends Role {
  val forsyth    = 'l'
  val binaryInt  = 38
}

case object ThirtyNineStone extends Role {
  val forsyth    = 'm'
  val binaryInt  = 39
}

case object FortyStone extends Role {
  val forsyth    = 'n'
  val binaryInt  = 40
}

case object FortyOneStone extends Role {
  val forsyth    = 'o'
  val binaryInt  = 41
}

case object FortyTwoStone extends Role {
  val forsyth    = 'p'
  val binaryInt  = 42
}

case object FortyThreeStone extends Role {
  val forsyth    = 'q'
  val binaryInt  = 43
}

case object FortyFourStone extends Role {
  val forsyth    = 'r'
  val binaryInt  = 44
}

case object FortyFiveStone extends Role {
  val forsyth    = 's'
  val binaryInt  = 45
}

case object FortySixStone extends Role {
  val forsyth    = 't'
  val binaryInt  = 46
}

case object FortySevenStone extends Role {
  val forsyth    = 'u'
  val binaryInt  = 47
}

case object FortyEightStone extends Role {
  val forsyth    = 'v'
  val binaryInt  = 48
}

object Role {

  val all: List[Role] =
    List(
      OneStone,
      TwoStone,
      ThreeStone,
      FourStone,
      FiveStone,
      SixStone,
      SevenStone,
      EightStone,
      NineStone,
      TenStone,
      ElevenStone,
      TwelveStone,
      ThirteenStone,
      FourteenStone,
      FifteenStone,
      SixteenStone,
      SeventeenStone,
      EighteenStone,
      NineteenStone,
      TwentyStone,
      TwentyOneStone,
      TwentyTwoStone,
      TwentyThreeStone,
      TwentyFourStone,
      TwentyFiveStone,
      TwentySixStone,
      TwentySevenStone,
      TwentyEightStone,
      TwentyNineStone,
      ThirtyStone,
      ThirtyOneStone,
      ThirtyTwoStone,
      ThirtyThreeStone,
      ThirtyFourStone,
      ThirtyFiveStone,
      ThirtySixStone,
      ThirtySevenStone,
      ThirtyEightStone,
      ThirtyNineStone,
      FortyStone,
      FortyOneStone,
      FortyTwoStone,
      FortyThreeStone,
      FortyFourStone,
      FortyFiveStone,
      FortySixStone,
      FortySevenStone,
      FortyEightStone
    )

  def allByGameFamily(gf: GameFamily): List[Role] =
    all.filter(r => r.gameFamily == gf)

  val allByForsyth: Map[Char, Role]                      = all map { r =>
    (r.forsyth, r)
  } toMap
  def allByForsyth(gf: GameFamily): Map[Char, Role]      = allByGameFamily(gf) map { r =>
    (r.forsyth, r)
  } toMap
  val allByPgn: Map[Char, Role]                          = all map { r =>
    (r.pgn, r)
  } toMap
  def allByPgn(gf: GameFamily): Map[Char, Role]          = allByGameFamily(gf) map { r =>
    (r.pgn, r)
  } toMap
  val allByName: Map[String, Role]                       = all map { r =>
    (r.name, r)
  } toMap
  def allByName(gf: GameFamily): Map[String, Role]       = allByGameFamily(gf) map { r =>
    (r.name, r)
  } toMap
  val allByGroundName: Map[String, Role]                 = all map { r =>
    (r.groundName, r)
  } toMap
  def allByGroundName(gf: GameFamily): Map[String, Role] = allByGameFamily(gf) map { r =>
    (r.groundName, r)
  } toMap
  val allByBinaryInt: Map[Int, Role]                     = all map { r =>
    (r.binaryInt, r)
  } toMap
  def allByBinaryInt(gf: GameFamily): Map[Int, Role]     = allByGameFamily(gf) map { r =>
    (r.binaryInt, r)
  } toMap
  val allByHashInt: Map[Int, Role]                       = all map { r =>
    (r.hashInt, r)
  } toMap

  def forsyth(c: Char): Option[Role] = allByForsyth get c

  def binaryInt(i: Int): Option[Role] = allByBinaryInt get i

  def hashInt(i: Int): Option[Role] = allByHashInt get i

  // only used in lila by insight module
  def pgnMoveToRole(gf: GameFamily, c: Char): Role =
    allByPgn(gf).get(c) match {
      case Some(r) => r
      case None    => sys.error("Could not find Role from pgnMove")
    }

  // unused by lila
  def javaSymbolToRole(s: String): Role =
    allByPgn
      .get(
        s.headOption match {
          case Some(c) => c
          case None    => 'P' // JavaRole.PAWN.symbol is ""
        }
      )
      .get

  def valueOf(r: Role): Option[Int] = r.valueOf

  val roleR  = s"([${allByForsyth.keys.mkString("")}])"
  val roleRr = s"([${allByForsyth.keys.map(k => s"${k.toLower}${k.toUpper}").mkString("")}]?)"
  val rolePR = s"([${allByForsyth.keys.map(k => s"${k.toLower}${k.toUpper}").mkString("")}]|\\+?)"

}
