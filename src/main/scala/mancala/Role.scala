package strategygames.mancala

import strategygames.{ P2, Player, P1, GameFamily }

import cats.implicits._
import ornicar.scalalib.Zero

case class MancalaRoleID(val id: Int)

sealed trait Role {
  val mancalaID: MancalaRoleID
  val forsyth: Char
  //lazy val forsythUpper: Char = forsyth.toUpper //this contradicts what the piece is now!
  lazy val pgn: Char          = forsyth
  lazy val name               = toString
  lazy val groundName         = s"${forsyth}-piece"
  val binaryInt: Int
  val hashInt: Int
  val storable: Boolean
  val valueOf: Option[Int]
  val gameFamily: GameFamily
  final def -(player: Player) = Piece(player, this)
  final def p1           = this - P1
  final def p2           = this - P2
}

sealed trait PromotableRole extends Role

case object OneStone extends Role {
  val mancalaID  = Role.oneStone
  val forsyth    = 'A'
  val binaryInt  = 1
  val hashInt    = 1
  val storable = true
  val gameFamily = GameFamily.Mancala()
  val valueOf = Option(1) 
}

case object TwoStone extends Role {
  val mancalaID  = Role.twoStone
  val forsyth    = 'B'
  val binaryInt  = 2
  val hashInt    = 2
  val storable = true
  val gameFamily = GameFamily.Mancala()
  val valueOf = Option(1) 
}

case object ThreeStone extends Role {
  val mancalaID  = Role.twoStone
  val forsyth    = 'C'
  val binaryInt  = 3
  val hashInt    = 3
  val storable = true
  val gameFamily = GameFamily.Mancala()
  val valueOf = Option(1) 
}

case object FourStone extends Role {
  val mancalaID  = Role.fourStone
  val forsyth    = 'D'
  val binaryInt  = 4
  val hashInt    = 4
  val storable = true
  val gameFamily = GameFamily.Mancala()
  val valueOf = Option(1)
}

case object FiveStone extends Role {
  val mancalaID  = Role.fiveStone
  val forsyth    = 'E'
  val binaryInt  = 5
  val hashInt    = 5
  val storable = true
  val gameFamily = GameFamily.Mancala()
  val valueOf = Option(1)
}

case object SixStone extends Role {
  val mancalaID  = Role.sixStone
  val forsyth    = 'F'
  val binaryInt  = 6
  val hashInt    = 6
  val storable = true
  val gameFamily = GameFamily.Mancala()
  val valueOf = Option(1)
}

case object SevenStone extends Role {
  val mancalaID  = Role.sevenStone
  val forsyth    = 'G'
  val binaryInt  = 7
  val hashInt    = 7
  val storable = true
  val gameFamily = GameFamily.Mancala()
  val valueOf = Option(1)
}

case object EightStone extends Role {
  val mancalaID  = Role.eightStone
  val forsyth    = 'H'
  val binaryInt  = 8
  val hashInt    = 8
  val storable = true
  val gameFamily = GameFamily.Mancala()
  val valueOf = Option(1)
}

case object NineStone extends Role {
  val mancalaID  = Role.nineStone
  val forsyth    = 'I'
  val binaryInt  = 9
  val hashInt    = 9
  val storable = true
  val gameFamily = GameFamily.Mancala()
  val valueOf = Option(1)
}

case object TenStone extends Role {
  val mancalaID  = Role.tenStone
  val forsyth    = 'J'
  val binaryInt  = 10
  val hashInt    = 10
  val storable = true
  val gameFamily = GameFamily.Mancala()
  val valueOf = Option(1)
}

case object ElevenStone extends Role {
  val mancalaID  = Role.elevenStone
  val forsyth    = 'K'
  val binaryInt  = 11
  val hashInt    = 11
  val storable = true
  val gameFamily = GameFamily.Mancala()
  val valueOf = Option(1)
}

case object TwelveStone extends Role {
  val mancalaID  = Role.twelveStone
  val forsyth    = 'L'
  val binaryInt  = 12
  val hashInt    = 12
  val storable = true
  val gameFamily = GameFamily.Mancala()
  val valueOf = Option(1)
}

case object ThirteenStone extends Role {
  val mancalaID  = Role.thirteenStone
  val forsyth    = 'M'
  val binaryInt  = 13
  val hashInt    = 13
  val storable = true
  val gameFamily = GameFamily.Mancala()
  val valueOf = Option(1)
}

case object FourteenStone extends Role {
  val mancalaID  = Role.fourteenStone
  val forsyth    = 'N'
  val binaryInt  = 14
  val hashInt    = 14
  val storable = true
  val gameFamily = GameFamily.Mancala()
  val valueOf = Option(1)
}

case object FifteenStone extends Role {
  val mancalaID  = Role.fifteenStone
  val forsyth    = 'O'
  val binaryInt  = 15
  val hashInt    = 15
  val storable = true
  val gameFamily = GameFamily.Mancala()
  val valueOf = Option(1)
}

case object SixteenStone extends Role {
  val mancalaID  = Role.sixteenStone
  val forsyth    = 'P'
  val binaryInt  = 16
  val hashInt    = 16
  val storable = true
  val gameFamily = GameFamily.Mancala()
  val valueOf = Option(1)
}

case object SeventeenStone extends Role {
  val mancalaID  = Role.seventeenStone
  val forsyth    = 'Q'
  val binaryInt  = 17
  val hashInt    = 17
  val storable = true
  val gameFamily = GameFamily.Mancala()
  val valueOf = Option(1)
}

case object EighteenStone extends Role {
  val mancalaID  = Role.eighteenStone
  val forsyth    = 'R'
  val binaryInt  = 18
  val hashInt    = 18
  val storable = true
  val gameFamily = GameFamily.Mancala()
  val valueOf = Option(1)
}

case object NineteenStone extends Role {
  val mancalaID  = Role.nineteenStone
  val forsyth    = 'S'
  val binaryInt  = 19
  val hashInt    = 19
  val storable = true
  val gameFamily = GameFamily.Mancala()
  val valueOf = Option(1)
}

case object TwentyStone extends Role {
  val mancalaID  = Role.twentyStone
  val forsyth    = 'T'
  val binaryInt  = 20
  val hashInt    = 20
  val storable = true
  val gameFamily = GameFamily.Mancala()
  val valueOf = Option(1)
}

case object TwentyOneStone extends Role {
  val mancalaID  = Role.twentyOneStone
  val forsyth    = 'U'
  val binaryInt  = 21
  val hashInt    = 21
  val storable = true
  val gameFamily = GameFamily.Mancala()
  val valueOf = Option(1)
}

case object TwentyTwoStone extends Role {
  val mancalaID  = Role.twentyTwoStone
  val forsyth    = 'V'
  val binaryInt  = 22
  val hashInt    = 22
  val storable = true
  val gameFamily = GameFamily.Mancala()
  val valueOf = Option(1)
}

case object TwentyThreeStone extends Role {
  val mancalaID  = Role.twentyThreeStone
  val forsyth    = 'W'
  val binaryInt  = 23
  val hashInt    = 23
  val storable = true
  val gameFamily = GameFamily.Mancala()
  val valueOf = Option(1)
}

case object TwentyFourStone extends Role {
  val mancalaID  = Role.twentyFourStone
  val forsyth    = 'X'
  val binaryInt  = 24
  val hashInt    = 24
  val storable = true
  val gameFamily = GameFamily.Mancala()
  val valueOf = Option(1)
}

case object TwentyFiveStone extends Role {
  val mancalaID  = Role.twentyFiveStone
  val forsyth    = 'Y'
  val binaryInt  = 25
  val hashInt    = 25
  val storable = true
  val gameFamily = GameFamily.Mancala()
  val valueOf = Option(1)
}

case object TwentySixStone extends Role {
  val mancalaID  = Role.twentySixStone
  val forsyth    = 'Z'
  val binaryInt  = 26
  val hashInt    = 26
  val storable = true
  val gameFamily = GameFamily.Mancala()
  val valueOf = Option(1)
}

case object TwentySevenStone extends Role {
  val mancalaID  = Role.twentySevenStone
  val forsyth    = 'a'
  val binaryInt  = 27
  val hashInt    = 27
  val storable = true
  val gameFamily = GameFamily.Mancala()
  val valueOf = Option(1)
}

case object TwentyEightStone extends Role {
  val mancalaID  = Role.twentyEightStone
  val forsyth    = 'b'
  val binaryInt  = 28
  val hashInt    = 28
  val storable = true
  val gameFamily = GameFamily.Mancala()
  val valueOf = Option(1)
}

case object TwentyNineStone extends Role {
  val mancalaID  = Role.twentyNineStone
  val forsyth    = 'c'
  val binaryInt  = 29
  val hashInt    = 29
  val storable = true
  val gameFamily = GameFamily.Mancala()
  val valueOf = Option(1)
}

case object ThirtyStone extends Role {
  val mancalaID  = Role.thirtyStone
  val forsyth    = 'd'
  val binaryInt  = 30
  val hashInt    = 30
  val storable = true
  val gameFamily = GameFamily.Mancala()
  val valueOf = Option(1)
}

case object ThirtyOneStone extends Role {
  val mancalaID  = Role.thirtyOneStone
  val forsyth    = 'e'
  val binaryInt  = 31
  val hashInt    = 31
  val storable = true
  val gameFamily = GameFamily.Mancala()
  val valueOf = Option(1)
}

case object ThirtyTwoStone extends Role {
  val mancalaID  = Role.thirtyTwoStone
  val forsyth    = 'f'
  val binaryInt  = 32
  val hashInt    = 32
  val storable = true
  val gameFamily = GameFamily.Mancala()
  val valueOf = Option(1)
}

case object ThirtyThreeStone extends Role {
  val mancalaID  = Role.thirtyThreeStone
  val forsyth    = 'g'
  val binaryInt  = 33
  val hashInt    = 33
  val storable = true
  val gameFamily = GameFamily.Mancala()
  val valueOf = Option(1)
}

case object ThirtyFourStone extends Role {
  val mancalaID  = Role.thirtyFourStone
  val forsyth    = 'h'
  val binaryInt  = 34
  val hashInt    = 34
  val storable = true
  val gameFamily = GameFamily.Mancala()
  val valueOf = Option(1)
}

case object ThirtyFiveStone extends Role {
  val mancalaID  = Role.thirtyFiveStone
  val forsyth    = 'i'
  val binaryInt  = 35
  val hashInt    = 35
  val storable = true
  val gameFamily = GameFamily.Mancala()
  val valueOf = Option(1)
}

case object ThirtySixStone extends Role {
  val mancalaID  = Role.thirtySixStone
  val forsyth    = 'j'
  val binaryInt  = 36
  val hashInt    = 36
  val storable = true
  val gameFamily = GameFamily.Mancala()
  val valueOf = Option(1)
}

case object ThirtySevenStone extends Role {
  val mancalaID  = Role.thirtySevenStone
  val forsyth    = 'k'
  val binaryInt  = 37
  val hashInt    = 37
  val storable = true
  val gameFamily = GameFamily.Mancala()
  val valueOf = Option(1)
}

case object ThirtyEightStone extends Role {
  val mancalaID  = Role.thirtyEightStone
  val forsyth    = 'l'
  val binaryInt  = 38
  val hashInt    = 38
  val storable = true
  val gameFamily = GameFamily.Mancala()
  val valueOf = Option(1)
}

case object ThirtyNineStone extends Role {
  val mancalaID  = Role.thirtyNineStone
  val forsyth    = 'm'
  val binaryInt  = 39
  val hashInt    = 39
  val storable = true
  val gameFamily = GameFamily.Mancala()
  val valueOf = Option(1)
}

case object FortyStone extends Role {
  val mancalaID  = Role.fortyStone
  val forsyth    = 'n'
  val binaryInt  = 40
  val hashInt    = 40
  val storable = true
  val gameFamily = GameFamily.Mancala()
  val valueOf = Option(1)
}

case object FortyOneStone extends Role {
  val mancalaID  = Role.fortyOneStone
  val forsyth    = 'o'
  val binaryInt  = 41
  val hashInt    = 41
  val storable = true
  val gameFamily = GameFamily.Mancala()
  val valueOf = Option(1)
}

case object FortyTwoStone extends Role {
  val mancalaID  = Role.fortyTwoStone
  val forsyth    = 'p'
  val binaryInt  = 42
  val hashInt    = 42
  val storable = true
  val gameFamily = GameFamily.Mancala()
  val valueOf = Option(1)
}

case object FortyThreeStone extends Role {
  val mancalaID  = Role.fortyThreeStone
  val forsyth    = 'q'
  val binaryInt  = 43
  val hashInt    = 43
  val storable = true
  val gameFamily = GameFamily.Mancala()
  val valueOf = Option(1)
}

case object FortyFourStone extends Role {
  val mancalaID  = Role.fortyFourStone
  val forsyth    = 'r'
  val binaryInt  = 44
  val hashInt    = 44
  val storable = true
  val gameFamily = GameFamily.Mancala()
  val valueOf = Option(1)
}

case object FortyFiveStone extends Role {
  val mancalaID  = Role.fortyFiveStone
  val forsyth    = 's'
  val binaryInt  = 45
  val hashInt    = 45
  val storable = true
  val gameFamily = GameFamily.Mancala()
  val valueOf = Option(1)
}

case object FortySixStone extends Role {
  val mancalaID  = Role.fortySixStone
  val forsyth    = 't'
  val binaryInt  = 46
  val hashInt    = 46
  val storable = true
  val gameFamily = GameFamily.Mancala()
  val valueOf = Option(1)
}

case object FortySevenStone extends Role {
  val mancalaID  = Role.fortySevenStone
  val forsyth    = 'u'
  val binaryInt  = 47
  val hashInt    = 47
  val storable = true
  val gameFamily = GameFamily.Mancala()
  val valueOf = Option(1)
}

case object FortyEightStone extends Role {
  val mancalaID  = Role.fortyEightStone
  val forsyth    = 'v'
  val binaryInt  = 48
  val hashInt    = 48
  val storable = true
  val gameFamily = GameFamily.Mancala()
  val valueOf = Option(1)
}

case object FortyNineStone extends Role {
  val mancalaID  = Role.fortyNineStone
  val forsyth    = 'w'
  val binaryInt  = 49
  val hashInt    = 49
  val storable = true
  val gameFamily = GameFamily.Mancala()
  val valueOf = Option(1)
}

case object FiftyStone extends Role {
  val mancalaID  = Role.fiftyStone
  val forsyth    = 'x'
  val binaryInt  = 50
  val hashInt    = 50
  val storable = true
  val gameFamily = GameFamily.Mancala()
  val valueOf = Option(1)
}

case object FiftyOneStone extends Role {
  val mancalaID  = Role.fiftyOneStone
  val forsyth    = 'y'
  val binaryInt  = 51
  val hashInt    = 51
  val storable = true
  val gameFamily = GameFamily.Mancala()
  val valueOf = Option(1)
}

case object FiftyTwoStone extends Role {
  val mancalaID  = Role.fiftyTwoStone
  val forsyth    = 'z'
  val binaryInt  = 52
  val hashInt    = 52
  val storable = true
  val gameFamily = GameFamily.Mancala()
  val valueOf = Option(1)
}

object Role {
  //---------------------------------------------------
  // These are all of the pieces that mancala supports
  // Internally, mancala uses an enum to represent these
  // but we are just going to use ints for this.
  //
  // The init method prints them out for us to use.
  //---------------------------------------------------
  val oneStone          = MancalaRoleID(1)
  val twoStone          = MancalaRoleID(2)
  val threeStone        = MancalaRoleID(3)
  val fourStone         = MancalaRoleID(4)
  val fiveStone         = MancalaRoleID(5)
  val sixStone          = MancalaRoleID(6)
  val sevenStone        = MancalaRoleID(7)
  val eightStone        = MancalaRoleID(8)
  val nineStone         = MancalaRoleID(9)
  val tenStone          = MancalaRoleID(10)
  val elevenStone       = MancalaRoleID(11)
  val twelveStone       = MancalaRoleID(12)
  val thirteenStone     = MancalaRoleID(13)
  val fourteenStone     = MancalaRoleID(14)
  val fifteenStone      = MancalaRoleID(15)
  val sixteenStone      = MancalaRoleID(16)
  val seventeenStone    = MancalaRoleID(17)
  val eighteenStone     = MancalaRoleID(18)
  val nineteenStone     = MancalaRoleID(19)
  val twentyStone       = MancalaRoleID(20)
  val twentyOneStone    = MancalaRoleID(21)
  val twentyTwoStone    = MancalaRoleID(22)
  val twentyThreeStone  = MancalaRoleID(23)
  val twentyFourStone   = MancalaRoleID(24)
  val twentyFiveStone   = MancalaRoleID(25)
  val twentySixStone    = MancalaRoleID(26)
  val twentySevenStone  = MancalaRoleID(27)
  val twentyEightStone  = MancalaRoleID(28)
  val twentyNineStone   = MancalaRoleID(29)
  val thirtyStone       = MancalaRoleID(30)
  val thirtyOneStone    = MancalaRoleID(31)
  val thirtyTwoStone    = MancalaRoleID(32)
  val thirtyThreeStone  = MancalaRoleID(33)
  val thirtyFourStone   = MancalaRoleID(34)
  val thirtyFiveStone   = MancalaRoleID(35)
  val thirtySixStone    = MancalaRoleID(36)
  val thirtySevenStone  = MancalaRoleID(37)
  val thirtyEightStone  = MancalaRoleID(38)
  val thirtyNineStone   = MancalaRoleID(39)
  val fortyStone        = MancalaRoleID(40)
  val fortyOneStone     = MancalaRoleID(41)
  val fortyTwoStone     = MancalaRoleID(42)
  val fortyThreeStone   = MancalaRoleID(43)
  val fortyFourStone    = MancalaRoleID(44)
  val fortyFiveStone    = MancalaRoleID(45)
  val fortySixStone     = MancalaRoleID(46)
  val fortySevenStone   = MancalaRoleID(47)
  val fortyEightStone   = MancalaRoleID(48)
  val fortyNineStone    = MancalaRoleID(49)
  val fiftyStone        = MancalaRoleID(50)
  val fiftyOneStone     = MancalaRoleID(51)
  val fiftyTwoStone     = MancalaRoleID(52)
  val undefined         = MancalaRoleID(0)

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
      FortyEightStone,
      FortyNineStone,
      FiftyStone,
      FiftyOneStone,
      FiftyTwoStone,
    )

  def allByGameFamily(gf: GameFamily): List[Role] =
    all.filter(r => r.gameFamily == gf)

  val allByForsyth: Map[Char, Role] = all map { r =>
    (r.forsyth, r)
  } toMap
  def allByForsyth(gf: GameFamily): Map[Char, Role] = allByGameFamily(gf) map { r =>
    (r.forsyth, r)
  } toMap
  val allByPgn: Map[Char, Role] = all map { r =>
    (r.pgn, r)
  } toMap
  def allByPgn(gf: GameFamily): Map[Char, Role] = allByGameFamily(gf) map { r =>
    (r.pgn, r)
  } toMap
  val allByName: Map[String, Role] = all map { r =>
    (r.name, r)
  } toMap
  def allByName(gf: GameFamily): Map[String, Role] = allByGameFamily(gf) map { r =>
    (r.name, r)
  } toMap
  val allByGroundName: Map[String, Role] = all map { r =>
    (r.groundName, r)
  } toMap
  def allByGroundName(gf: GameFamily): Map[String, Role] = allByGameFamily(gf) map { r =>
    (r.groundName, r)
  } toMap
  val allByBinaryInt: Map[Int, Role] = all map { r =>
    (r.binaryInt, r)
  } toMap
  def allByBinaryInt(gf: GameFamily): Map[Int, Role] = allByGameFamily(gf) map { r =>
    (r.binaryInt, r)
  } toMap
  val allByHashInt: Map[Int, Role] = all map { r =>
    (r.hashInt, r)
  } toMap
  def allByMancalaID(gf: GameFamily): Map[Int, Role] = allByGameFamily(gf) map { r =>
    (r.mancalaID.id, r)
  } toMap

  def forsyth(c: Char): Option[Role] = allByForsyth get c

  def binaryInt(i: Int): Option[Role] = allByBinaryInt get i

  def hashInt(i: Int): Option[Role] = allByHashInt get i

  def storable: List[Role] = all.filter(_.storable)

  //only used in lila by insight module
  def pgnMoveToRole(gf: GameFamily, c: Char): Role =
    allByPgn(gf).get(c) match {
      case Some(r) => r
      case None    => sys.error("Could not find Role from pgnMove")
    }

  //unused by lila
  def javaSymbolToRole(s: String): Role =
    allByPgn
      .get(
        s.headOption match {
          case Some(c) => c
          case None    => 'P' //JavaRole.PAWN.symbol is ""
        }
      )
      .get

  def valueOf(r: Role): Option[Int] = r.valueOf

  val roleR = s"([${allByForsyth.keys.mkString("")}])"
  val roleRr = s"([${allByForsyth.keys.map(k => s"${k.toLower}${k.toUpper}").mkString("")}]?)"
  val rolePR = s"([${allByForsyth.keys.map(k => s"${k.toLower}${k.toUpper}").mkString("")}]|\\+?)"

}
