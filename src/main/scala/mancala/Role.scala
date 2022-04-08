package strategygames.mancala

import strategygames.{ P2, Player, P1, GameFamily }

import cats.implicits._
import ornicar.scalalib.Zero

case class MancalaRoleID(val id: Int)

sealed trait Role {
  val mancalaID: MancalaRoleID
  val forsyth: Char
  lazy val forsythUpper: Char = forsyth.toUpper
  lazy val pgn: Char          = forsythUpper
  lazy val name               = toString.toLowerCase
  lazy val groundName         = s"${forsyth.toLower}-piece"
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
      TwentySixStone
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
