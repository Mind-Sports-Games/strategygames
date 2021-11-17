package strategygames.fairysf

import strategygames.{ Black, Color, White, GameFamily }

import cats.implicits._

case class FairySFRoleID(val id: Int)

sealed trait Role {
  val fairySFID: FairySFRoleID
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
  final def -(color: Color) = Piece(color, this)
  final def white           = this - White
  final def black           = this - Black
}
sealed trait PromotableRole extends Role

/** Promotable in antichess.
  */
case object ShogiPawn extends Role {
  val fairySFID  = Role.shogiPawn
  val forsyth    = 'P'
  val binaryInt  = 1
  val hashInt    = 8
  val storable   = true
  val valueOf    = Option(1)
  val gameFamily = GameFamily.Shogi()
}

case object ShogiLance extends Role {
  val fairySFID = Role.lance
  val forsyth   = 'L'
  val binaryInt = 2
  val hashInt   = 7
  val storable  = true
  val valueOf   = Option(3)
  val gameFamily = GameFamily.Shogi()
}

case object ShogiKnight extends Role {
  val fairySFID = Role.shogiKnight
  val forsyth   = 'N'
  val binaryInt = 3
  val hashInt   = 6
  val storable  = true
  val valueOf   = Option(3)
  val gameFamily = GameFamily.Shogi()
}

case object ShogiSilver extends Role {
  val fairySFID = Role.silver
  val forsyth   = 'S'
  val binaryInt = 4
  val hashInt   = 5
  val storable  = true
  val valueOf   = Option(5)
  val gameFamily = GameFamily.Shogi()
}

case object ShogiGold extends PromotableRole {
  val fairySFID = Role.gold
  val forsyth   = 'G'
  val binaryInt = 5
  val hashInt   = 4
  val storable  = true
  val valueOf   = Option(5)
  val gameFamily = GameFamily.Shogi()
}

case object ShogiBishop extends Role {
  val fairySFID = Role.bishop
  val forsyth   = 'B'
  val binaryInt = 6
  val hashInt   = 3
  val storable  = true
  val valueOf   = Option(8)
  val gameFamily = GameFamily.Shogi()
}

case object ShogiRook extends Role {
  val fairySFID = Role.rook
  val forsyth   = 'R'
  val binaryInt = 7
  val hashInt   = 2
  val storable  = true
  val valueOf   = Option(10)
  val gameFamily = GameFamily.Shogi()
}

case object ShogiKing extends Role {
  val fairySFID = Role.king
  val forsyth   = 'K'
  val binaryInt = 8
  val hashInt   = 1
  val storable  = false
  val valueOf   = None
  val gameFamily = GameFamily.Shogi()
}

case object ShogiHorse extends PromotableRole {
  val fairySFID = Role.dragonHorse
  val forsyth   = 'H'
  val binaryInt = 9
  val hashInt   = 9
  //depends if storable means
  //can a piece of this role be stored when captured
  //or if this role can be stored
  val storable  = true
  val valueOf   = Option(10)
  val gameFamily = GameFamily.Shogi()
}

case object ShogiDragon extends PromotableRole {
  val fairySFID = Role.bers
  val forsyth   = 'D'
  val binaryInt = 10
  val hashInt   = 10
  val storable  = true
  val valueOf   = Option(12)
  val gameFamily = GameFamily.Shogi()
}


case object XiangqiSoldier extends Role {
  val fairySFID = Role.soldier
  val forsyth   = 'P'
  val binaryInt = 1
  val hashInt   = 7
  val storable  = false
  val valueOf   = Option(1)
  val gameFamily = GameFamily.Xiangqi()
}

case object XiangqiCannon extends Role {
  val fairySFID = Role.cannon
  val forsyth   = 'C'
  val binaryInt = 2
  val hashInt   = 6
  val storable  = false
  val valueOf   = Option(5)
  val gameFamily = GameFamily.Xiangqi()
}

case object XiangqiHorse extends Role {
  val fairySFID = Role.horse
  val forsyth   = 'N'
  val binaryInt = 3
  val hashInt   = 5
  val storable  = false
  val valueOf   = Option(4)
  val gameFamily = GameFamily.Xiangqi()
}

case object XiangqiElephant extends Role {
  val fairySFID = Role.elephant
  val forsyth   = 'B'
  val binaryInt = 4
  val hashInt   = 4
  val storable  = false
  val valueOf   = Option(2)
  val gameFamily = GameFamily.Xiangqi()
}

case object XiangqiRook extends Role {
  val fairySFID = Role.rook
  val forsyth   = 'R'
  val binaryInt = 5
  val hashInt   = 3
  val storable  = false
  val valueOf   = Option(9)
  val gameFamily = GameFamily.Xiangqi()
}

case object XiangqiAdvisor extends Role {
  val fairySFID = Role.fers
  val forsyth   = 'A'
  val binaryInt = 6
  val hashInt   = 2
  val storable  = false
  val valueOf   = Option(2)
  val gameFamily = GameFamily.Xiangqi()
}

case object XiangqiKing extends Role {
  val fairySFID = Role.king
  val forsyth   = 'K'
  val binaryInt = 7
  val hashInt   = 1
  val storable  = false
  val valueOf   = None
  val gameFamily = GameFamily.Xiangqi()
}

object Role {
  //---------------------------------------------------
  // These are all of the pieces that fairysf supports
  // Internally, Fairsf uses an enum to represent these
  // but we are just going to use ints for this.
  //
  // The init method prints them out for us to use.
  //---------------------------------------------------
  val aiwok          = FairySFRoleID(10)
  val alfil          = FairySFRoleID(7)
  val amazon         = FairySFRoleID(14)
  val archbishop     = FairySFRoleID(12)
  val banner         = FairySFRoleID(33)
  val bers           = FairySFRoleID(11)
  val bishop         = FairySFRoleID(3)
  val biskni         = FairySFRoleID(16)
  val breakthrough   = FairySFRoleID(25)
  val cannon         = FairySFRoleID(27)
  val centaur        = FairySFRoleID(36)
  val chancellor     = FairySFRoleID(13)
  val clobber        = FairySFRoleID(24)
  val commoner       = FairySFRoleID(35)
  val dragonHorse    = FairySFRoleID(23)
  val elephant       = FairySFRoleID(31)
  val fers           = FairySFRoleID(6)
  val fersAlfil      = FairySFRoleID(8)
  val gold           = FairySFRoleID(22)
  val horse          = FairySFRoleID(30)
  val immobile       = FairySFRoleID(26)
  val janggiCannon   = FairySFRoleID(28)
  val janggiElephant = FairySFRoleID(32)
  val king           = FairySFRoleID(63)
  val knibis         = FairySFRoleID(15)
  val knight         = FairySFRoleID(2)
  val kniroo         = FairySFRoleID(17)
  val lance          = FairySFRoleID(20)
  val pawn           = FairySFRoleID(1)
  val queen          = FairySFRoleID(5)
  val rook           = FairySFRoleID(4)
  val rookni         = FairySFRoleID(18)
  val shogiKnight    = FairySFRoleID(21)
  val shogiPawn      = FairySFRoleID(19)
  val silver         = FairySFRoleID(9)
  val soldier        = FairySFRoleID(29)
  val wazir          = FairySFRoleID(34)

  val all: List[Role] =
    List(
      ShogiPawn,
      ShogiLance,
      ShogiKnight,
      ShogiSilver,
      ShogiGold,
      ShogiBishop,
      ShogiRook,
      ShogiKing,
      ShogiHorse,
      ShogiDragon,
      XiangqiSoldier,
      XiangqiAdvisor,
      XiangqiElephant,
      XiangqiHorse,
      XiangqiCannon,
      XiangqiRook,
      XiangqiKing
    )

  val allPromotable: List[PromotableRole] =
    List(ShogiGold, ShogiHorse, ShogiDragon)

  val promotionMap: Map[Role, PromotableRole] = Map(
    ShogiPawn   -> ShogiGold,
    ShogiLance  -> ShogiGold,
    ShogiKnight -> ShogiGold,
    ShogiSilver -> ShogiGold,
    ShogiBishop -> ShogiHorse,
    ShogiRook   -> ShogiDragon
  )
  def allByGameFamily(gf: GameFamily): List[Role] = all.filter(_.gameFamily == gf)
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
  val allPromotableByName: Map[String, PromotableRole] =
    allPromotable map { r =>
      (r.toString, r)
    } toMap
  val allPromotableByForsyth: Map[Char, PromotableRole] =
    allPromotable map { r =>
      (r.forsyth, r)
    } toMap
  val allPromotableByPgn: Map[Char, PromotableRole] =
    allPromotable map { r =>
      (r.pgn, r)
    } toMap
  val allByFairySFID: Map[Int, Role] = all map { r =>
    (r.fairySFID.id, r)
  } toMap

  def forsyth(c: Char): Option[Role] = allByForsyth get c

  def binaryInt(i: Int): Option[Role] = allByBinaryInt get i

  def hashInt(i: Int): Option[Role] = allByHashInt get i

  def promotable(c: Char): Option[PromotableRole] =
    (c match {
      case 'b' => ShogiHorse
      case 'r' => ShogiDragon
      case _ => ShogiGold
    }).some//allPromotableByForsyth.getOrElse(c.toUpper, ShogiGold.some)

  def promotable(name: String): Option[PromotableRole] =
    allPromotableByName get name.capitalize

  def promotable(name: Option[String]): Option[PromotableRole] =
    name flatMap promotable

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

  def valueOf(r: Role): Option[Int] =
    // Taken from: https://en.wikipedia.org/wiki/Shogi_strategy
    // Merged with: https://github.com/WandererXII/lishogi/blob/master/modules/shogi/src/main/scala/Role.scala
    // https://en.wikipedia.org/wiki/Xiangqi#Approximate_relative_values_of_the_pieces
    r.valueOf

  val roleR = s"([${allByForsyth.keys.mkString("")}])"

}
