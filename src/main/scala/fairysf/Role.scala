package strategygames.fairysf

import strategygames.{ Black, Color, White }

case class FairySFRoleID(val id: Int)

sealed trait Role {
  val fairySFID: FairySFRoleID
  val forsyth: Char
  lazy val forsythUpper: Char = forsyth.toUpper
  lazy val pgn: Char          = forsythUpper
  lazy val name               = toString.toLowerCase
  val binaryInt: Int
  val hashInt: Int
  val storable: Boolean
  final def -(color: Color) = Piece(color, this)
  final def white           = this - White
  final def black           = this - Black
}
sealed trait PromotableRole extends Role

/** Promotable in antichess.
  */
case object ShogiPawn extends Role {
  val fairySFID = Role.shogiPawn
  val forsyth   = 'P'
  val binaryInt = 1
  val hashInt   = 8
  val storable  = true
}

case object ShogiLance extends Role {
  val fairySFID = Role.lance
  val forsyth   = 'L'
  val binaryInt = 2
  val hashInt   = 7
  val storable  = true
}
case object ShogiKnight extends Role {
  val fairySFID = Role.shogiKnight
  val forsyth   = 'N'
  val binaryInt = 3
  val hashInt   = 6
  val storable  = true
}

case object ShogiSilver extends Role {
  val fairySFID = Role.silver
  val forsyth   = 'S'
  val binaryInt = 4
  val hashInt   = 5
  val storable  = true
}

case object ShogiGold extends PromotableRole {
  val fairySFID = Role.gold
  val forsyth   = 'G'
  val binaryInt = 5
  val hashInt   = 4
  val storable  = true
}

case object ShogiBishop extends Role {
  val fairySFID = Role.bishop
  val forsyth   = 'B'
  val binaryInt = 6
  val hashInt   = 3
  val storable  = true
}

case object ShogiRook extends Role {
  val fairySFID = Role.rook
  val forsyth   = 'R'
  val binaryInt = 7
  val hashInt   = 2
  val storable  = true
}

case object ShogiKing extends Role {
  val fairySFID = Role.king
  val forsyth   = 'K'
  val binaryInt = 8
  val hashInt   = 1
  val storable  = false
}

case object ShogiHorse extends PromotableRole {
  val fairySFID = Role.horse
  val forsyth   = 'H'
  val binaryInt = 9
  val hashInt   = 9
  //depends if storable means
  //can a piece of this role be stored when captured
  //or if this role can be stored
  val storable  = true
}

case object ShogiDragon extends PromotableRole {
  val fairySFID = Role.dragonHorse
  val forsyth   = 'D'
  val binaryInt = 10
  val hashInt   = 10
  val storable  = true
}


case object XiangqiSoldier extends Role {
  val fairySFID = Role.soldier
  val forsyth   = 'P'
  val binaryInt = 1
  val hashInt   = 7
  val storable  = false
}

case object XiangqiCannon extends Role {
  val fairySFID = Role.lance
  val forsyth   = 'C'
  val binaryInt = 2
  val hashInt   = 6
  val storable  = false
}

case object XiangqiHorse extends Role {
  val fairySFID = Role.knight
  val forsyth   = 'N'
  val binaryInt = 3
  val hashInt   = 5
  val storable  = false
}

case object XiangqiElephant extends Role {
  val fairySFID = Role.elephant
  val forsyth   = 'B'
  val binaryInt = 4
  val hashInt   = 4
  val storable  = false
}

case object XiangqiRook extends Role {
  val fairySFID = Role.rook
  val forsyth   = 'R'
  val binaryInt = 5
  val hashInt   = 3
  val storable  = false
}

case object XiangqiAdvisor extends Role {
  //probably wrong. Wikipedia says the advisor can be named:
  //guards or ministers, and less commonly as assistants, mandarins, or warriors
  //or "scholar", "gentleman", "officer", "guardian", or "official"
  //none of which are in the fairysf list!
  val fairySFID = Role.archbishop
  val forsyth   = 'A'
  val binaryInt = 6
  val hashInt   = 2
  val storable  = false
}

case object XiangqiKing extends Role {
  val fairySFID = Role.king
  val forsyth   = 'K'
  val binaryInt = 7
  val hashInt   = 1
  val storable  = false
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
  val allByForsyth: Map[Char, Role] = all map { r =>
    (r.forsyth, r)
  } toMap
  val allByPgn: Map[Char, Role] = all map { r =>
    (r.pgn, r)
  } toMap
  val allByName: Map[String, Role] = all map { r =>
    (r.name, r)
  } toMap
  val allByBinaryInt: Map[Int, Role] = all map { r =>
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
    allPromotableByForsyth get c

  def promotable(name: String): Option[PromotableRole] =
    allPromotableByName get name.capitalize

  def promotable(name: Option[String]): Option[PromotableRole] =
    name flatMap promotable

  def storable: List[Role] = all.filter(_.storable)

  def pgnMoveToRole(c: Char): Role =
    allByPgn.get(c) match {
      case Some(r) => r
      case None    => ShogiPawn //TODO: ??? //this is probably wrong,
    }

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
    r match {
      case ShogiPawn                => Option(1)
      case ShogiLance | ShogiKnight => Option(3)
      case ShogiSilver | ShogiGold  => Option(5)
      case ShogiBishop              => Option(8)
      case ShogiRook | ShogiHorse   => Option(10)
      case ShogiDragon              => Option(12)
      case ShogiKing                => None
    }
}
