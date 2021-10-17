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
case object ShogiPawn extends PromotableRole {
  val fairySFID = Role.shogiPawn
  val forsyth   = 'P'
  val binaryInt = 1
  val hashInt   = 8
  val storable  = false
}

case object ShogiLance extends PromotableRole {
  val fairySFID = Role.lance
  val forsyth   = 'L'
  val binaryInt = 2
  val hashInt   = 7
  val storable  = true
}
case object ShogiKnight extends PromotableRole {
  val fairySFID = Role.shogiKnight
  val forsyth   = 'N'
  val binaryInt = 3
  val hashInt   = 6
  val storable  = true
}

case object ShogiSilver extends PromotableRole {
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

case object ShogiBishop extends PromotableRole {
  val fairySFID = Role.bishop
  val forsyth   = 'B'
  val binaryInt = 6
  val hashInt   = 3
  val storable  = true
}

case object ShogiRook extends PromotableRole {
  val fairySFID = Role.rook
  val forsyth   = 'R'
  val binaryInt = 7
  val hashInt   = 2
  val storable  = true
}

case object ShogiKing extends PromotableRole {
  val fairySFID = Role.king
  val forsyth   = 'K'
  val binaryInt = 8
  val hashInt   = 1
  val storable  = true
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
    List(ShogiPawn, ShogiLance, ShogiKnight, ShogiSilver, ShogiGold, ShogiBishop, ShogiRook, ShogiKing)
  val allPromotable: List[PromotableRole] =
    List(ShogiPawn, ShogiLance, ShogiKnight, ShogiSilver, ShogiBishop, ShogiRook)
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
      case None    => ??? // ShogiPawn // TODO: this is probably wrong,
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
    // TODO: It's incomplete because none of the promoted pieces
    //       are currently represented.
    r match {
      case ShogiPawn                => Option(1)
      case ShogiLance | ShogiKnight => Option(3)
      case ShogiSilver | ShogiGold  => Option(5)
      case ShogiBishop              => Option(8)
      case ShogiRook                => Option(9)
      case _                        => None
    }
}
