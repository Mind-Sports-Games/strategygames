package strategygames.dameo

import strategygames.{ GameFamily, P1, P2, Player }

sealed trait Role {
  val forsyth: Char
  val forsythUpper: Char          = forsyth.toUpper
  lazy val pdn: Char              = forsyth
  lazy val name                   = toString
  lazy val groundName             = s"${forsyth}-piece"
  val binaryInt: Int
  val hashInt: Int                = binaryInt
  val valueOf: Option[Int]
  lazy val gameFamily: GameFamily = GameFamily.Dameo()
  final def -(player: Player)     = Piece(player, this)
  final def p1                    = this - P1
  final def p2                    = this - P2
}

sealed trait PromotableRole extends Role

//TODO Dameo - check forsyth is what we want. Depends on FEN format
case object King extends PromotableRole {
  val forsyth   = 'k'
  val binaryInt = 1
  val valueOf = Some(2)
}

case object Man extends Role {
  val forsyth   = 'm'
  val binaryInt = 2
  val valueOf = Some(1)
}

case object GhostMan extends Role {
  val forsyth   = 'g'
  val binaryInt = 4
  val valueOf = Some(0)
}

case object GhostKing extends Role {
  val forsyth   = 'p'
  val binaryInt = 3
  val valueOf = Some(0)
}

object Role {

  val all: List[Role] = List(King, Man)
  val allPromotable: List[PromotableRole] = List(King)

  def defaultRole: Role = Man

  def allByGameFamily(gf: GameFamily): List[Role] =
    all.filter(r => r.gameFamily == gf)

  val allByForsyth: Map[Char, Role]                      = all map { r =>
    (r.forsyth, r)
  } toMap
  def allByForsyth(gf: GameFamily): Map[Char, Role]      = allByGameFamily(gf) map { r =>
    (r.forsyth, r)
  } toMap
  val allByPdn: Map[Char, Role]                          = all map { r =>
    (r.pdn, r)
  } toMap
  def allByPdn(gf: GameFamily): Map[Char, Role]          = allByGameFamily(gf) map { r =>
    (r.pdn, r)
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
  val allPromotableByName: Map[String, PromotableRole]  = allPromotable.map(r => (r.toString, r)) toMap
  val allPromotableByForsyth: Map[Char, PromotableRole] = allPromotable.map(r => (r.forsyth, r)) toMap
  val allPromotableByPdn: Map[Char, PromotableRole]     = allPromotable.map(r => (r.pdn, r)) toMap
  val allPromotableByGroundName: Map[String, PromotableRole]                 =
    allPromotable map { r =>
      (r.groundName, r)
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

  // only used in lila by insight module
  def pdnMoveToRole(c: Char): Role =
    // We dont want ghosts to be returned here
    c match {
      case 'K' | 'O' => King
      case _         => Man
    }

  // unused by lila
  def javaSymbolToRole(s: String): Role =
    s match {
      case "" => Man
      case _  => King
    }

  // unused by lila
  def javaSymbolToInt(s: String): Int =
    s.headOption match {
      case Some(c) if c.toInt >= 65 && c.toInt <= 90  => c.toInt - 64
      case Some(c) if c.toInt >= 97 && c.toInt <= 122 => c.toInt - 70
      case _                                          => sys.error(s"Could not get Int from java symbol: $s")
    }

  def valueOf(r: Role): Option[Int] = r.valueOf

  val roleR  = s"([${allByForsyth.keys.mkString("")}])"
  val roleRr = s"([${allByForsyth.keys.map(k => s"${k.toLower}${k.toUpper}").mkString("")}]?)"

}
