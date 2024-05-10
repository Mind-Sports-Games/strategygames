package strategygames.samurai

import strategygames.{ GameFamily, P1, P2, Player }

sealed trait Role {
  val forsyth: Char
  // lazy val forsythUpper: Char = forsyth.toUpper //this contradicts what the piece is now!
  lazy val pgn: Char              = forsyth
  lazy val name                   = toString
  lazy val groundName             = s"${forsyth}-piece"
  val binaryInt: Int
  lazy val hashInt: Int           = binaryInt
  lazy val valueOf: Option[Int]   = Option(1)
  lazy val gameFamily: GameFamily = GameFamily.Oware()
  final def -(player: Player)     = Piece(player, this)
  final def p1                    = this - P1
  final def p2                    = this - P2
}

sealed trait PromotableRole extends Role

case object Stone extends Role {
  val forsyth   = 's'
  val binaryInt = 1
}

object Role {

  val all: List[Role] =
    List(
      Stone
    )

  def defaultRole: Role = Stone

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
      case None    => sys.error(s"Could not find Role from pgnMove: $c (gf: $gf)")
    }

  // unused by lila
  def javaSymbolToRole(s: String): Role =
    allByPgn
      .get(
        s.headOption match {
          case Some(c) => c
          case None    => sys.error(s"Could not find Role from java symbol: $s")
        }
      )
      .get

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
  val rolePR = s"([${allByForsyth.keys.map(k => s"${k.toLower}${k.toUpper}").mkString("")}]|\\+?)"

}
