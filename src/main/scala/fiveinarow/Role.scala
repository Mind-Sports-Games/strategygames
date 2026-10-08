package strategygames.fiveinarow

import strategygames.{ GameFamily, Player }

sealed trait Role {
  val forsyth: Char
  lazy val forsythUpper: Char     = forsyth
  lazy val pgn: Char              = forsyth
  lazy val name                   = toString
  lazy val groundName             = s"${forsyth.toLower}-piece"
  val binaryInt: Int
  val hashInt: Int
  val storable: Boolean           = false
  lazy val valueOf: Option[Int]   = Option(1)
  lazy val gameFamily: GameFamily = GameFamily.FiveInARow()
  final def -(player: Player)     = Piece(player, this)
}

case object BlackStone extends Role {
  val forsyth   = 'B'
  val binaryInt = 1
  val hashInt   = 0
}

case object WhiteStone extends Role {
  val forsyth   = 'W'
  val binaryInt = 2
  val hashInt   = 1
}

object Role {

  val all: List[Role] = List(BlackStone, WhiteStone)

  def defaultRole: Role = BlackStone

  def allByGameFamily(gf: GameFamily): List[Role] = all.filter(_.gameFamily == gf)

  val allByForsyth: Map[Char, Role]                      = all.map(r => (r.forsyth, r)).toMap
  def allByForsyth(gf: GameFamily): Map[Char, Role]      = allByGameFamily(gf).map(r => (r.forsyth, r)).toMap
  val allByPgn: Map[Char, Role]                          = all.map(r => (r.pgn, r)).toMap
  def allByPgn(gf: GameFamily): Map[Char, Role]          = allByGameFamily(gf).map(r => (r.pgn, r)).toMap
  val allByName: Map[String, Role]                       = all.map(r => (r.name, r)).toMap
  def allByName(gf: GameFamily): Map[String, Role]       = allByGameFamily(gf).map(r => (r.name, r)).toMap
  val allByGroundName: Map[String, Role]                 = all.map(r => (r.groundName, r)).toMap
  def allByGroundName(gf: GameFamily): Map[String, Role] =
    allByGameFamily(gf).map(r => (r.groundName, r)).toMap
  val allByBinaryInt: Map[Int, Role]                     = all.map(r => (r.binaryInt, r)).toMap
  def allByBinaryInt(gf: GameFamily): Map[Int, Role]     =
    allByGameFamily(gf).map(r => (r.binaryInt, r)).toMap
  val allByHashInt: Map[Int, Role]                       = all.map(r => (r.hashInt, r)).toMap

  def forsyth(c: Char): Option[Role] = allByForsyth get c

  def binaryInt(i: Int): Option[Role] = allByBinaryInt get i

  def hashInt(i: Int): Option[Role] = allByHashInt get i

  def storable: List[Role] = all.filter(_.storable)

  def pgnMoveToRole(gf: GameFamily, c: Char): Role =
    allByPgn(gf).get(c) match {
      case Some(r) => r
      case None    => sys.error(s"Could not find Role from pgnMove: $c (gf: $gf)")
    }

  def javaSymbolToRole(s: String): Role =
    s.headOption.flatMap(allByPgn.get).getOrElse(sys.error(s"Could not find Role from java symbol: $s"))

  def javaSymbolToInt(s: String): Int = javaSymbolToRole(s).binaryInt

  def valueOf(r: Role): Option[Int] = r.valueOf

  val roleR = s"([${allByForsyth.keys.mkString("")}])"

}
