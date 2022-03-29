package strategygames.oware

import strategygames.{ P2, Player, P1, GameFamily }

import cats.implicits._
import ornicar.scalalib.Zero

case class OwareRoleID(val id: Int)

sealed trait Role {
  val owareID: OwareRoleID
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

case object ZeroStone extends Role {
  val fairySFID  = Role.zeroStone
  val forsyth    = 'A'
  val binaryInt  = 1
  val hashInt    = 8
  val storable   = true
  val valueOf    = Option(0)
  val gameFamily = GameFamily.Oware()
}

case object OneStone extends Role {
  val fairySFID  = Role.oneStone
  val forsyth    = 'B'
  val binaryInt  = 2
  val hashInt    = 7
  val storable   = true
  val valueOf    = Option(1)
  val gameFamily = GameFamily.Oware()
}


object Role {
  //---------------------------------------------------
  // These are all of the pieces that oware supports
  // Internally, oware uses an enum to represent these
  // but we are just going to use ints for this.
  //
  // The init method prints them out for us to use.
  //---------------------------------------------------
  val zeroStone      = OwareRoleID(1)
  val oneStone       = OwareRoleID(2)
  val undefined      = OwareRoleID(0)

  val all: List[Role] =
    List(
      ZeroStone,
      OneStone
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
  def allByOwareID(gf: GameFamily): Map[Int, Role] = allByGameFamily(gf) map { r =>
    (r.owareID.id, r)
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

  def valueOf(r: Role): Option[Int] =
    // Taken from: https://en.wikipedia.org/wiki/Shogi_strategy
    // Merged with: https://github.com/WandererXII/lishogi/blob/master/modules/shogi/src/main/scala/Role.scala
    // https://en.wikipedia.org/wiki/Xiangqi#Approximate_relative_values_of_the_pieces
    r.valueOf

  val roleR = s"([${allByForsyth.keys.mkString("")}])"
  val roleRr = s"([${allByForsyth.keys.map(k => s"${k.toLower}${k.toUpper}").mkString("")}]?)"
  val rolePR = s"([${allByForsyth.keys.map(k => s"${k.toLower}${k.toUpper}").mkString("")}]|\\+?)"

}
