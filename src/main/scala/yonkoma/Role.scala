package strategygames.yonkoma

import strategygames.GameFamily

sealed trait Role {
  val forsyth: Char
  val forsythUpper: Char
  lazy val pgn: Char              = forsyth
  lazy val name                   = toString
  lazy val groundName             = s"${forsyth}-piece"
  val binaryInt: Int
  val hashInt: Int
  val storable: Boolean
  lazy val gameFamily: GameFamily = GameFamily.Yonkoma()
}

// the only piece in the game: each player has two of them
case object Checker extends Role {
  val forsyth      = 'c'
  val forsythUpper = 'C'
  val binaryInt    = 1
  val hashInt      = 0
  val storable     = false
}

object Role {

  val all: List[Role] = List(Checker)

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
  // lila's board storage reads a piece back by its binaryInt
  val allByBinaryInt: Map[Int, Role]                     = all map { r =>
    (r.binaryInt, r)
  } toMap

  def forsyth(c: Char): Option[Role] = allByForsyth get c

  def storable: List[Role] = all.filter(_.storable)

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

}
