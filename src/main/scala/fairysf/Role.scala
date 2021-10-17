package strategygames.fairysf

import strategygames.{ Black, Color, White }

sealed trait Role {
  val fairySfID: Int
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

/* These are all of the pieces that fairysf supports
 * My plan is to use the integer value (left column)
 * to represent the pieces when passing back and forth
+----+----------------+----------+
| ID | Name           | Betza    |
+----+----------------+----------+
| 62 |                |          |
+----+----------------+----------+
| 10 | aiwok          | RNF      |
+----+----------------+----------+
| 7  | alfil          | A        |
+----+----------------+----------+
| 14 | amazon         | QN       |
+----+----------------+----------+
| 12 | archbishop     | BN       |
+----+----------------+----------+
| 33 | banner         | RcpRnN   |
+----+----------------+----------+
| 11 | bers           | RF       |
+----+----------------+----------+
| 3  | bishop         | B        |
+----+----------------+----------+
| 16 | biskni         | mBcN     |
+----+----------------+----------+
| 25 | breakthrough   | fWfFcF   |
+----+----------------+----------+
| 27 | cannon         | mRcpR    |
+----+----------------+----------+
| 36 | centaur        | KN       |
+----+----------------+----------+
| 13 | chancellor     | RN       |
+----+----------------+----------+
| 24 | clobber        | cW       |
+----+----------------+----------+
| 35 | commoner       | K        |
+----+----------------+----------+
| 23 | dragonHorse    | BW       |
+----+----------------+----------+
| 31 | elephant       | nA       |
+----+----------------+----------+
| 6  | fers           | F        |
+----+----------------+----------+
| 8  | fersAlfil      | FA       |
+----+----------------+----------+
| 22 | gold           | WfF      |
+----+----------------+----------+
| 30 | horse          | nN       |
+----+----------------+----------+
| 26 | immobile       |          |
+----+----------------+----------+
| 28 | janggiCannon   | pR       |
+----+----------------+----------+
| 32 | janggiElephant | mafsmafW |
+----+----------------+----------+
| 63 | king           | K        |
+----+----------------+----------+
| 15 | knibis         | mNcB     |
+----+----------------+----------+
| 2  | knight         | N        |
+----+----------------+----------+
| 17 | kniroo         | mNcR     |
+----+----------------+----------+
| 20 | lance          | fR       |
+----+----------------+----------+
| 1  | pawn           | fmWfceF  |
+----+----------------+----------+
| 5  | queen          | Q        |
+----+----------------+----------+
| 4  | rook           | R        |
+----+----------------+----------+
| 18 | rookni         | mRcN     |
+----+----------------+----------+
| 21 | shogiKnight    | fN       |
+----+----------------+----------+
| 19 | shogiPawn      | fW       |
+----+----------------+----------+
| 9  | silver         | FfW      |
+----+----------------+----------+
| 29 | soldier        | fsW      |
+----+----------------+----------+
| 34 | wazir          | W        |
+----+----------------+----------+
 */

/** Promotable in antichess.
  */
case object ShogiPawn extends PromotableRole {
  val fairySfID = 19
  val forsyth   = 'P'
  val binaryInt = 1
  val hashInt   = 8
  val storable  = false
}

case object ShogiLance extends PromotableRole {
  val fairySfID = 20
  val forsyth   = 'L'
  val binaryInt = 2
  val hashInt   = 7
  val storable  = true
}
case object ShogiKnight extends PromotableRole {
  val fairySfID = 21
  val forsyth   = 'N'
  val binaryInt = 3
  val hashInt   = 6
  val storable  = true
}

case object ShogiSilver extends PromotableRole {
  val fairySfID = 9
  val forsyth   = 'S'
  val binaryInt = 4
  val hashInt   = 5
  val storable  = true
}

case object ShogiGold extends PromotableRole {
  val fairySfID = 22
  val forsyth   = 'G'
  val binaryInt = 5
  val hashInt   = 4
  val storable  = true
}

case object ShogiBishop extends PromotableRole {
  val fairySfID = 3
  val forsyth   = 'B'
  val binaryInt = 6
  val hashInt   = 3
  val storable  = true
}

case object ShogiRook extends PromotableRole {
  val fairySfID = 4
  val forsyth   = 'R'
  val binaryInt = 7
  val hashInt   = 2
  val storable  = true
}

case object ShogiKing extends PromotableRole {
  val fairySfID = 63
  val forsyth   = 'K'
  val binaryInt = 8
  val hashInt   = 1
  val storable  = true
}

object Role {

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
      case None    => sys.error("TODO: implement me")// ShogiPawn // TODO: this is probably wrong,
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
    r match {
      case ShogiPawn                => Option(1)
      case ShogiLance | ShogiKnight => Option(3)
      case ShogiSilver | ShogiGold  => Option(5)
      case ShogiBishop              => Option(8)
      case ShogiRook                => Option(9)
      case _                        => None
    }
}
