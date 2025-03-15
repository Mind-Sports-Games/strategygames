//package strategygames.abalone.format
//import strategygames.abalone._
//
//import cats.data.Validated
//import cats.implicits._
//
//@deprecated("Alex", since="1.5.5") sealed trait Uci {
//
//  def uci: String
//  def piotr: String
//
//  def origDest: (Pos, Pos)
//
//  def apply(situation: Situation): Validated[String, Move]
//}
//
//object Uci {
//
//  case class Move(
//      orig: Pos,
//      dest: Pos
//  ) extends Uci {
//
//    def keys = orig.key + dest.key
//    def uci  = keys
//
//    def keysPiotr = orig.piotrStr + dest.piotrStr
//    def piotr     = keysPiotr
//
//    def origDest = orig -> dest
//
//    def apply(situation: Situation) = situation.move(orig, dest)
//  }
//
//  object Move {
//
//    def apply(move: String): Option[Move] =
//      move match {
//        case moveR(orig, dest) =>
//          (
//            Pos.fromKey(orig),
//            Pos.fromKey(dest)
//          ) match {
//            case (Some(orig), Some(dest)) => {
//              Move(
//                orig = orig,
//                dest = dest
//              ).some
//            }
//            case _                        => None
//          }
//        case _                 => None
//      }
//
//    def piotr(move: String) =
//      for {
//        orig <- move.headOption flatMap Pos.piotr
//        dest <- move lift 1 flatMap Pos.piotr
//      } yield Move(orig, dest)
//
//    def fromStrings(origS: String, destS: String) =
//      for {
//        orig <- Pos.fromKey(origS)
//        dest <- Pos.fromKey(destS)
//      } yield Move(orig, dest)
//
//    val moveR = s"^${Pos.posR}${Pos.posR}".r
//  }
//
//  case class WithSan(uci: Uci, san: String)
//
//  def apply(move: strategygames.abalone.Move) = Uci.Move(move.orig, move.dest)
//
//  def apply(move: String) = Uci.Move(move)
//
//  def piotr(move: String): Option[Uci] = Uci.Move.piotr(move)
//
//  def readList(moves: String): Option[List[Uci]] =
//    moves.split(' ').toList.map(apply(_)).sequence
//
//  def writeList(moves: List[Uci]): String =
//    moves.map(_.uci) mkString " "
//
//  def readListPiotr(moves: String): Option[List[Uci]] =
//    moves.split(' ').toList.map(piotr).sequence
//
//  def writeListPiotr(moves: List[Uci]): String =
//    moves.map(_.piotr) mkString " "
//}
