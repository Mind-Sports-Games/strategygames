import cats.data.Validated
import cats.syntax.either.*

package object strategygames {

  val P1 = Player.P1
  val P2 = Player.P2

  /** Sequential validation that returns Validated.
    * Use when each step depends on the previous (short-circuit on first error).
    * Internally uses Either for proper short-circuit semantics.
    */
  inline def validated[E, A](either: => Either[E, A]): Validated[E, A] = either.toValidated

  type PosInfo = (Piece, Int)

  type PieceMap = Map[Pos, PosInfo]

  type PositionHash = Array[Byte]

  type ActionStrs = Seq[Seq[String]]

  type VActionStrs = Vector[Vector[String]]

}
