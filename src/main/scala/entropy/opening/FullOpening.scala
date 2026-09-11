package strategygames.entropy.opening

import strategygames.entropy.format.FEN

final class FullOpening(
    val eco: String,
    val name: String,
    val fen: String
) {

  def ecoName = s"$eco $name"

  override def toString = ecoName

  def atPly(ply: Int) = FullOpening.AtPly(this, ply)
}

object FullOpening {

  case class AtPly(opening: FullOpening, ply: Int)

  // entropy has no opening book: every game begins from an empty board and a full bag
  val all: Vector[FullOpening] = Vector.empty

  val byFen: Map[String, FullOpening] = Map.empty

  def findByFen(fen: FEN): Option[FullOpening] = byFen get fen.value

}
