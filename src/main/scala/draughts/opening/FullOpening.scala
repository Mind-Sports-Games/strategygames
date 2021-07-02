package draughts
package opening

final class FullOpening(
    val code: String,
    val name: String,
    val fen: String,
    val source: Option[String]
) {

  def fullName = source.fold(s"$code: $name") { s => s"$s $code: $name" }

  override def toString = fullName

  def atPly(ply: Int) = FullOpening.AtPly(this, ply)
}

object FullOpening {

  case class AtPly(opening: FullOpening, ply: Int)
}
