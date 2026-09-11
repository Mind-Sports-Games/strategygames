package strategygames.entropy.opening

import strategygames.entropy.format.FEN
import strategygames.ActionStrs

object FullOpeningDB {

  def findByFen(fen: FEN): Option[FullOpening] = FullOpening.findByFen(fen)

  def searchInFens(fens: Vector[FEN]): Option[FullOpening] =
    fens.foldRight(none[FullOpening]) { case (fen, acc) =>
      acc orElse findByFen(fen)
    }

  def search(actionStrs: ActionStrs): Option[FullOpening.AtPly] = None

  private def none[A]: Option[A] = None

}
