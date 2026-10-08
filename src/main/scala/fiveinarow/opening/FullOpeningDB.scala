package strategygames.fiveinarow.opening

import strategygames.fiveinarow.format.FEN
import strategygames.ActionStrs

object FullOpeningDB {

  def findByFen(fen: FEN): Option[FullOpening] = FullOpening.findByFen(fen)

  def searchInFens(fens: Vector[FEN]): Option[FullOpening] =
    fens.foldRight(Option.empty[FullOpening]) { case (fen, acc) =>
      acc orElse findByFen(fen)
    }

  def search(@annotation.nowarn actionStrs: ActionStrs): Option[FullOpening.AtPly] = None

}
