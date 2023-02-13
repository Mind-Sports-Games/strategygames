package strategygames.opening

abstract class Ecopening(
    val eco: Ecopening.ECO,
    val variantGrouping: String,
    val family: Ecopening.FamilyName,
    val name: String,
    val moves: String,
    val fen: Ecopening.FEN,
    val lastMoveUci: String
) extends Ordered[Ecopening] {

  def ecoName: String

  def compare(other: Ecopening): Int
  def possibleContinuation(other: Ecopening): Boolean
  val size: Int
  val moveList: List[String]

  override def toString: String
}

object Ecopening {
  type FamilyName = String
  type ECO        = String
  type FEN        = String

  final case class Chess(f: strategygames.chess.opening.Ecopening)
      extends Ecopening(
        f.eco,
        f.variantGrouping,
        f.family,
        f.name,
        f.moves,
        f.fen,
        f.lastMoveUci
      ) {
    override def toString                      = f.toString()
    def ecoName                                = s"${f.eco} ${f.name}"
    def compare(other: Ecopening)              = f.eco compare other.eco
    def possibleContinuation(other: Ecopening) =
      (f.variantGrouping == other.variantGrouping) &&
        ((f.lastMoveUci == "" && other.size == 1) || ((f.size + 1 == other.size) && (f.moveList == other.moveList.reverse.tail.reverse))) &&
        other.lastMoveUci != ""
    lazy val size                              = f.moveList.size
    lazy val moveList                          = f.moves.split(' ').toList
  }

  final case class Draughts(f: strategygames.draughts.opening.Ecopening)
      extends Ecopening(
        f.eco,
        f.variantGrouping,
        f.family,
        f.name,
        f.moves,
        f.fen,
        f.lastMoveUci
      ) {
    override def toString                      = f.toString()
    def ecoName                                = s"${f.eco} ${f.name}"
    def compare(other: Ecopening)              = f.eco compare other.eco
    def possibleContinuation(other: Ecopening) =
      (f.variantGrouping == other.variantGrouping) &&
        ((f.lastMoveUci == "" && other.size == 1) || ((f.size + 1 == other.size) && (f.moveList == other.moveList.reverse.tail.reverse))) &&
        other.lastMoveUci != ""
    lazy val size                              = f.moveList.size
    lazy val moveList                          = f.moves.split(' ').toList
  }

  final case class FairySF(f: strategygames.fairysf.opening.Ecopening)
      extends Ecopening(
        f.eco,
        f.variantGrouping,
        f.family,
        f.name,
        f.moves,
        f.fen,
        f.lastMoveUci
      ) {
    override def toString                      = f.toString()
    def ecoName                                = s"${f.eco} ${f.name}"
    def compare(other: Ecopening)              = f.eco compare other.eco
    def possibleContinuation(other: Ecopening) =
      (f.variantGrouping == other.variantGrouping) &&
        ((f.lastMoveUci == "" && other.size == 1) || ((f.size + 1 == other.size) && (f.moveList == other.moveList.reverse.tail.reverse))) &&
        other.lastMoveUci != ""
    lazy val size                              = f.moveList.size
    lazy val moveList                          = f.moves.split(' ').toList
  }

  final case class Samurai(f: strategygames.samurai.opening.Ecopening)
      extends Ecopening(
        f.eco,
        f.variantGrouping,
        f.family,
        f.name,
        f.moves,
        f.fen,
        f.lastMoveUci
      ) {
    override def toString = f.toString()

    def ecoName                                = s"${f.eco} ${f.name}"
    def compare(other: Ecopening)              = f.eco compare other.eco
    def possibleContinuation(other: Ecopening) =
      (f.variantGrouping == other.variantGrouping) &&
        ((f.lastMoveUci == "" && other.size == 1) || ((f.size + 1 == other.size) && (f.moveList == other.moveList.reverse.tail.reverse))) &&
        other.lastMoveUci != ""
    lazy val size                              = f.moveList.size
    lazy val moveList                          = f.moves.split(' ').toList
  }

  final case class Togyzkumalak(f: strategygames.togyzkumalak.opening.Ecopening)
      extends Ecopening(
        f.eco,
        f.variantGrouping,
        f.family,
        f.name,
        f.moves,
        f.fen,
        f.lastMoveUci
      ) {
    override def toString = f.toString()

    def ecoName                                = s"${f.eco} ${f.name}"
    def compare(other: Ecopening)              = f.eco compare other.eco
    def possibleContinuation(other: Ecopening) =
      (f.variantGrouping == other.variantGrouping) &&
        ((f.lastMoveUci == "" && other.size == 1) || ((f.size + 1 == other.size) && (f.moveList == other.moveList.reverse.tail.reverse))) &&
        other.lastMoveUci != ""
    lazy val size                              = f.moveList.size
    lazy val moveList                          = f.moves.split(' ').toList
  }

}
