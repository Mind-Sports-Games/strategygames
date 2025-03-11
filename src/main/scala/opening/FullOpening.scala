package strategygames.opening

abstract class FullOpening(
    val eco: String,
    val name: String,
    val fen: String,
    val source: Option[String] = None
) {

  override def toString: String

  // TODO when wanting to use multiaction variants in openings we will
  // need to reconsider whether openings are done by plies or turns
  // and then update other code (including lila-ws)
  def atPly(ply: Int) = FullOpening.AtPly(this, ply)

}

object FullOpening {

  final case class Chess(f: strategygames.chess.opening.FullOpening)
      extends FullOpening(
        f.eco,
        f.name,
        f.fen
      ) {

    override def toString = f.toString()

  }

  final case class Draughts(f: strategygames.draughts.opening.FullOpening)
      extends FullOpening(
        f.code,
        f.name,
        f.fen,
        f.source
      ) {

    override def toString = f.toString()

  }

  final case class FairySF(f: strategygames.fairysf.opening.FullOpening)
      extends FullOpening(
        f.eco,
        f.name,
        f.fen
      ) {

    override def toString = f.toString()

  }

  final case class Samurai(f: strategygames.samurai.opening.FullOpening)
      extends FullOpening(
        f.eco,
        f.name,
        f.fen
      ) {

    override def toString = f.toString()

  }

  final case class Togyzkumalak(f: strategygames.togyzkumalak.opening.FullOpening)
      extends FullOpening(
        f.eco,
        f.name,
        f.fen
      ) {

    override def toString = f.toString()

  }

  final case class Go(f: strategygames.go.opening.FullOpening)
      extends FullOpening(
        f.eco,
        f.name,
        f.fen
      ) {

    override def toString = f.toString()

  }

  final case class Backgammon(f: strategygames.backgammon.opening.FullOpening)
      extends FullOpening(
        f.eco,
        f.name,
        f.fen
      ) {

    override def toString = f.toString()

  }

  final case class Abalone(f: strategygames.abalone.opening.FullOpening)
      extends FullOpening(
        f.eco,
        f.name,
        f.fen
      ) {

    override def toString = f.toString()

  }

  final case class Dameo(f: strategygames.dameo.opening.FullOpening)
      extends FullOpening(
        f.eco,
        f.name,
        f.fen
      ) {

    override def toString = f.toString()

  }

  case class AtPly(opening: FullOpening, ply: Int)

}
