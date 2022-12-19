package strategygames

import variant.Variant

sealed abstract class Board(
    val pieces: PieceMap,
    val history: History,
    val variant: Variant,
    val pocketData: Option[PocketData] = None
) {

  def apply(at: Pos): Option[Piece] = (pieces get at).map(_._1)

  def hasPiece(p: Piece) = pieces.values.map(_._1) exists (p ==)

  def withHistory(h: History): Board

  def situationOf(player: Player): Situation

  def valid(strict: Boolean) = variant.valid(this, strict)

  def materialImbalance: Int

  override def toString: String

  // TODO: there is probably a better way to generalize this.
  def copy(history: History, variant: Variant): Board
  def copy(history: History): Board

  // TODO: Yup, still not type safe. :D
  def toChess: chess.Board
  def toDraughts: draughts.Board
  def toFairySF: fairysf.Board
  def toMancala: mancala.Board
}

object Board {

  case class Chess(b: chess.Board)
      extends Board(
        b.pieces.map { case (pos, piece) => (Pos.Chess(pos), (Piece.Chess(piece), 1)) },
        History.Chess(b.history),
        Variant.Chess(b.variant),
        b.pocketData.map(PocketData.Chess)
      ) {

    def withHistory(h: History): Board = h match {
      case History.Chess(h) => Chess(b.withHistory(h))
      case _                => sys.error("Not passed Chess objects")
    }

    def situationOf(player: Player): Situation = Situation.Chess(b.situationOf(player))

    def materialImbalance: Int = b.materialImbalance

    override def toString: String = b.toString

    def copy(history: History, variant: Variant): Board = (history, variant) match {
      case (History.Chess(history), Variant.Chess(variant)) =>
        Chess(b.copy(history = history, variant = variant))
      case _                                                => sys.error("Unable to copy a chess board with non-chess arguments")
    }
    def copy(history: History): Board                   = history match {
      case History.Chess(history) => Chess(b.copy(history = history))
      case _                      => sys.error("Unable to copy a chess board with non-chess arguments")
    }

    def toChess    = b
    def toDraughts = sys.error("Can't make a draughts board from a chess board")
    def toFairySF  = sys.error("Can't make a fairysf board from a chess board")
    def toMancala  = sys.error("Can't make a mancala board from a chess board")

  }

  case class Draughts(b: draughts.Board)
      extends Board(
        b.pieces.map { case (pos, piece) => (Pos.Draughts(pos), (Piece.Draughts(piece), 1)) },
        History.Draughts(b.history),
        Variant.Draughts(b.variant)
      ) {

    def withHistory(h: History): Board = h match {
      case History.Draughts(h) => Draughts(b.withHistory(h))
      case _                   => sys.error("Not passed Draughts objects")
    }

    def situationOf(player: Player): Situation = Situation.Draughts(b.situationOf(player))

    def materialImbalance: Int = b.materialImbalance

    override def toString: String = b.toString

    def copy(history: History, variant: Variant): Board = (history, variant) match {
      case (History.Draughts(history), Variant.Draughts(variant)) =>
        Draughts(b.copy(history = history, variant = variant))
      case _                                                      => sys.error("Unable to copy a draughts board with non-draughts arguments")
    }
    def copy(history: History): Board                   = history match {
      case History.Draughts(history) => Draughts(b.copy(history = history))
      case _                         => sys.error("Unable to copy a draughts board with non-draughts arguments")
    }

    def toDraughts = b
    def toChess    = sys.error("Can't make a chess board from a draughts board")
    def toFairySF  = sys.error("Can't make a fairysf board from a draughts board")
    def toMancala  = sys.error("Can't make a mancala board from a draughts board")

  }

  case class FairySF(b: fairysf.Board)
      extends Board(
        b.pieces.map { case (pos, piece) => (Pos.FairySF(pos), (Piece.FairySF(piece), 1)) },
        History.FairySF(b.history),
        Variant.FairySF(b.variant),
        b.pocketData.map(PocketData.FairySF)
      ) {

    def withHistory(h: History): Board = h match {
      case History.FairySF(h) => FairySF(b.withHistory(h))
      case _                  => sys.error("Not passed FairySF objects")
    }

    def situationOf(player: Player): Situation = Situation.FairySF(b.situationOf(player))

    def materialImbalance: Int = b.materialImbalance

    override def toString: String = b.toString

    def copy(history: History, variant: Variant): Board = (history, variant) match {
      case (History.FairySF(history), Variant.FairySF(variant)) =>
        FairySF(b.copy(history = history, variant = variant))
      case _                                                    => sys.error("Unable to copy a fairysf board with non-fairysf arguments")
    }
    def copy(history: History): Board                   = history match {
      case History.FairySF(history) => FairySF(b.copy(history = history))
      case _                        => sys.error("Unable to copy a fairysf board with non-fairysf arguments")
    }

    def toFairySF  = b
    def toChess    = sys.error("Can't make a chess board from a fairysf board")
    def toDraughts = sys.error("Can't make a draughts board from a fairysf board")
    def toMancala  = sys.error("Can't make a mancala board from a fairysf board")

  }

  case class Mancala(b: mancala.Board)
      extends Board(
        b.pieces.map { case (pos, (piece, count)) => (Pos.Mancala(pos), (Piece.Mancala(piece), count)) },
        History.Mancala(b.history),
        Variant.Mancala(b.variant)
      ) {

    def withHistory(h: History): Board = h match {
      case History.Mancala(h) => Mancala(b.withHistory(h))
      case _                  => sys.error("Not passed mancala objects")
    }

    def situationOf(player: Player): Situation = Situation.Mancala(b.situationOf(player))

    def materialImbalance: Int = b.materialImbalance

    override def toString: String = b.toString

    def copy(history: History, variant: Variant): Board = (history, variant) match {
      case (History.Mancala(history), Variant.Mancala(variant)) =>
        Mancala(b.copy(history = history, variant = variant))
      case _                                                    => sys.error("Unable to copy a mancala board with non-mancala arguments")
    }
    def copy(history: History): Board                   = history match {
      case History.Mancala(history) => Mancala(b.copy(history = history))
      case _                        => sys.error("Unable to copy a mancala board with non-mancala arguments")
    }

    def toFairySF  = sys.error("Can't make a fairysf board from a mancala board")
    def toChess    = sys.error("Can't make a chess board from a mancala board")
    def toDraughts = sys.error("Can't make a draughts board from a mancala board")
    def toMancala  = b

  }

  def apply(lib: GameLogic, pieces: Iterable[(Pos, (Piece, Int))], variant: Variant): Board =
    (lib, variant) match {
      case (GameLogic.Draughts(), Variant.Draughts(variant)) =>
        Draughts(
          draughts.Board.apply(
            pieces.map { case (Pos.Draughts(pos), (Piece.Draughts(piece), _)) => (pos, piece) },
            variant
          )
        )
      case (GameLogic.Chess(), Variant.Chess(variant))       =>
        Chess(
          chess.Board.apply(
            pieces.map { case (Pos.Chess(pos), (Piece.Chess(piece), _)) => (pos, piece) },
            variant
          )
        )
      case (GameLogic.FairySF(), Variant.FairySF(variant))   =>
        FairySF(
          fairysf.Board.apply(
            pieces.map { case (Pos.FairySF(pos), (Piece.FairySF(piece), _)) => (pos, piece) },
            variant
          )
        )
      case (GameLogic.Mancala(), Variant.Mancala(variant))   =>
        Mancala(
          mancala.Board.apply(
            pieces.map { case (Pos.Mancala(pos), (Piece.Mancala(piece), count)) => (pos, (piece, count)) },
            variant
          )
        )
      case _                                                 => sys.error("Mismatched gamelogic types 27")
    }

  implicit def chessBoard(b: chess.Board)       = Board.Chess(b)
  implicit def draughtsBoard(b: draughts.Board) = Board.Draughts(b)
  implicit def fairysfBoard(b: fairysf.Board)   = Board.FairySF(b)
  implicit def mancalaBoard(b: mancala.Board)   = Board.Mancala(b)

  def init(lib: GameLogic, variant: Variant): Board = (lib, variant) match {
    case (GameLogic.Draughts(), Variant.Draughts(variant)) => Draughts(draughts.Board.init(variant))
    case (GameLogic.Chess(), Variant.Chess(variant))       => Chess(chess.Board.init(variant))
    case (GameLogic.Chess(), Variant.FairySF(variant))     => FairySF(fairysf.Board.init(variant))
    case (GameLogic.Mancala(), Variant.Mancala(variant))   => Mancala(mancala.Board.init(variant))
    case _                                                 => sys.error("Mismatched gamelogic types 28")
  }

}
