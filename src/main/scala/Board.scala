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
  def toSamurai: samurai.Board
  def toTogyzkumalak: togyzkumalak.Board
  def toGo: go.Board
  def toBackgammon: backgammon.Board
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

    def toChess        = b
    def toDraughts     = sys.error("Can't make a draughts board from a chess board")
    def toFairySF      = sys.error("Can't make a fairysf board from a chess board")
    def toSamurai      = sys.error("Can't make a samurai board from a chess board")
    def toTogyzkumalak = sys.error("Can't make a togyzkumalak board from a chess board")
    def toGo           = sys.error("Can't make a go board from a chess board")
    def toBackgammon   = sys.error("Can't make a backgammon board from a chess board")

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

    def toDraughts     = b
    def toChess        = sys.error("Can't make a chess board from a draughts board")
    def toFairySF      = sys.error("Can't make a fairysf board from a draughts board")
    def toSamurai      = sys.error("Can't make a samurai board from a draughts board")
    def toTogyzkumalak = sys.error("Can't make a togyzkumalak board from a draughts board")
    def toGo           = sys.error("Can't make a go board from a draughts board")
    def toBackgammon   = sys.error("Can't make a backgammon board from a draughts board")

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

    def toFairySF      = b
    def toChess        = sys.error("Can't make a chess board from a fairysf board")
    def toDraughts     = sys.error("Can't make a draughts board from a fairysf board")
    def toSamurai      = sys.error("Can't make a samurai board from a fairysf board")
    def toTogyzkumalak = sys.error("Can't make a togyzkumalak board from a fairysf board")
    def toGo           = sys.error("Can't make a go board from a fairysf board")
    def toBackgammon   = sys.error("Can't make a backgammon board from a fairysf board")

  }

  case class Samurai(b: samurai.Board)
      extends Board(
        b.pieces.map { case (pos, (piece, count)) => (Pos.Samurai(pos), (Piece.Samurai(piece), count)) },
        History.Samurai(b.history),
        Variant.Samurai(b.variant)
      ) {

    def withHistory(h: History): Board = h match {
      case History.Samurai(h) => Samurai(b.withHistory(h))
      case _                  => sys.error("Not passed samurai objects")
    }

    def situationOf(player: Player): Situation = Situation.Samurai(b.situationOf(player))

    def materialImbalance: Int = b.materialImbalance

    override def toString: String = b.toString

    def copy(history: History, variant: Variant): Board = (history, variant) match {
      case (History.Samurai(history), Variant.Samurai(variant)) =>
        Samurai(b.copy(history = history, variant = variant))
      case _                                                    => sys.error("Unable to copy a samurai board with non-samurai arguments")
    }
    def copy(history: History): Board                   = history match {
      case History.Samurai(history) => Samurai(b.copy(history = history))
      case _                        => sys.error("Unable to copy a samurai board with non-samurai arguments")
    }

    def toFairySF      = sys.error("Can't make a fairysf board from a samurai board")
    def toChess        = sys.error("Can't make a chess board from a samurai board")
    def toDraughts     = sys.error("Can't make a draughts board from a samurai board")
    def toSamurai      = b
    def toTogyzkumalak = sys.error("Can't make a togyzkumalak board from a samurai board")
    def toGo           = sys.error("Can't make a go board from a samurai board")
    def toBackgammon   = sys.error("Can't make a backgammon board from a samurai board")

  }

  case class Togyzkumalak(b: togyzkumalak.Board)
      extends Board(
        b.pieces.map { case (pos, (piece, count)) =>
          (Pos.Togyzkumalak(pos), (Piece.Togyzkumalak(piece), count))
        },
        History.Togyzkumalak(b.history),
        Variant.Togyzkumalak(b.variant)
      ) {

    def withHistory(h: History): Board = h match {
      case History.Togyzkumalak(h) => Togyzkumalak(b.withHistory(h))
      case _                       => sys.error("Not passed togyzkumalak objects")
    }

    def situationOf(player: Player): Situation = Situation.Togyzkumalak(b.situationOf(player))

    def materialImbalance: Int = b.materialImbalance

    override def toString: String = b.toString

    def copy(history: History, variant: Variant): Board = (history, variant) match {
      case (History.Togyzkumalak(history), Variant.Togyzkumalak(variant)) =>
        Togyzkumalak(b.copy(history = history, variant = variant))
      case _                                                              => sys.error("Unable to copy a togyzkumalak board with non-togyzkumalak arguments")
    }
    def copy(history: History): Board                   = history match {
      case History.Togyzkumalak(history) => Togyzkumalak(b.copy(history = history))
      case _                             => sys.error("Unable to copy a togyzkumalak board with non-togyzkumalak arguments")
    }

    def toFairySF      = sys.error("Can't make a fairysf board from a togyzkumalak board")
    def toChess        = sys.error("Can't make a chess board from a togyzkumalak board")
    def toDraughts     = sys.error("Can't make a draughts board from a togyzkumalak board")
    def toSamurai      = sys.error("Can't make a samurai board from a togyzkumalak board")
    def toTogyzkumalak = b
    def toGo           = sys.error("Can't make a go board from a togyzkumalak board")
    def toBackgammon   = sys.error("Can't make a backgammon board from a togyzkumalak board")

  }

  case class Go(b: go.Board)
      extends Board(
        b.pieces.map { case (pos, piece) => (Pos.Go(pos), (Piece.Go(piece), 1)) },
        History.Go(b.history),
        Variant.Go(b.variant),
        b.pocketData.map(PocketData.Go)
      ) {

    def withHistory(h: History): Board = h match {
      case History.Go(h) => Go(b.withHistory(h))
      case _             => sys.error("Not passed go objects")
    }

    def situationOf(player: Player): Situation = Situation.Go(b.situationOf(player))

    def materialImbalance: Int = b.materialImbalance

    override def toString: String = b.toString

    def copy(history: History, variant: Variant): Board = (history, variant) match {
      case (History.Go(history), Variant.Go(variant)) =>
        Go(b.copy(history = history, variant = variant))
      case _                                          => sys.error("Unable to copy a go board with non-go arguments")
    }
    def copy(history: History): Board                   = history match {
      case History.Go(history) => Go(b.copy(history = history))
      case _                   => sys.error("Unable to copy a go board with non-go arguments")
    }

    def toFairySF      = sys.error("Can't make a fairysf board from a go board")
    def toChess        = sys.error("Can't make a chess board from a go board")
    def toDraughts     = sys.error("Can't make a draughts board from a go board")
    def toSamurai      = sys.error("Can't make a samurai board from a go board")
    def toTogyzkumalak = sys.error("Can't make a togyzkumalak board from a go board")
    def toGo           = b
    def toBackgammon   = sys.error("Can't make a backgammon board from a go board")

  }

  case class Backgammon(b: backgammon.Board)
      extends Board(
        b.pieces.map { case (pos, (piece, count)) =>
          (Pos.Backgammon(pos), (Piece.Backgammon(piece), count))
        },
        History.Backgammon(b.history),
        Variant.Backgammon(b.variant)
      ) {

    def withHistory(h: History): Board = h match {
      case History.Backgammon(h) => Backgammon(b.withHistory(h))
      case _                     => sys.error("Not passed backgammon objects")
    }

    def situationOf(player: Player): Situation = Situation.Backgammon(b.situationOf(player))

    def materialImbalance: Int = b.materialImbalance

    override def toString: String = b.toString

    def copy(history: History, variant: Variant): Board = (history, variant) match {
      case (History.Backgammon(history), Variant.Backgammon(variant)) =>
        Backgammon(b.copy(history = history, variant = variant))
      case _                                                          => sys.error("Unable to copy a backgammon board with non-backgammon arguments")
    }
    def copy(history: History): Board                   = history match {
      case History.Backgammon(history) => Backgammon(b.copy(history = history))
      case _                           => sys.error("Unable to copy a backgammon board with non-backgammon arguments")
    }

    def toFairySF      = sys.error("Can't make a fairysf board from a backgammon board")
    def toChess        = sys.error("Can't make a chess board from a backgammon board")
    def toDraughts     = sys.error("Can't make a draughts board from a backgammon board")
    def toSamurai      = sys.error("Can't make a samurai board from a backgammon board")
    def toTogyzkumalak = sys.error("Can't make a togyzkumalak board from a backgammon board")
    def toGo           = sys.error("Can't make a go board from a backgammon board")
    def toBackgammon   = b

  }

  def apply(lib: GameLogic, pieces: Iterable[(Pos, (Piece, Int))], variant: Variant): Board =
    (lib, variant) match {
      case (GameLogic.Draughts(), Variant.Draughts(variant))         =>
        Draughts(
          draughts.Board.apply(
            pieces.flatMap {
              case (Pos.Draughts(pos), (Piece.Draughts(piece), _)) => Some((pos, piece))
              case _                                               => None
            },
            variant
          )
        )
      case (GameLogic.Chess(), Variant.Chess(variant))               =>
        Chess(
          chess.Board.apply(
            pieces.flatMap {
              case (Pos.Chess(pos), (Piece.Chess(piece), _)) => Some((pos, piece))
              case _                                         => None
            },
            variant
          )
        )
      case (GameLogic.FairySF(), Variant.FairySF(variant))           =>
        FairySF(
          fairysf.Board.apply(
            pieces.flatMap {
              case (Pos.FairySF(pos), (Piece.FairySF(piece), _)) => Some((pos, piece))
              case _                                             => None
            },
            variant
          )
        )
      case (GameLogic.Samurai(), Variant.Samurai(variant))           =>
        Samurai(
          samurai.Board.apply(
            pieces.flatMap {
              case (Pos.Samurai(pos), (Piece.Samurai(piece), count)) => Some((pos, (piece, count)))
              case _                                                 => None
            },
            variant
          )
        )
      case (GameLogic.Togyzkumalak(), Variant.Togyzkumalak(variant)) =>
        Togyzkumalak(
          togyzkumalak.Board.apply(
            pieces.flatMap {
              case (Pos.Togyzkumalak(pos), (Piece.Togyzkumalak(piece), count)) =>
                Some((pos, (piece, count)))
              case _                                                           => None
            },
            variant
          )
        )
      case (GameLogic.Go(), Variant.Go(variant))                     =>
        Go(
          go.Board.apply(
            pieces.flatMap {
              case (Pos.Go(pos), (Piece.Go(piece), _)) => Some((pos, piece))
              case _                                   => None
            },
            variant
          )
        )
      case (GameLogic.Backgammon(), Variant.Backgammon(variant))     =>
        Backgammon(
          backgammon.Board.apply(
            pieces.map { case (Pos.Backgammon(pos), (Piece.Backgammon(piece), count)) =>
              (pos, (piece, count))
            },
            variant
          )
        )
      case _                                                         => sys.error("Mismatched gamelogic types 27")
    }

  implicit def chessBoard(b: chess.Board)               = Board.Chess(b)
  implicit def draughtsBoard(b: draughts.Board)         = Board.Draughts(b)
  implicit def fairysfBoard(b: fairysf.Board)           = Board.FairySF(b)
  implicit def samuraiBoard(b: samurai.Board)           = Board.Samurai(b)
  implicit def togyzkumalakBoard(b: togyzkumalak.Board) = Board.Togyzkumalak(b)
  implicit def goBoard(b: go.Board)                     = Board.Go(b)
  implicit def backgammonBoard(b: backgammon.Board)     = Board.Backgammon(b)

  def init(lib: GameLogic, variant: Variant): Board = (lib, variant) match {
    case (GameLogic.Draughts(), Variant.Draughts(variant))         => Draughts(draughts.Board.init(variant))
    case (GameLogic.Chess(), Variant.Chess(variant))               => Chess(chess.Board.init(variant))
    case (GameLogic.FairySF(), Variant.FairySF(variant))           => FairySF(fairysf.Board.init(variant))
    case (GameLogic.Samurai(), Variant.Samurai(variant))           => Samurai(samurai.Board.init(variant))
    case (GameLogic.Togyzkumalak(), Variant.Togyzkumalak(variant)) =>
      Togyzkumalak(togyzkumalak.Board.init(variant))
    case (GameLogic.Go(), Variant.Go(variant))                     => Go(go.Board.init(variant))
    case (GameLogic.Backgammon(), Variant.Backgammon(variant))     => Backgammon(backgammon.Board.init(variant))
    case _                                                         => sys.error("Mismatched gamelogic types 28")
  }

}
