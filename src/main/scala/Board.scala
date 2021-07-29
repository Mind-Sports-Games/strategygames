package chess

import variant.{ Crazyhouse, Variant, ChessVariant }

sealed class Board(pieces: PieceMap, history: History, variant: Variant){

  def rolesOf(c: Color): List[Role] =
    pieces.values
      .collect {
        case piece if piece.color == c => piece.role
      }
      .to(List)

  lazy val occupation: Color.Map[Set[Pos]] = Color.Map { color =>
    pieces.collect { case (pos, piece) if piece is color => pos }.to(Set)
  }

  def hasPiece(p: Piece) = pieces.values exists (p ==)

  def count(p: Piece): Int = pieces.values count (_ == p)
  def count(c: Color): Int = pieces.values count (_.color == c)

}

case class ChessBoard(
    pieces: ChessPieceMap,
    history: History,
    variant: ChessVariant,
    crazyData: Option[Crazyhouse.Data] = None
) extends Board(pieces, history, variant) {

  def apply(at: Pos): Option[ChessPiece] = pieces get at

  def apply(file: File, rank: Rank) = pieces get Pos(file, rank)

  def boardSize = variant.boardSize

  lazy val actors: Map[Pos, ChessActor] = pieces map { case (pos, piece) =>
    (pos, ChessActor(piece, pos, this))
  }

  lazy val actorsOf: Color.Map[Seq[ChessActor]] = {
    val (w, b) = actors.values.toSeq.partition { _.color.white }
    Color.Map(w, b)
  }

  //def rolesOf(c: Color): List[ChessRole] = Board.rolesOf(c)

  def actorAt(at: Pos): Option[ChessActor] = actors get at

  def piecesOf(c: Color): Map[Pos, Piece] = pieces filter (_._2 is c)

  lazy val kingPos: Map[Color, Pos] = pieces.collect { case (pos, ChessPiece(color, King)) =>
    color -> pos
  }

  def kingPosOf(c: Color): Option[Pos] = kingPos get c

  def check(c: Color): Boolean = c.fold(checkWhite, checkBlack)

  lazy val checkWhite = checkOf(White)
  lazy val checkBlack = checkOf(Black)

  private def checkOf(c: Color): Boolean =
    kingPosOf(c) exists { kingPos =>
      variant.kingThreatened(this, !c, kingPos)
    }

  def destsFrom(from: Pos): Option[List[Pos]] = actorAt(from) map (_.destinations)

  def seq(actions: ChessBoard => Option[ChessBoard]*): Option[ChessBoard] =
    actions.foldLeft(Option(this): Option[ChessBoard])(_ flatMap _)

  def place(piece: ChessPiece, at: Pos): Option[ChessBoard] =
    if (pieces contains at) None
    else Option(copy(pieces = pieces + ((at, piece))))

  def take(at: Pos): Option[ChessBoard] =
    if (pieces contains at) Option(copy(pieces = pieces - at))
    else None

  def move(orig: Pos, dest: Pos): Option[ChessBoard] =
    if (pieces contains dest) None
    else
      pieces get orig map { piece =>
        copy(pieces = pieces - orig + ((dest, piece)))
      }


  def taking(orig: Pos, dest: Pos, taking: Option[Pos] = None): Option[ChessBoard] =
    for {
      piece <- pieces get orig
      takenPos = taking getOrElse dest
      if pieces contains takenPos
    } yield copy(pieces = pieces - takenPos - orig + (dest -> piece))

  def promote(pos: Pos): Option[ChessBoard] =
    for {
      pawn <- apply(pos)
      if pawn is Pawn
      b2 <- take(pos)
      b3 <- b2.place(pawn.color.queen, pos)
    } yield b3

  def castles: Castles = history.castles

  def withHistory(h: History): ChessBoard = copy(history = h)

  def withPieces(newPieces: ChessPieceMap) = copy(pieces = newPieces)

  def withCastles(c: Castles) = withHistory(history withCastles c)

  def withVariant(v: ChessVariant): ChessBoard = {
    if (v == Crazyhouse)
      copy(variant = v).ensureCrazyData
    else
      copy(variant = v)
  }

  def withCrazyData(data: Crazyhouse.Data)         = copy(crazyData = Option(data))
  def withCrazyData(data: Option[Crazyhouse.Data]) = copy(crazyData = data)
  def withCrazyData(f: Crazyhouse.Data => Crazyhouse.Data): ChessBoard =
    withCrazyData(f(crazyData | Crazyhouse.Data.init))

  def ensureCrazyData = withCrazyData(crazyData | Crazyhouse.Data.init)

  def unmovedRooks =
    UnmovedRooks {
      history.unmovedRooks.pos.filter(pos =>
        apply(pos).exists(piece => piece.is(Rook) && piece.color.backRank == pos.rank)
      )
    }

  def fixCastles: ChessBoard =
    withCastles {
      if (variant.allowsCastling) {
        val wkPos   = kingPosOf(White)
        val bkPos   = kingPosOf(Black)
        val wkReady = wkPos.fold(false)(_.rank == Rank.First)
        val bkReady = bkPos.fold(false)(_.rank == Rank.Eighth)
        def rookReady(color: Color, kPos: Option[Pos], left: Boolean) =
          kPos.fold(false) { kp =>
            actorsOf(color) exists { a =>
              a.piece.is(Rook) && a.pos ?- kp && (left ^ (a.pos ?> kp)) && history.unmovedRooks.pos(
                a.pos
              )
            }
          }
        Castles(
          whiteKingSide = castles.whiteKingSide && wkReady && rookReady(White, wkPos, left = false),
          whiteQueenSide = castles.whiteQueenSide && wkReady && rookReady(White, wkPos, left = true),
          blackKingSide = castles.blackKingSide && bkReady && rookReady(Black, bkPos, left = false),
          blackQueenSide = castles.blackQueenSide && bkReady && rookReady(Black, bkPos, left = true)
        )
      } else Castles.none
    }

  def updateHistory(f: History => History) = copy(history = f(history))

  def autoDraw: Boolean =
    variant.fiftyMoves(history) || variant.isInsufficientMaterial(this) || history.fivefoldRepetition

  def situationOf(color: Color) = ChessSituation(this, color)

  def valid(strict: Boolean) = variant.valid(this, strict)

  def visual = format.Visual >> this

  def materialImbalance: Int = variant.materialImbalance(this)

  def fileOccupation(file: File): Map[Pos, ChessPiece] = pieces.filter(_._1.file == file)

  def rankOccupation(rank: Rank): Map[Pos, ChessPiece] = pieces.filter(_._1.rank == rank)

  private def diagOccupation(p: Pos, dir: Direction, diagPieces: Map[Pos, ChessPiece] = Map[Pos, ChessPiece]()): Map[Pos, ChessPiece] =
    dir(p) match {
      case Some(diagPos) =>
        if (pieces.contains(diagPos))
          diagOccupation(diagPos, dir, diagPieces ++ Map(diagPos -> pieces(diagPos)))
        else
          diagOccupation(diagPos, dir, diagPieces)
      case None => return diagPieces
    }

  def diagAscOccupation(pos: Pos): Map[Pos, ChessPiece] =
   if (pieces.contains(pos))
      Map[Pos, ChessPiece](pos -> pieces(pos)) ++ diagOccupation(pos, _.upRight) ++ diagOccupation(pos, _.downLeft)
    else
      diagOccupation(pos, _.upRight) ++ diagOccupation(pos, _.downLeft)

  def diagDescOccupation(pos: Pos): Map[Pos, ChessPiece] =
   if (pieces.contains(pos))
      Map[Pos, ChessPiece](pos -> pieces(pos)) ++ diagOccupation(pos, _.upLeft) ++ diagOccupation(pos, _.downRight)
    else
      diagOccupation(pos, _.upLeft) ++ diagOccupation(pos, _.downRight)

  override def toString = s"$variant Position after ${history.lastMove}\n$visual"
}

object ChessBoard {

  def apply(pieces: Iterable[(Pos, ChessPiece)], variant: ChessVariant): ChessBoard =
    ChessBoard(pieces.toMap, if (variant.allowsCastling) Castles.all else Castles.none, variant)

  def apply(pieces: Iterable[(Pos, ChessPiece)], castles: Castles, variant: ChessVariant): ChessBoard =
    ChessBoard(pieces.toMap, History(castles = castles), variant, variantCrazyData(variant))

  def init(variant: ChessVariant): ChessBoard = ChessBoard(variant.pieces, variant.castles, variant)

  def empty(variant: ChessVariant): ChessBoard = ChessBoard(Nil, variant)

  private def variantCrazyData(variant: ChessVariant) =
    (variant == Crazyhouse) option Crazyhouse.Data.init

  sealed abstract class BoardSize(    
      //val pos: BoardPos,    
      val width: Int,    
      val height: Int    
  ) {    
    val key = (width * height).toString    
    val sizes = List(width, height)    
    
    val fields = (width * height) / 2    
    val promotableYWhite = 1    
    val promotableYBlack = height    
  }    
  object BoardSize {    
    val all: List[BoardSize] = List(D100, D64)    
    //val max = D100.pos    
  }    
    
  case object D100    
    extends BoardSize(    
      //pos = Pos100,    
      width = 10,    
      height = 10    
    )    
  case object D64    
    extends BoardSize(    
      //pos = Pos64,    
      width = 8,    
      height = 8    
    )
}

case class DraughtsBoard(
    pieces: PieceMap,
    history: History,
    variant: Variant
) extends Board(pieces, history, variant) {}

object DraughtsBoard{}
