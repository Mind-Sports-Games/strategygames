package strategygames.chess

import strategygames.Player

import format.Uci

import variant.Variant

case class Board(
    pieces: PieceMap,
    history: History,
    variant: Variant,
    pocketData: Option[PocketData] = None
) {

  def apply(at: Pos): Option[Piece] = pieces get at
  def apply(file: File, rank: Rank) = pieces get Pos(file, rank)

  lazy val actors: Map[Pos, Actor] = pieces map { case (pos, piece) =>
    (pos, Actor(piece, pos, this))
  }

  lazy val actorsOf: Player.Map[Seq[Actor]] = {
    val (w, b) = actors.values.toSeq.partition { _.player.p1 }
    Player.Map(w, b)
  }

  def rolesOf(c: Player): List[Role] =
    pieces.values
      .collect {
        case piece if piece.player == c => piece.role
      }
      .to(List)

  def actorAt(at: Pos): Option[Actor] = actors get at

  def piecesOf(c: Player): Map[Pos, Piece] = pieces filter (_._2 is c)

  lazy val kingPos: Map[Player, Pos] = pieces.collect { case (pos, Piece(player, King)) =>
    player -> pos
  }

  def kingPosOf(c: Player): Option[Pos] = kingPos get c

  def check(c: Player): Boolean = c.fold(checkP1, checkP2)

  lazy val checkP1 = checkOf(P1)
  lazy val checkP2 = checkOf(P2)

  private def checkOf(c: Player): Boolean =
    kingPosOf(c) exists { kingPos =>
      variant.kingThreatened(
        board = this,
        player = !c,
        to = kingPos,
        validatingCheck = true
      )
    }

  def destsFrom(from: Pos): Option[List[Pos]] =
    actorAt(from) map (_.destinations)

  def seq(actions: Board => Option[Board]*): Option[Board] =
    actions.foldLeft(Option(this): Option[Board])(_ flatMap _)

  def place(piece: Piece, at: Pos): Option[Board] =
    if (pieces contains at) None
    else Option(copy(pieces = pieces + ((at, piece))))

  def take(at: Pos): Option[Board] =
    if (pieces contains at) Option(copy(pieces = pieces - at))
    else None

  def move(orig: Pos, dest: Pos): Option[Board] =
    if (pieces contains dest) None
    else
      pieces get orig map { piece =>
        copy(pieces = pieces - orig + ((dest, piece)))
      }

  def taking(orig: Pos, dest: Pos, taking: Option[Pos] = None): Option[Board] =
    for {
      piece   <- pieces get orig
      takenPos = taking getOrElse dest
      if pieces contains takenPos
    } yield copy(pieces = pieces - takenPos - orig + (dest -> piece))

  lazy val occupation: Player.Map[Set[Pos]] = Player.Map { player =>
    pieces.collect { case (pos, piece) if piece is player => pos }.to(Set)
  }

  def hasPiece(p: Piece) = pieces.values exists (p ==)

  def promote(pos: Pos): Option[Board] =
    for {
      pawn <- apply(pos)
      if pawn is Pawn
      b2   <- take(pos)
      b3   <- b2.place(Piece(pawn.player, Queen), pos)
    } yield b3

  def castles: Castles = history.castles

  def withHistory(h: History): Board = copy(history = h)

  def withCastles(c: Castles) = withHistory(history withCastles c)

  def withPieces(newPieces: PieceMap) = copy(pieces = newPieces)

  def withVariant(v: Variant): Board =
    if (v.dropsVariant) copy(variant = v).ensureCrazyData
    else copy(variant = v)

  def withCrazyData(data: PocketData)                   = copy(pocketData = Option(data))
  def withCrazyData(data: Option[PocketData])           = copy(pocketData = data)
  def withCrazyData(f: PocketData => PocketData): Board =
    withCrazyData(f(pocketData | PocketData.init))

  def ensureCrazyData = withCrazyData(pocketData | PocketData.init)

  def unmovedRooks =
    UnmovedRooks {
      history.unmovedRooks.pos.filter(pos =>
        apply(pos).exists(piece => piece.is(Rook) && Rank.backRank(piece.player) == pos.rank)
      )
    }

  def fixCastles: Board =
    withCastles {
      if (variant.allowsCastling) {
        val wkPos                                                       = kingPosOf(P1)
        val bkPos                                                       = kingPosOf(P2)
        val wkReady                                                     = wkPos.fold(false)(_.rank == Rank.First)
        val bkReady                                                     = bkPos.fold(false)(_.rank == Rank.Eighth)
        def rookReady(player: Player, kPos: Option[Pos], left: Boolean) =
          kPos.fold(false) { kp =>
            actorsOf(player) exists { a =>
              a.piece.is(
                Rook
              ) && a.pos ?- kp && (left ^ (a.pos ?> kp)) && history.unmovedRooks
                .pos(
                  a.pos
                )
            }
          }
        Castles(
          p1KingSide = castles.p1KingSide && wkReady && rookReady(P1, wkPos, left = false),
          p1QueenSide = castles.p1QueenSide && wkReady && rookReady(P1, wkPos, left = true),
          p2KingSide = castles.p2KingSide && bkReady && rookReady(P2, bkPos, left = false),
          p2QueenSide = castles.p2QueenSide && bkReady && rookReady(P2, bkPos, left = true)
        )
      } else Castles.none
    }

  def updateHistory(f: History => History) = copy(history = f(history))

  def count(p: Piece): Int  = pieces.values count (_ == p)
  def count(c: Player): Int = pieces.values count (_.player == c)

  def autoDraw: Boolean =
    variant.fiftyMoves(history) || variant.isInsufficientMaterial(
      this
    ) || history.fivefoldRepetition

  def situationOf(player: Player) = Situation(this, player)

  def visual = format.Visual >> this

  def valid(strict: Boolean) = variant.valid(this, strict)

  def materialImbalance: Int = variant.materialImbalance(this)

  // used for multiaction variants like Monster Chess.
  // Won't always produce a result for Atomic - but this isnt needed for that variant
  def lastActionPlayer: Option[Player] = history.lastAction
    .flatMap {
      case m: Uci.Move => Some(m.dest)
      case d: Uci.Drop => Some(d.pos)
      // TODO: this probably needs to be fixed?
      case _           => sys.error("Dice Rolls are not supported (lastActionPlayer)")
    }
    .flatMap(apply)
    .map(_.player)

  def fileOccupation(file: File): Map[Pos, Piece] =
    pieces.filter(_._1.file == file)

  def rankOccupation(rank: Rank): Map[Pos, Piece] =
    pieces.filter(_._1.rank == rank)

  private def diagOccupation(
      p: Pos,
      dir: Direction,
      diagPieces: Map[Pos, Piece] = Map[Pos, Piece]()
  ): Map[Pos, Piece] =
    dir(p) match {
      case Some(diagPos) =>
        if (pieces.contains(diagPos))
          diagOccupation(
            diagPos,
            dir,
            diagPieces ++ Map(diagPos -> pieces(diagPos))
          )
        else
          diagOccupation(diagPos, dir, diagPieces)
      case None          => return diagPieces
    }

  def diagAscOccupation(pos: Pos): Map[Pos, Piece] =
    if (pieces.contains(pos))
      Map[Pos, Piece](pos -> pieces(pos)) ++ diagOccupation(
        pos,
        _.upRight
      ) ++ diagOccupation(pos, _.downLeft)
    else
      diagOccupation(pos, _.upRight) ++ diagOccupation(pos, _.downLeft)

  def diagDescOccupation(pos: Pos): Map[Pos, Piece] =
    if (pieces.contains(pos))
      Map[Pos, Piece](pos -> pieces(pos)) ++ diagOccupation(
        pos,
        _.upLeft
      ) ++ diagOccupation(pos, _.downRight)
    else
      diagOccupation(pos, _.upLeft) ++ diagOccupation(pos, _.downRight)

  override def toString =
    s"$variant Position after ${history.recentTurnUciString}\n$visual"
}

object Board {

  def apply(pieces: Iterable[(Pos, Piece)], variant: Variant): Board =
    Board(
      pieces.toMap,
      if (variant.allowsCastling) Castles.all else Castles.none,
      variant
    )

  def apply(
      pieces: Iterable[(Pos, Piece)],
      castles: Castles,
      variant: Variant
  ): Board =
    Board(
      pieces.toMap,
      History(castles = castles),
      variant,
      variantCrazyData(variant)
    )

  def init(variant: Variant): Board =
    Board(variant.pieces, variant.castles, variant)

  def empty(variant: Variant): Board = Board(Nil, variant)

  private def variantCrazyData(variant: Variant) =
    (variant.dropsVariant) option PocketData.init

}
