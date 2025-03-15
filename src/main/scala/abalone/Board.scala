//package strategygames.abalone
//
//import strategygames.Player
//import strategygames.abalone.variant.Variant
//
//case class Board(
//                  pieces: PieceMap,
//                  history: History,
//                  variant: Variant
//                ) {
//  def apply(at: Pos): Option[Piece] = pieces.get(at)
//
//  def apply(file: File, rank: Rank): Option[Piece] = {
//    Pos(file, rank) match {
//      case Some(pos) => this (pos)
//      case None => None
//    }
//  }
//
//  def piecesOf(player: Player): PieceMap = pieces.filter(_._2.is(player))
//
//  def isEmptyPos(pos: Option[Pos]): Boolean = pos.fold(false)(!pieces.contains(_))
//
//  def withHistory(h: History): Board = copy(history = h)
//
//  def updateHistory(f: History => History) = copy(history = f(history))
//
//  def withVariant(v: Variant): Board =
//    if (v.dropsVariant) copy(variant = v)
//    else copy(variant = v)
//
//  def situationOf(player: Player) = Situation(this, player)
//
//  def valid(strict: Boolean) = variant.valid(this, strict)
//
//  def materialImbalance: Int = variant.materialImbalance(this)
//
//  def autoDraw: Boolean = history.threefoldRepetition && variant.repetitionEnabled
//
//  override def toString = s"$variant Position after ${history.recentTurnUciString}"
//
//  lazy val actors: Map[Pos, Actor] = pieces.map { case (pos, piece) =>
//    (pos, Actor(piece, pos, this))
//  }
//
//  lazy val posMap: Map[Piece, Iterable[Pos]] = pieces.groupMap(_._2)(_._1)
//
//  lazy val piecesOnBoardCount: Int = pieces.keys.size
//}
//
//object Board {
//  def apply(pieces: Iterable[(Pos, Piece)], variant: Variant): Board =
//    Board(pieces.toMap, History(), variant)
//
//  def init(variant: Variant): Board = Board(variant.pieces, variant)
//
//  // def empty(variant: Variant): Board = Board(Nil, variant)
//}