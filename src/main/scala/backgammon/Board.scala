package strategygames.backgammon

import strategygames.Player

import format.Uci

import variant.Variant

case class Board(
    pieces: PieceMap,
    history: History,
    variant: Variant,
    pocketData: Option[PocketData] = None,
    unusedDice: List[Int] = List.empty
) {

  def apply(at: Pos): Option[Piece] = (pieces get at).map(_._1)
  def apply(file: File, rank: Rank) = pieces get Pos(file, rank)

  def piecesOf(player: Player): PieceMap = pieces filter (_._2._1 is player)

  // doubles the number of dice if a double is rolled
  private def diceToActionDice(dice: List[Int]): List[Int] =
    if (dice.distinct.size == 1) List.fill(4)(dice.distinct).flatten else dice

  def setDice(dice: List[Int]): Board =
    copy(unusedDice = diceToActionDice(dice))

  def useDie(die: Int): Board = copy(unusedDice = unusedDice.diff(List(die)))

  lazy val unusedDiceStr: String = if (unusedDice.isEmpty) "-" else unusedDice.mkString("/")

  lazy val usedDice: List[Int] = diceToActionDice(
    history.currentTurn.flatMap {
      case u: Uci.DiceRoll => Some(u.dice)
      case _               => None
    }.flatten
  ).diff(unusedDice)

  lazy val usedDiceStr: String = if (usedDice.isEmpty) "-" else usedDice.mkString("/")

  def piecesOnBar(player: Player): Boolean =
    pocketData.fold(false) { pocketData => pocketData.pockets(player).roles.nonEmpty }

  def pieceCountOnBar(player: Player): Int =
    pocketData.fold(0) { pocketData => pocketData.pockets(player).roles.size }

  // At least one piece in opponents home
  def pieceInOpponentsHome(player: Player): Boolean =
    piecesOf(player).keys.toList.intersect(Pos.home(!player)).nonEmpty

  def piecesInQuarter(player: Player, quarter: Int): Int =
    piecesOf(player)
      .filter { case (pos, _) => Pos.byQuarter(player, quarter).contains(pos) }
      .values
      .map(_._2)
      .sum

  // all pieces in home
  def piecesCanLift(player: Player): Boolean =
    !piecesOnBar(player) && piecesOf(player).keys.toList.diff(Pos.home(player)).isEmpty

  def pipCount(player: Player): Int =
    piecesOf(player).map {
      case (pos, piece) => (
        ((Pos.barIndex(!piece._1.player) * Pos.indexDirection(piece._1.player)) +
          (pos.index * Pos.indexDirection(!piece._1.player))) * piece._2
      )
    }.sum + (pieceCountOnBar(player) * 25)

  // only looks at pieces on board not pieces on bar
  def furthestFromEnd(player: Player): Int =
    if (piecesOf(player).isEmpty) 0
    else
      (Pos.barIndex(!player) +
        (piecesOf(player).keys.map(_.index * Pos.indexDirection(!player)).max *
          Pos.indexDirection(player))).abs

  private def onlySafeOnePointPieces(player: Player) =
    (furthestFromEnd(player) == 1 &&
      (playerPiecesOnBoardCount(player) - unusedDice.size).max(0) % 2 == 0) ||
      (playerPiecesOnBoardCount(player) == 1 && !history.hasRolledDiceThisTurn)

  def racePosition: Boolean =
    (!piecesOnBar(Player.P1) &&
      !piecesOnBar(Player.P2) &&
      furthestFromEnd(Player.P1) <= 24 - furthestFromEnd(Player.P2)) ||
      onlySafeOnePointPieces(Player.P1) ||
      onlySafeOnePointPieces(Player.P2)

  lazy val actors: Map[Pos, (Actor, Int)] = pieces map { case (pos, (piece, count)) =>
    (pos, (Actor(piece, pos, this), count))
  }

  lazy val posMap: Map[(Piece, Int), Iterable[Pos]] = pieces.groupMap(_._2)(_._1)

  lazy val piecesOnBoardCount: Int                    = pieces.map(_._2._2).sum
  lazy val playerPiecesOnBoardCount: Map[Player, Int] = Player.all.map { p =>
    (p, pieces.collect { case (_, (piece, count)) if piece.player == p => count }.sum)
  }.toMap

  def piecesOnBoardOrInPocket: Int = Player.all.map(playerPiecesOnBoardOrInPocket).sum

  def playerPiecesOnBoardOrInPocket(player: Player): Int =
    playerPiecesOnBoardCount(player) + pieceCountOnBar(player)

  def withHistory(h: History): Board       = copy(history = h)
  def updateHistory(f: History => History) = copy(history = f(history))

  def withVariant(v: Variant): Board =
    if (v.dropsVariant) copy(variant = v)
    else copy(variant = v)

  def withPocketData(data: PocketData)                   = copy(pocketData = Option(data))
  def withPocketData(data: Option[PocketData])           = copy(pocketData = data)
  def withPocketData(f: PocketData => PocketData): Board =
    withPocketData(f(pocketData | PocketData.init))

  def ensurePocketData = withPocketData(pocketData | PocketData.init)

  def situationOf(player: Player) = Situation(this, player)

  def valid(strict: Boolean) = variant.valid(this, strict)

  def materialImbalance: Int = variant.materialImbalance(this)

  override def toString = s"$variant Position after ${history.recentTurnUciString}"
}

object Board {

  def apply(pieces: Iterable[(Pos, (Piece, Int))], variant: Variant): Board =
    Board(pieces.toMap, History(), variant, Some(PocketData.init))

  def init(variant: Variant): Board = Board(variant.pieces, variant)

  sealed abstract class BoardSize(
      val width: Int,
      val height: Int
  ) {

    val key   = s"${width}x${height}"
    val sizes = List(width, height)

    val validPos: List[Pos] =
      Pos.all.filter(p => p.file.index < width && p.rank.index < height)

    override def toString = key

  }

  object BoardSize {
    val all: List[BoardSize] = List(Dim12x2)
  }

  case object Dim12x2
      extends BoardSize(
        width = 12,
        height = 2
      )

}
