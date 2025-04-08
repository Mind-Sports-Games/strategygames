package strategygames.dameo

import strategygames.Player

import scala.annotation.nowarn

// TODO Dameo - move generation probably wants to go in here
// You can change the functions here to whatever works best
// but I've stubbed out what the draughts gamelogic has
final case class Actor(
    piece: Piece,
    pos: Pos,
    board: Board
) {

  lazy val noncaptures: List[Move]   = noncaptureMoves()
  lazy val captures: List[Move]      = captureMoves(false)
  lazy val capturesFinal: List[Move] = captureMoves(true)

  def getCaptures(finalSquare: Boolean) = if (finalSquare) capturesFinal else captures

  private def noncaptureMoves(): List[Move] = {
    def dy: Int = if (player == P1) 1 else -1
    def dxs: List[Int] = List(-1, 0, 1)

    def posits: List[Pos] = piece.role match {
      case Man => dxs.flatMap(dx => linearStep(Some(pos), dx, dy))
        .filter(board.withinBounds).filter(board.empty)
      case King => dxs.flatMap(
        dx => dxs.flatMap(
          dy => if (dx == 0 && dy == 0) None else
          LazyList.iterate(Option(pos))(_.flatMap(_.step(dx, dy))).drop(1)
          .takeWhile(p => !p.isEmpty && board.withinBounds(p.get) && board.empty(p.get))
        )
      ).flatten
      case _ => List()
    }

    posits.flatMap(dest => Some(
      Move(
        piece=piece,
        orig=pos,
        dest=dest,
        situationBefore=Situation(board, piece.player),
        after=board.move(pos, dest).get,
        autoEndTurn=true,
      ))
    )
  }

  private def captureMoves(@nowarn finalSquare: Boolean): List[Move] = {

    def capAndDest: List[(Pos, Pos)] = piece.role match {
      case Man => {
        def chains: List[List[(Pos, Pos)]] = allCaptureChains()
        chains match {
          case List() => List()
          case chains => {
            def byLength: Map[Int, List[List[(Pos, Pos)]]] = chains.groupBy(_.length)
            def maxLen: Int = byLength.keys.max
            byLength(maxLen).map(_(0))
          }
        }
      }
      case _ => List()
    }

    capAndDest.map({case (cap, dest) =>
      Move(
        piece=piece,
        orig=pos,
        dest=dest,
        situationBefore=Situation(board, piece.player),
        after=board.move(pos, dest).get,
        autoEndTurn=true, //TODO wat do?
        capture=Some(cap)
      )}
    )
  }

  def linearStep(stepPos: Option[Pos], dx: Int, dy: Int): Option[Pos] = {
    /* Step through a line of own pieces in this direction */
    def nextPos: Option[Pos] = stepPos.flatMap(_.step(dx, dy))
    if (nextPos.flatMap(board.pieces.get(_)) == Some(piece)) {
      linearStep(nextPos, dx, dy)
    } else {
      nextPos
    }
  }

  def allCaptureChains(curPos: Pos = pos, curBoard: Board = board,
    thisChain: List[(Pos, Pos)] = List()): List[List[(Pos, Pos)]] = {
    def nextSteps: List[(Pos, Pos)] = allCaptureSteps(curBoard, curPos)
    if (nextSteps.isEmpty) {
      return if (thisChain.isEmpty) List() else List(thisChain)
    } else {
      return nextSteps.map({case (cap, dest) => allCaptureChains(dest,
        Board(
          curBoard.pieces - curPos - cap + (cap -> Piece(!player, GhostMan)),
          board.history, board.variant),
        thisChain :+ (cap, dest))}).flatten
    }
  }

  def allCaptureSteps(curBoard: Board, curPos: Pos): List[(Pos, Pos)] = {
    def dxys: List[(Int, Int)] = List((-1, 0), (1, 0), (0, -1), (0, 1))
    dxys.flatMap({case (dx, dy) => captureStep(curBoard, Some(curPos), dx, dy)})
  }

  def captureStep(curBoard: Board, stepPos: Option[Pos], dx: Int, dy: Int): Option[(Pos, Pos)] = {
    def capPos: Option[Pos] = stepPos.flatMap(_.step(dx, dy))
      .filter(curBoard.pieces.get(_).map(_.player) == Some(!player))
      .filter(curBoard.pieces.get(_).map(_.isGhost) != Some(true))
    def destPos: Option[Pos] = capPos.flatMap(_.step(dx, dy))
      .filter(curBoard.empty(_)).filter(curBoard.withinBounds(_))
    for (a <- capPos; b <- destPos) yield (a, b)
  }

  def player: Player         = piece.player
  def is(c: Player): Boolean = c == piece.player
  def is(p: Piece): Boolean  = p == piece

  def onLongDiagonal: Boolean = false

  @nowarn private def shortRangeMoves(dirs: Directions, checkPromotion: Boolean): List[Move] = List()

  @nowarn private def longRangeMoves(dirs: Directions): List[Move] = List()

}
