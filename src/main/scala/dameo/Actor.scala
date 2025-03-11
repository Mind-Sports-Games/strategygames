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

  private def noncaptureMoves(): List[Move] = List()

  @nowarn private def captureMoves(finalSquare: Boolean): List[Move] = List()

  def player: Player         = piece.player
  def is(c: Player): Boolean = c == piece.player
  def is(p: Piece): Boolean  = p == piece

  def onLongDiagonal: Boolean = false

  @nowarn private def shortRangeMoves(dirs: Directions, checkPromotion: Boolean): List[Move] = List()

  @nowarn private def longRangeMoves(dirs: Directions): List[Move] = List()

  def move(
      dest: Pos,
      after: Board,
      /* Set this to upgrade to multiaction */
      autoEndTurn: Boolean,
      /* Single capture or none */
      capture: Option[Pos],
      promotion: Option[PromotableRole] = None
  ) = Move(
    piece = piece,
    orig = pos,
    dest = dest,
    situationBefore = Situation(board, piece.player),
    after = after,
    autoEndTurn = autoEndTurn,
    capture = capture,
    promotion = promotion
  )

}
