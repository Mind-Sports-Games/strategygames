package strategygames.draughts
package variant

import strategygames.Player

import format.FEN

import cats.implicits._

case object Frysk
    extends Variant(
      id = 8,
      gameType = 97,
      key = "frysk",
      name = "Frysk!",
      standardInitialPosition = false,
      boardSize = Board.D100
    ) {

  def perfId: Int    = 116
  def perfIcon: Char = ''

  def pieces           = Variant.symmetricBackrank(Vector(Man, Man, Man, Man, Man), boardSize)
  def initialFen       = FEN("W:W46,47,48,49,50:B1,2,3,4,5:H0:F1")
  def startingPosition = StartingPosition("---", initialFen, "", "Initial position".some)

  def captureDirs    = Frisian.captureDirs
  def moveDirsPlayer = Frisian.moveDirsPlayer
  def moveDirsAll    = Frisian.moveDirsAll

  override def getCaptureValue(board: Board, taken: List[Pos]) = Frisian.getCaptureValue(board, taken)
  override def getCaptureValue(board: Board, taken: Pos)       = Frisian.getCaptureValue(board, taken)

  override def validMoves(situation: Situation, finalSquare: Boolean = false): Map[Pos, List[Move]] =
    Frisian.validMoves(situation, finalSquare)
  override def finalizeBoard(
      board: Board,
      uci: format.Uci.Move,
      captured: Option[List[Piece]],
      situationBefore: Situation,
      finalSquare: Boolean
  ): Board = Frisian.finalizeBoard(board, uci, captured, situationBefore, finalSquare)

  def maxDrawingMoves(board: Board): Option[Int] = Frisian.maxDrawingMoves(board)
  def updatePositionHashes(
      board: Board,
      move: Move,
      hash: strategygames.draughts.PositionHash
  ): PositionHash =
    Frisian.updatePositionHashes(board, move, hash)

  override protected def validSide(board: Board, strict: Boolean)(player: Player) = {
    val roles = board rolesOf player
    (roles.count(_ == Man) > 0 || roles.count(_ == King) > 0) &&
    (!strict || roles.size <= 5) &&
    (!menOnPromotionRank(board, player) || board.ghosts != 0)
  }

}
