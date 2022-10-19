package strategygames.draughts
package variant

import strategygames.Player

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import cats.implicits._

case object Pool
    extends Variant(
      id = 13,
      gameType = 27,
      key = "pool",
      name = "Pool",
      standardInitialPosition = false,
      boardSize = Board.D64
    ) {

  def perfId: Int    = 124
  def perfIcon: Char = ''

  override def fenVariant    = true
  override def aiVariant     = false
  def pieces                 = Russian.pieces
  def initialFen             = Russian.initialFen
  def startingPosition       = Russian.startingPosition
  // TODO: Set this
  override val openingTables = List(OpeningTable.tableFMJDBrazilian, OpeningTable.tableIDFBasic)

  def captureDirs    = Standard.captureDirs
  def moveDirsPlayer = Standard.moveDirsPlayer
  def moveDirsAll    = Standard.moveDirsAll

  override def validMoves(situation: Situation, finalSquare: Boolean = false): Map[Pos, List[Move]] =
    Russian.validMoves(situation, finalSquare)

  override def finalizeBoard(
      board: Board,
      uci: format.Uci.Move,
      captured: Option[List[Piece]],
      situationBefore: Situation,
      finalSquare: Boolean
  ): Board = {
    val promoted =
      situationBefore.board.actorAt(uci.orig) zip
        board.actorAt(uci.dest) map { case (before, after) =>
          before.piece != after.piece
        } getOrElse false
    Russian.finalizeBoardWithRemainingCaptures(
      board = board,
      remainingCaptures = if (promoted) 0 else board.actorAt(uci.dest).map(_.captureLength).getOrElse(0)
    )
  }

  override def shortRangeCaptures(actor: Actor, finalSquare: Boolean): List[Move] = {
    val buf    = new ArrayBuffer[Move]
    val player = actor.player

    def walkCaptures(
        walkDir: Direction,
        curBoard: Board,
        curPos: PosMotion,
        firstSquare: Option[PosMotion],
        firstBoard: Option[Board],
        allSquares: List[PosMotion],
        allTaken: List[PosMotion]
    ): Int =
      walkDir._2(curPos).fold(0) { nextPos =>
        curBoard(nextPos) match {
          case Some(captPiece) if captPiece.isNot(player) && !captPiece.isGhost =>
            walkDir._2(nextPos) match {
              case Some(landingPos) if curBoard(landingPos).isEmpty =>
                val boardAfter    = curBoard.takingUnsafe(curPos, landingPos, actor.piece, nextPos, captPiece)
                val newSquares    = landingPos :: allSquares
                val newTaken      = nextPos :: allTaken
                val opposite      = Variant.oppositeDirs(walkDir._1)
                val newDest       = if (firstSquare.isDefined) firstSquare else landingPos.some
                val newBoard      = if (firstBoard.isDefined) firstBoard else boardAfter.some
                val extraCaptures = captureDirs.foldLeft(0) { case (total, captDir) =>
                  if (captDir._1 == opposite) total
                  else
                    total + walkCaptures(
                      captDir,
                      boardAfter,
                      landingPos,
                      newDest,
                      newBoard,
                      newSquares,
                      newTaken
                    )
                }
                if (extraCaptures == 0) {
                  val boardAfterPromote =
                    if (promotablePos(landingPos, player))
                      boardAfter.promote(landingPos).getOrElse(boardAfter)
                    else boardAfter
                  val newMove           =
                    if (finalSquare)
                      actor.move(landingPos, boardAfterPromote.withoutGhosts, newSquares, newTaken)
                    else
                      actor.move(
                        firstSquare.getOrElse(landingPos),
                        firstBoard.getOrElse(boardAfterPromote),
                        newSquares,
                        newTaken
                      )
                  buf += newMove
                }
                extraCaptures + 1
              case _                                                => 0
            }
          case _                                                                => 0
        }
      }

    captureDirs.foreach {
      walkCaptures(_, actor.board, actor.pos, None, None, Nil, Nil)
    }
    buf.toList
  }

  override def longRangeCaptures(actor: Actor, finalSquare: Boolean): List[Move] = {
    val buf = new ArrayBuffer[Move]
    captureDirs.foreach {
      innerLongRangeCaptures(buf, actor, actor.board, actor.pos, _, finalSquare, None, None, Nil, Nil)
    }
    buf.toList
  }

  private def innerLongRangeCaptures(
      buf: ArrayBuffer[Move],
      actor: Actor,
      initBoard: Board,
      initPos: PosMotion,
      initDir: Direction,
      finalSquare: Boolean,
      initFirstSquare: Option[PosMotion],
      initFirstBoard: Option[Board],
      initAllSquares: List[PosMotion],
      initAllTaken: List[PosMotion]
  ): Int = {

    @tailrec
    def walkUntilCapture(
        walkDir: Direction,
        curBoard: Board,
        curPos: PosMotion,
        firstSquare: Option[PosMotion],
        firstBoard: Option[Board],
        allSquares: List[Pos],
        allTaken: List[Pos]
    ): Int =
      walkDir._2(curPos) match {
        case Some(nextPos) =>
          curBoard(nextPos) match {
            case None                                                                   =>
              walkUntilCapture(
                walkDir,
                curBoard.moveUnsafe(curPos, nextPos, actor.piece),
                nextPos,
                firstSquare,
                firstBoard,
                allSquares,
                allTaken
              )
            case Some(captPiece) if captPiece.isNot(actor.player) && !captPiece.isGhost =>
              walkDir._2(nextPos) match {
                case Some(landingPos) if curBoard(landingPos).isEmpty =>
                  val boardAfter = curBoard.takingUnsafe(curPos, landingPos, actor.piece, nextPos, captPiece)
                  walkAfterCapture(
                    walkDir,
                    boardAfter,
                    landingPos,
                    firstSquare,
                    firstBoard,
                    allSquares,
                    nextPos :: allTaken,
                    true,
                    0
                  )
                case _                                                => 0
              }
            case _                                                                      => 0
          }
        case _             => 0
      }

    def walkAfterCapture(
        walkDir: Direction,
        curBoard: Board,
        curPos: PosMotion,
        firstSquare: Option[PosMotion],
        firstBoard: Option[Board],
        allSquares: List[Pos],
        newTaken: List[Pos],
        justTaken: Boolean,
        currentCaptures: Int
    ): Int = {
      val newSquares        = curPos :: allSquares
      val opposite          = Variant.oppositeDirs(walkDir._1)
      val newDest           = if (firstSquare.isDefined) firstSquare else curPos.some
      val newBoard          = if (firstBoard.isDefined) firstBoard else curBoard.some
      val extraCaptures     = captureDirs.foldLeft(0) { case (total, captDir) =>
        if (captDir._1 == opposite) total
        else total + walkUntilCapture(captDir, curBoard, curPos, newDest, newBoard, newSquares, newTaken)
      }
      val moreExtraCaptures = walkDir._2(curPos) match {
        case Some(nextPos) if curBoard(nextPos).isEmpty =>
          walkAfterCapture(
            walkDir,
            curBoard.moveUnsafe(curPos, nextPos, actor.piece),
            nextPos,
            firstSquare,
            firstBoard,
            allSquares,
            newTaken,
            false,
            currentCaptures + extraCaptures
          )
        case _                                          => 0
      }
      val totalCaptures     = currentCaptures + extraCaptures + moreExtraCaptures
      if (totalCaptures == 0) {
        if (finalSquare)
          buf += actor.move(curPos, curBoard.withoutGhosts, newSquares, newTaken)
        else
          buf += actor.move(
            firstSquare.getOrElse(curPos),
            firstBoard.getOrElse(curBoard),
            newSquares,
            newTaken
          )
      }
      if (justTaken) totalCaptures + 1
      else totalCaptures
    }

    walkUntilCapture(
      initDir,
      initBoard,
      initPos,
      initFirstSquare,
      initFirstBoard,
      initAllSquares,
      initAllTaken
    )
  }

  def maxDrawingMoves(board: Board): Option[Int] =
    Russian.maxDrawingMoves(board)

  def updatePositionHashes(board: Board, move: Move, hash: PositionHash): PositionHash =
    Russian.updatePositionHashes(board, move, hash)

  override def validSide(board: Board, strict: Boolean)(player: Player) = {
    val roles = board rolesOf player
    (roles.count(_ == Man) > 0 || roles.count(_ == King) > 0) &&
    (!strict || roles.size <= 12) &&
    (!menOnPromotionRank(board, player) || board.ghosts != 0)
  }
}
