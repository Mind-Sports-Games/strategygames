package strategygames.fairysf
package variant

import strategygames.{ GameFamily, P1, P2, Player }
import scalalib.extensions.*

case object OctagonFlipello
    extends Variant(
      id = 12,
      key = "octagonflipello",
      name = "Octagon Flipello",
      standardInitialPosition = true,
      fishnetKey = "ps-octagonflipello",
      boardSize = Board.Dim10x10
    ) {

  def gameFamily: GameFamily = GameFamily.Flipello()

  def perfIcon: Char = ''
  def perfId: Int    = 211

  override def dropsVariant      = true
  override def onlyDropsVariant  = true
  override def hasGameScore      = true
  override def canOfferDraw      = false
  override def repetitionEnabled = false

  override def hasAnalysisBoard: Boolean = true
  override def hasFishnet: Boolean       = true

  // cache this rather than checking with the API everytime
  override def initialFen =
    format.FEN(
      "10/10/10/10/4pP4/4Pp4/10/10/10/10[PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPpppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppp] w 0 1"
    )

  private val invalidSquares: List[Pos] =
    List(Pos.A1, Pos.A2, Pos.A9, Pos.A10, Pos.B1, Pos.B10, Pos.I1, Pos.I10, Pos.J1, Pos.J2, Pos.J9, Pos.J10)

  override def specialEnd(situation: Situation) = {
    (situation.board.piecesOnBoardCount == (boardSize.width * boardSize.height) - invalidSquares.size) ||
    (situation.board.apiPosition.legalMoves.size == 0) ||
    pendingDoublePass(situation)
  }

  def pendingPass(situation: Situation) =
    situation.moves.size == 1 && situation.drops.fold(0) { _.size } == 0

  def applyPass(situation: Situation) =
    situation.moves.values.flatMap(moves => moves.headOption.map(_.situationAfter)).headOption

  def pendingDoublePass(situation: Situation) =
    pendingPass(situation) && applyPass(situation).fold(false)(pendingPass(_))

  override def specialDraw(situation: Situation) =
    specialEnd(situation) &&
      (situation.board.playerPiecesOnBoardCount(P1) == situation.board.playerPiecesOnBoardCount(P2))

  override def winner(situation: Situation): Option[Player] =
    (specialEnd(situation) && !specialDraw(situation)) option
      situation.board.playerPiecesOnBoardCount.maxBy(_._2)._1

  // Couldn't restrict in fairy api so restrict moves here
  override def validDrops(situation: Situation): List[Drop] =
    super.validDrops(situation).filterNot(p => invalidSquares.contains(p.pos))

}
