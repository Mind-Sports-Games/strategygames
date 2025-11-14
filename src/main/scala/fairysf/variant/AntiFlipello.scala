package strategygames.fairysf
package variant

import strategygames.{ GameFamily, P1, P2, Player }

case object AntiFlipello
    extends Variant(
      id = 11,
      key = "antiflipello",
      name = "AntiFlipello",
      standardInitialPosition = true,
      fishnetKey = "ps-antiflipello",
      boardSize = Board.Dim8x8
    ) {

  def gameFamily: GameFamily = GameFamily.Flipello()

  def perfIcon: Char = ''
  def perfId: Int    = 210

  override def dropsVariant      = true
  override def onlyDropsVariant  = true
  override def hasGameScore      = true
  override def canOfferDraw      = false
  override def repetitionEnabled = false

  override def hasAnalysisBoard: Boolean = true
  override def hasFishnet: Boolean       = false

  // cache this rather than checking with the API everytime
  override def initialFen =
    format.FEN(
      "8/8/8/3pP3/3Pp3/8/8/8[PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPpppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppp] w - - 0 1"
    )

  override def specialEnd(situation: Situation) = {
    (situation.board.piecesOnBoardCount == boardSize.width * boardSize.height) ||
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
      situation.board.playerPiecesOnBoardCount.minBy(_._2)._1

}
