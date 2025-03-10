package strategygames.fairysf
package variant

import strategygames.{ GameFamily, P1, P2, Player }

case object Flipello10
    extends Variant(
      id = 7,
      key = "flipello10",
      name = "Flipello10",
      standardInitialPosition = true,
      fishnetKey = "ps-flipello10",
      boardSize = Board.Dim10x10
    ) {

  def gameFamily: GameFamily = GameFamily.Flipello()

  def perfIcon: Char = ''
  def perfId: Int    = 205

  override def dropsVariant      = true
  override def onlyDropsVariant  = true
  override def hasGameScore      = true
  override def canOfferDraw      = false
  override def repetitionEnabled = false

  // cache this rather than checking with the API everytime
  override def initialFen =
    format.FEN(
      "10/10/10/10/4pP4/4Pp4/10/10/10/10[PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPpppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppp] w - - 0 1"
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
      situation.board.playerPiecesOnBoardCount.maxBy(_._2)._1

}
