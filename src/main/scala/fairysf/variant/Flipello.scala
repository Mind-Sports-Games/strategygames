package strategygames.fairysf
package variant

import strategygames.{ GameFamily, P1, P2, Player }

case object Flipello
    extends Variant(
      id = 6,
      key = "flipello",
      name = "Flipello",
      standardInitialPosition = true,
      fairysfName = FairySFName("flipello"),
      boardSize = Board.Dim8x8
    ) {

  def gameFamily: GameFamily = GameFamily.Flipello()

  def perfIcon: Char = 'l'
  def perfId: Int    = 204

  override def baseVariant: Boolean = true

  override def dropsVariant      = true
  override def onlyDropsVariant  = true
  override def hasGameScore      = true
  override def repetitionEnabled = false

  // cache this rather than checking with the API everytime
  override def initialFen =
    format.FEN(
      "8/8/8/3pP3/3Pp3/8/8/8[PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPpppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppp] w 0 1"
    )

  override def specialEnd(situation: Situation) =
    (situation.board.piecesOnBoardCount == boardSize.width * boardSize.height) ||
      (situation.board.apiPosition.legalMoves.size == 0)

  override def specialDraw(situation: Situation) =
    situation.board.playerPiecesOnBoardCount(P1) == situation.board.playerPiecesOnBoardCount(P2)

  override def winner(situation: Situation): Option[Player] =
    (specialEnd(situation) && !specialDraw(situation)) option
      situation.board.playerPiecesOnBoardCount.maxBy(_._2)._1

}
