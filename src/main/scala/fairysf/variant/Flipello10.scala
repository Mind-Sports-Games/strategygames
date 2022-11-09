package strategygames.fairysf
package variant

import strategygames.{ GameFamily, P1, P2, Player }

case object Flipello10
    extends Variant(
      id = 7,
      key = "flipello10",
      name = "Flipello10",
      standardInitialPosition = true,
      fairysfName = FairySFName("ps-flipello10"),
      boardSize = Board.Dim10x10
    ) {

  def gameFamily: GameFamily = GameFamily.Flipello()

  def perfIcon: Char = ''
  def perfId: Int    = 205

  override def dropsVariant      = true
  override def onlyDropsVariant  = true
  override def hasGameScore      = true
  override def repetitionEnabled = false

  // cache this rather than checking with the API everytime
  override def initialFen =
    format.FEN(
      "10/10/10/10/4pP4/4Pp4/10/10/10/10[PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPpppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppp] w 0 1"
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
