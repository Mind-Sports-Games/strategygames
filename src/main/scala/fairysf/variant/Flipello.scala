package strategygames.fairysf
package variant

import strategygames.GameFamily

case object Flipello
    extends Variant(
      id = 3,
      key = "flipello",
      name = "Flipello",
      shortName = "Flipello",
      title = "Flipello",
      standardInitialPosition = true,
      fairysfName=FairySFName("flipello"),
      boardSize = Board.Dim8x8
    ) {

  def gameFamily: GameFamily = GameFamily.Flipello()

  def perfIcon: Char = 'l'
  def perfId: Int = 204

  override def baseVariant: Boolean = true

  override def dropsVariant = true
  override def onlyDropsVariant = true

  //cache this rather than checking with the API everytime
  override def initialFen =
    format.FEN("8/8/8/3pP3/3Pp3/8/8/8[PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPpppppppppppppppppppppppppppppppp] w 0 1")

}
