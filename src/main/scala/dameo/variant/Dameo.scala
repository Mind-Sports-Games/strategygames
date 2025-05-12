package strategygames.dameo
package variant

import strategygames.dameo._
import strategygames.dameo.format.FEN
import strategygames.GameFamily

case object Dameo
    extends Variant(
      id = 1,
      key = "dameo",
      name = "Dameo",
      standardInitialPosition = true,
      boardSize = Board.Dim8x8
    ) {

  def gameFamily: GameFamily = GameFamily.Dameo()

  def perfIcon: Char = ''
  def perfId: Int    = 800

  override def baseVariant: Boolean = true

  override def initialFen: FEN = FEN(
    "W:Wa1,b1,b2,c1,c2,c3,d1,d2,d3,e1,e2,e3,f1,f2,f3,g1,g2,h1:Ba8,b7,b8,c6,c7,c8,d6,d7,d8,e6,e7,e8,f6,f7,f8,g7,g8,h8:H0:F1"
  )
}
