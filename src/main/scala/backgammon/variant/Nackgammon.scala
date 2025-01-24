package strategygames.backgammon
package variant

import strategygames.backgammon._
import strategygames.GameFamily

case object Nackgammon
    extends Variant(
      id = 2,
      key = "nackgammon",
      name = "Nackgammon",
      standardInitialPosition = false,
      boardSize = Board.Dim12x2
    ) {

  def gameFamily: GameFamily = GameFamily.Backgammon()

  def perfIcon: Char = ''
  def perfId: Int    = 601

  override def baseVariant: Boolean = false

  override def initialFen =
    format.FEN("4S,3,3s,1,4s,3,2S,2S/4s,3,3S,1,4S,3,2s,2s[] - - w 0 0 - 1")

  override def initialFens = List(
    format.FEN("4S,3,3s,1,4s,3,2S,2S/4s,3,3S,1,4S,3,2s,2s[] - - w 0 0 - 1"),
    format.FEN("4S,3,3s,1,4s,3,2S,2S/4s,3,3S,1,4S,3,2s,2s[] - - b 0 0 - 1")
  )

}
