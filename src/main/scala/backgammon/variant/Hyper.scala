package strategygames.backgammon
package variant

import strategygames.backgammon._
import strategygames.{ GameFamily, Player }

case object Hyper
    extends Variant(
      id = 4,
      key = "hyper",
      name = "Hyper",
      standardInitialPosition = false,
      boardSize = Board.Dim12x2
    ) {

  def gameFamily: GameFamily = GameFamily.Backgammon()

  def perfIcon: Char = ''
  def perfId: Int    = 602

  override def numStartingPiecesPerPlayer: Int = 3

  // gammon and backgammon only count when the cube has been doubled in this variant
  override def gammonWin(situation: Situation) =
    if (situation.board.cubeData.map(_.value).getOrElse(1) == 1) false
    else super.gammonWin(situation)

  override def gammonPosition(situation: Situation, player: Player) =
    if (situation.board.cubeData.map(_.value).getOrElse(1) == 1) false
    else super.gammonPosition(situation, player)

  override def backgammonWin(situation: Situation) =
    if (situation.board.cubeData.map(_.value).getOrElse(1) == 1) false
    else super.backgammonWin(situation)

  override def backgammonPosition(situation: Situation, player: Player) =
    if (situation.board.cubeData.map(_.value).getOrElse(1) == 1) false
    else super.backgammonPosition(situation, player)

  override def baseVariant: Boolean = false

  override def initialFen =
    format.FEN("9,1S,1S,1S/9,1s,1s,1s[] - - w 0 0 - 1")

  override def initialFens = List(
    format.FEN("9,1S,1S,1S/9,1s,1s,1s[] - - w 0 0 - 1"),
    format.FEN("9,1S,1S,1S/9,1s,1s,1s[] - - b 0 0 - 1")
  )

}
