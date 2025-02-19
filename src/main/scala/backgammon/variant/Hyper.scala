package strategygames.backgammon
package variant

import strategygames.backgammon._
import strategygames.{ GameFamily, Player, Status }

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

  // gammon and backgammon only count when the cube has been doubled in Hyper (Jacoby Rule)
  override def gammonWin(situation: Situation) =
    if (situation.board.hasDoubledCube) super.gammonWin(situation)
    else false

  override def gammonPosition(situation: Situation, player: Player) =
    if (situation.board.hasDoubledCube) super.gammonPosition(situation, player)
    else false

  override def backgammonWin(situation: Situation) =
    if (situation.board.hasDoubledCube) super.backgammonWin(situation)
    else false

  override def backgammonPosition(situation: Situation, player: Player) =
    if (situation.board.hasDoubledCube) super.backgammonPosition(situation, player)
    else false

  override def resignStatus(situation: Situation, player: Player): Status.type => Status =
    if (situation.board.racePosition)
      if (backgammonPosition(situation, player)) _.ResignBackgammon
      else if (gammonPosition(situation, player)) _.ResignGammon
      else _.Resign
    else if (situation.board.history.score(player) == 0 && situation.board.hasDoubledCube) _.ResignBackgammon
    else _.Resign

  override def outOfTimeStatus(situation: Situation): Status.type => Status =
    if (situation.board.racePosition)
      if (backgammonPosition(situation, situation.player)) _.OutoftimeBackgammon
      else if (gammonPosition(situation, situation.player)) _.OutoftimeGammon
      else _.Outoftime
    else if (situation.board.history.score(situation.player) == 0 && situation.board.hasDoubledCube)
      _.OutoftimeBackgammon
    else _.Outoftime

  override def insufficientMaterialStatus(situation: Situation): Status.type => Status =
    if (!situation.board.hasDoubledCube) _.RuleOfGin
    else if (situation.opponentHasInsufficientMaterialForBackgammon) _.GinBackgammon
    else if (situation.opponentHasInsufficientMaterialForGammon) _.GinGammon
    else _.RuleOfGin

  override def baseVariant: Boolean = false

  override def initialFen =
    format.FEN("9,1S,1S,1S/9,1s,1s,1s[] - - w 0 0 - 1")

  override def initialFens = List(
    format.FEN("9,1S,1S,1S/9,1s,1s,1s[] - - w 0 0 - 1"),
    format.FEN("9,1S,1S,1S/9,1s,1s,1s[] - - b 0 0 - 1")
  )

}
