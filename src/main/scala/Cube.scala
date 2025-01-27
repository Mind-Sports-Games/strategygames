package strategygames

import strategygames.{ GameLogic, Player }

sealed abstract class CubeData(
    val gameLogic: GameLogic,
    val value: Int,
    val owner: Option[Player],
    val underOffer: Boolean,
    val rejected: Boolean
)

object CubeData {

  case class Backgammon(c: backgammon.CubeData)
      extends CubeData(
        GameLogic.Backgammon(),
        c.value,
        c.owner,
        c.underOffer,
        c.rejected
      )

  def init(lib: GameLogic): CubeData = lib match {
    case GameLogic.Backgammon() => Backgammon(backgammon.CubeData.init)
    case _                      => sys.error("Unable to initialise cube data for non backgammon lib")
  }

}
