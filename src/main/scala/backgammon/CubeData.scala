package strategygames.backgammon

import strategygames.{ MultiPointState, Player }

case class CubeData(
    value: Int,
    owner: Option[Player],
    underOffer: Boolean,
    rejected: Boolean
) {

  def canOffer(player: Player, multiPointState: Option[MultiPointState]): Boolean =
    !underOffer &&
      owner != Some(!player) &&
      value < CubeData.MAX_VALUE &&
      value < multiPointState.fold(CubeData.MAX_VALUE)(mps =>
        mps.target - player.fold(mps.p1Points, mps.p2Points)
      )

  def offer(player: Player): CubeData =
    if (value >= CubeData.MAX_VALUE) sys.error(s"Cannot offer cube beyond {CubeData.MAX_VALUE}")
    else if (value > 1 && Some(player) != owner) sys.error(s"Cube offer from invalid player: ${player}")
    else if (value == 1) copy(owner = Some(player), underOffer = true)
    else copy(underOffer = true)

  def double(player: Player): CubeData =
    if (!underOffer) sys.error("Cube not under offer, so cannot double")
    else if (Some(player) == owner) sys.error(s"Cube cannot be doubled by this player: ${player}")
    else copy(value * 2, Some(player), false, false)

  def reject(player: Player): CubeData =
    if (!underOffer) sys.error("Cube not under offer, so cannot reject")
    else if (Some(player) == owner) sys.error(s"Cube cannot be rejected by this player: ${player}")
    else copy(rejected = true)

}

object CubeData {

  val init = CubeData(1, None, false, false)

  val MAX_VALUE = 64

}
