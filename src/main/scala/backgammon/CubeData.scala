package strategygames.backgammon

import strategygames.Player

case class CubeData(
    value: Int,
    owner: Option[Player],
    underOffer: Boolean,
    rejected: Boolean
) {

  def canOffer(player: Player): Boolean =
    !underOffer && value < 64 && owner != Some(!player)

  def offer(player: Player): CubeData =
    if (value >= 64) sys.error("Cannot offer cube beyond 64")
    else if (value > 1 && Some(player) != owner) sys.error("Cube offer from invalid player")
    else if (value == 1) copy(owner = Some(player), underOffer = true)
    else copy(underOffer = true)

  def double(player: Player): CubeData =
    if (!underOffer) sys.error("Cube not under offer, so cannot double")
    else if (Some(player) == owner) sys.error("Cube cannot be doubled by this player")
    else copy(value * 2, Some(player), false, false)

  def reject(player: Player): CubeData =
    if (!underOffer) sys.error("Cube not under offer, so cannot reject")
    else if (Some(player) == owner) sys.error("Cube cannot be rejected by this player")
    else copy(rejected = true)

}

object CubeData {
  val init = CubeData(1, None, false, false)
}
