package strategygames.go

sealed abstract class Ruleset {
  def settlesOnAFourthConsecutivePass: Boolean
  def forbidsRecreatingAnEarlierPosition: Boolean
  def settlesOnRecreatingAnEarlierPosition: Boolean
}

object Ruleset {

  case object AsCurrentlyPlayed extends Ruleset {
    val settlesOnAFourthConsecutivePass      = true
    val forbidsRecreatingAnEarlierPosition   = true
    val settlesOnRecreatingAnEarlierPosition = false
  }

  private[go] case object AsOriginallyPlayed extends Ruleset {
    val settlesOnAFourthConsecutivePass      = false
    val forbidsRecreatingAnEarlierPosition   = false
    val settlesOnRecreatingAnEarlierPosition = true
  }
}
