package strategygames

import scalalib.extensions.*

sealed abstract class Speed(
    val id: Int,
    val key: String,
    val range: Range,
    val name: String,
    val title: String,
    val totalTimeRoughEstimation: Centis,
    val perfId: Int,
    val perfIcon: Char
) extends Ordered[Speed] {

  def compare(other: Speed) = range.min compare other.range.min
}

object Speed {

  case object UltraBullet
      extends Speed(
        0,
        "ultraBullet",
        0 to 29,
        "UltraBullet",
        "Insanely fast games: less than 30 seconds",
        Centis(25 * 100),
        0,
        '{'
      )

  case object Bullet
      extends Speed(
        1,
        "bullet",
        30 to 179,
        "Bullet",
        "Very fast games: less than 3 minutes",
        Centis(90 * 100),
        1,
        'T'
      )

  case object Blitz
      extends Speed(
        2,
        "blitz",
        180 to 479,
        "Blitz",
        "Fast games: 3 to 8 minutes",
        Centis(7 * 60 * 100),
        2,
        ')'
      )

  case object Rapid
      extends Speed(
        5,
        "rapid",
        480 to 1499,
        "Rapid",
        "Rapid games: 8 to 25 minutes",
        Centis(12 * 60 * 100),
        6,
        '#'
      )

  case object Classical
      extends Speed(
        3,
        "classical",
        1500 to 21599,
        "Classical",
        "Classical games: 25 minutes and more",
        Centis(30 * 60 * 100),
        3,
        '+'
      )

  case object Correspondence
      extends Speed(
        4,
        "correspondence",
        21600 to Int.MaxValue,
        "Correspondence",
        "Correspondence games: one or several days per move",
        Centis(60 * 60 * 100),
        4,
        ':'
      )

  val all     = List(UltraBullet, Bullet, Blitz, Rapid, Classical, Correspondence)
  val limited = List(Bullet, Blitz, Rapid, Classical)

  val byId = all map { v =>
    (v.id, v)
  } toMap

  def apply(id: Int): Option[Speed] = byId get id

  def apply(clock: ClockConfig) = byTime(clock.estimateTotalSeconds)

  def apply(clock: Option[ClockConfig]) = byTime(clock.fold(Int.MaxValue)(_.estimateTotalSeconds))

  def byTime(seconds: Int): Speed = all.find(_.range contains seconds) | Correspondence

  def exists(id: Int): Boolean = byId contains id
}
