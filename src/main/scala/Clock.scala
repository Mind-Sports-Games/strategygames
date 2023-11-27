package strategygames
import scala.annotation.nowarn

import java.text.DecimalFormat
import cats.syntax.option._
import scala.util.chaining._

// Abstract timer trait
trait TimerTrait {
  val remaining: Centis
}

// This modifies the amount of time remaining on the clock after the
// move time has been applied, like increment
trait ClockTimeGrace {
  def timeToAdd(remaining: Centis, timeTaken: Centis): Tuple2[ClockTimeGrace, Centis]
  def timeWillAdd(remaining: Centis, timeTaken: Centis): Centis
  def goBerserk: ClockTimeGrace
  val maxGrace: Centis
}

case class NoClockTimeGrace() extends ClockTimeGrace {
  def timeToAdd(@nowarn remaining: Centis, timeTaken: Centis): Tuple2[ClockTimeGrace, Centis] =
    (this, timeWillAdd(remaining, timeTaken))
  def timeWillAdd(@nowarn remaining: Centis, timeTaken: Centis): Centis                       = Centis(0)
  val maxGrace: Centis                                                                        = Centis(0)
  def goBerserk: ClockTimeGrace                                                               = this
}
// NOTE: if we need a list of these, we can make a ListClockTimeGrace

// Fischer increment timer with increment. Increment is always subtracted from
// the elapsed time when time is used.
// Thus, remaining time can appear to go up
case class FischerIncrementGrace(val increment: Centis) extends ClockTimeGrace {
  override def timeToAdd(remaining: Centis, timeTaken: Centis): Tuple2[ClockTimeGrace, Centis] =
    (
      this,
      timeWillAdd(remaining, timeTaken)
    ) // 0 if no time is left, else the increment

  def timeWillAdd(remaining: Centis, timeTaken: Centis): Centis =
    (remaining > Centis(0)) ?? increment

  def goBerserk: ClockTimeGrace = NoClockTimeGrace()
  val maxGrace: Centis          = increment
}

// Bronstein increment timer with a delay. The minimum between the time used
// and the delay is subracted back to the elapsed time when time is used.
// Thus, using time will never seem to make the clock gain time.
case class SimpleDelayGrace(val delay: Centis) extends ClockTimeGrace {
  override def timeToAdd(remaining: Centis, timeTaken: Centis): Tuple2[ClockTimeGrace, Centis] =
    (this, timeWillAdd(remaining, timeTaken)) // up to the delay

  // NOTE: This is organized the way it is so that if you take more time than you were allowed, you don't actually get
  //       the full grace, so that you end up with not enough time remaining, otherwise the time remaining
  //       can chain together to keep you at zero, even though you should have lost.
  def timeWillAdd(remaining: Centis, timeTaken: Centis): Centis =
    ((remaining + timeTaken.atMost(delay)) > Centis(0)) ?? timeTaken.atMost(delay) // up to the delay

  def goBerserk: ClockTimeGrace = NoClockTimeGrace()
  val maxGrace: Centis          = delay
}

// BronsteinDelay gives back up to the entire amount, but only if they didn't use
// all of it. It's similar to SimpleDelay, but US allows you to go over and eat
// into your grace. Bronstein does not.
case class BronsteinDelayGrace(val delay: Centis) extends ClockTimeGrace {
  override def timeToAdd(remaining: Centis, timeTaken: Centis): Tuple2[ClockTimeGrace, Centis] =
    (this, timeWillAdd(remaining, timeTaken)) // 0 if no time is left, else the up to the delay

  def timeWillAdd(remaining: Centis, timeTaken: Centis): Centis =
    (remaining > Centis(0)) ?? timeTaken.atMost(delay)

  def goBerserk: ClockTimeGrace = NoClockTimeGrace()
  val maxGrace: Centis          = delay
}

case class Timer(
    val baseLimit: Centis,
    val clockTimeGrace: ClockTimeGrace = NoClockTimeGrace(),
    val nextTimer: Option[Timer] = None,
    val elapsed: Centis = Centis(0)
) extends TimerTrait {

  private def applyClockGrace(timeTaken: Centis): Timer =
    // TODO: make this work like the book
    clockTimeGrace.timeToAdd(this.remaining, timeTaken).pipe { case (newClockTimeGrace, postMoveGraceTime) =>
      copy(
        elapsed = elapsed - postMoveGraceTime,
        clockTimeGrace = newClockTimeGrace
      )
    }
  private def applyTimeTaken(timeTaken: Centis): Timer =
    copy(elapsed = elapsed + timeTaken)
  private def next: Timer                              = nextTimer.getOrElse(this)
  private def nextIfDone: Timer                        =
    if (elapsed >= limit && nextTimer.isDefined) next.takeTime(-remaining) else this

  def followedBy(timer: Timer): Timer =
    nextTimer.fold(copy(nextTimer = Some(timer)))(t => copy(nextTimer = Some(t.followedBy(timer))))

  def takeTime(timeTaken: Centis)       =
    applyTimeTaken(timeTaken)
      .applyClockGrace(timeTaken)
      .nextIfDone
  def setRemaining(t: Centis): Timer    = copy(elapsed = limit - t)
  def goBerserk(penalty: Centis): Timer =
    copy(baseLimit = baseLimit - penalty, clockTimeGrace = clockTimeGrace.goBerserk)
  def outOfTime: Boolean                = remainingAll <= Centis(0)
  def giveTime(t: Centis)               = takeTime(-t)

  // The remaining is whatever they have left + whatever they'll get if they were at the end of the gaming and
  // had used the maxGrace amount of time. This is important to work together in concer with Simple Delay
  val remaining: Centis = limit - elapsed
  def limit             = baseLimit + clockTimeGrace
    .timeWillAdd((baseLimit - elapsed).atMost(Centis(0)), clockTimeGrace.maxGrace)

  val remainingAll: Centis = nextTimer.fold(remaining)(t => t.remaining + remaining)

}

object Timer {
  def usDelay(limit: Centis, delay: Centis) = Timer(
    limit,
    clockTimeGrace = SimpleDelayGrace(delay)
  )

  def bronsteinDelay(limit: Centis, delay: Centis) = Timer(
    limit,
    clockTimeGrace = BronsteinDelayGrace(delay)
  )

  def fischerIncrement(limit: Centis, increment: Centis) = Timer(
    limit,
    clockTimeGrace = FischerIncrementGrace(increment)
  )

  def noIncrement(limit: Centis) = Timer(
    limit,
    clockTimeGrace = NoClockTimeGrace()
  )

  def byoyomi(limit: Centis) = Timer(
    limit,
    clockTimeGrace = BronsteinDelayGrace(limit) // The byoyomi grace is always the same as the limit
  )
}

sealed trait ClockConfig {
  // Abstract attributes
  def estimateTotalSeconds: Int
  def estimateTotalTime: Centis
  def limit: Centis
  def limitInMinutes: Double
  def limitSeconds: Int
  // TODO: this will need to be improved when we get multi-phased clocks.
  def graceSeconds: Int
  def initTime: Centis
  def berserkPenalty: Centis
  def berserkable: Boolean
  def emergSeconds: Int
  def show: String
  def toClock: ClockBase
  // TODO: startsAtZero is an abstraction leak.
  def startsAtZero: Boolean
  val timer: Timer
}

sealed trait ClockInfoBase {
  val time: Centis
  val periods: Int
}

sealed trait PlayerTimerBase {
  // Abstract attributes
  val config: ClockConfig
  val lag: LagTracker
  val elapsed: Centis
  val berserk: Boolean
  val lastMoveTime: Centis

  def recordLag(l: Centis): PlayerTimerBase
  def takeTime(t: Centis): PlayerTimerBase
  def setRemaining(t: Centis): PlayerTimerBase
  def goBerserk: PlayerTimerBase
  def giveTime(t: Centis): PlayerTimerBase
  def remaining: Centis
  def graceSeconds: Int

  // Implemented attributes
  def limit = {
    if (berserk) config.initTime - config.berserkPenalty
    else config.initTime
  }
}

sealed trait ClockBase {
  import timestamper.toNow

  // Abstract values
  val player: Player
  // val players: Player.Map[ClockPlayer]
  val timestamper: Timestamper
  val timestamp: Option[Timestamp]
  val config: ClockConfig

  // Abstract methods
  def start: ClockBase
  def pause: ClockBase
  def stop: ClockBase
  def stop(pause: Boolean = false): ClockBase
  def hardStop: ClockBase
  def switch(switchPlayer: Boolean = true): ClockBase
  def step(
      metrics: MoveMetrics = MoveMetrics(),
      gameActive: Boolean = true,
      switchClock: Boolean = true
  ): ClockBase
  def withTimestamper(timestamper: Timestamper): ClockBase
  def outOfTime(c: Player, withGrace: Boolean): Boolean
  def giveTime(c: Player, t: Centis): ClockBase
  def goBerserk(c: Player): ClockBase
  def clockPlayer(c: Player): PlayerTimerBase
  def clockPlayerExists(f: PlayerTimerBase => Boolean): Boolean
  def allClockPlayers: Seq[PlayerTimerBase]
  def lagCompAvg: Centis
  def setRemainingTime(p: Player, t: Centis): ClockBase
  def isPaused: Boolean

  def currentClockFor(c: Player): ClockInfoBase

  // Implemented attributes
  def graceSeconds              = config.graceSeconds
  def remainingTime(c: Player)  = (clockPlayer(c).remaining - pending(c)) nonNeg
  def moretimeable(c: Player)   = clockPlayer(c).remaining.centis < 100 * 60 * 60 * 2
  def lastMoveTimeOf(c: Player) = clockPlayer(c).lastMoveTime
  def graceOf(c: Player)        = clockPlayer(c).graceSeconds

  def takeback(switchPlayer: Boolean = true) = switch(switchPlayer)

  def isRunning            = timestamp.isDefined
  def berserked(c: Player) = clockPlayer(c).berserk
  def lag(c: Player)       = clockPlayer(c).lag

  // Lowball estimate of next move's lag comp for UI butter.
  def lagCompEstimate(c: Player) = clockPlayer(c).lag.compEstimate

  @inline def timestampFor(c: Player) = if (c == player) timestamp else None
  @inline def pending(c: Player)      = timestampFor(c).fold(Centis(0))(toNow)

  def estimateTotalSeconds = config.estimateTotalSeconds
  def estimateTotalTime    = config.estimateTotalTime
  def limit                = config.limit
  def limitInMinutes       = config.limitInMinutes
  def limitSeconds         = config.limitSeconds
}

// TODO: refactor byoyomi to work like this as well.

case class ClockInfo(time: Centis) extends ClockInfoBase { val periods: Int = 0 }

case class ClockPlayer(
    timer: Timer,
    lag: LagTracker,
    config: ClockConfig,
    berserk: Boolean = false,
    lastMoveTime: Centis = Centis(0)
) extends PlayerTimerBase {
  val elapsed = timer.elapsed

  def recordLag(l: Centis) = copy(lag = lag.recordLag(l))

  def takeTime(t: Centis) = copy(timer = timer.takeTime(t))

  def setRemaining(t: Centis) = copy(timer = timer.setRemaining(t))

  // Honestly going berserk should just change your clock completely and shouldn't be
  // at this level. That's lila/lichess decision, but fine.
  // Going berserk changes your timer
  def goBerserk = copy(
    berserk = true,
    timer = timer.goBerserk(config.berserkPenalty)
  )

  def giveTime(t: Centis): ClockPlayer = takeTime(-t)

  def remaining: Centis = timer.remaining

  def graceSeconds: Int = config.graceSeconds
}

// All unspecified durations are expressed in seconds
case class Clock(
    config: ClockConfig,
    player: Player,
    players: Player.Map[ClockPlayer],
    timestamp: Option[Timestamp] = None,
    timestamper: Timestamper = RealTimestamper,
    paused: Boolean = false
) extends ClockBase {
  import timestamper.{ now, toNow }

  def clockPlayer(c: Player)                           = players(c)
  def clockPlayerExists(f: PlayerTimerBase => Boolean) = players.exists(f)
  def allClockPlayers: Seq[ClockPlayer]                = players.all
  def lagCompAvg                                       = players map { ~_.lag.compAvg } reduce (_ avg _)

  def currentClockFor(c: Player) = {
    val elapsed               = pending(c)
    val remainingAfterElapsed = players(c).remaining - elapsed
    ClockInfo(remainingAfterElapsed)
  }

  def isPaused = paused

  def outOfTime(c: Player, withGrace: Boolean) =
    if (paused) false
    else
      players(c).remaining <=
        timestampFor(c).fold(Centis(0)) { t =>
          if (withGrace) (toNow(t) - (players(c).lag.quota atMost Centis(200))) nonNeg
          else toNow(t)
        }

  def start = if (isRunning) this else copy(timestamp = Option(now), paused = false)

  def pause = stop(true)

  def stop(pause: Boolean = false) =
    if (paused) this
    else
      timestamp.fold(this) { t =>
        val curT = toNow(t)
        copy(
          players = players.update(
            player,
            _.takeTime(curT).copy(lastMoveTime = curT)
          ),
          timestamp = None,
          paused = pause
        )
      }
  def stop                         = stop()

  def hardStop = copy(timestamp = None)

  def updatePlayer(c: Player)(f: ClockPlayer => ClockPlayer) =
    copy(players = players.update(c, f))

  def goBerserk(c: Player) = updatePlayer(c) { _.goBerserk }

  def setRemainingTime(c: Player, centis: Centis) =
    updatePlayer(c) {
      _.setRemaining(centis)
    }

  def giveTime(c: Player, t: Centis) =
    updatePlayer(c) {
      _.giveTime(t)
    }

  def withTimestamper(timestamper: Timestamper) = copy(timestamper = timestamper)

  def switch(switchPlayer: Boolean = true) =
    copy(
      player = if (switchPlayer) !player else player,
      timestamp = timestamp.map(_ => now)
    )

  def step(
      metrics: MoveMetrics = MoveMetrics(),
      gameActive: Boolean = true,
      switchClock: Boolean = true
  ) =
    (timestamp match {
      case None    =>
        metrics.clientLag.fold(this) { l =>
          updatePlayer(player) { _.recordLag(l) }
        }
      case Some(t) =>
        val elapsed             = toNow(t)
        val lag                 = ~metrics.reportedLag(elapsed) nonNeg
        val competitor          = players(player)
        val (lagComp, lagTrack) = competitor.lag.onMove(lag)
        val moveTime            = (elapsed - lagComp).nonNeg
        val clockActive         = gameActive && moveTime < competitor.remaining

        val newClock = updatePlayer(player) {
          _.takeTime(moveTime)
            .copy(lag = lagTrack, lastMoveTime = moveTime)
        }

        if (clockActive) newClock else newClock.hardStop
    }).switch(switchClock)
}

object ClockPlayer {
  def withConfig(config: ClockConfig) =
    ClockPlayer(
      config.timer,
      LagTracker.init(config),
      config
    )
}

object Clock {
  private val limitFormatter = new DecimalFormat("#.##")

  // TODO: All of these configs are a bit verbose.
  // All unspecified durations are expressed in seconds
  case class Config(limitSeconds: Int, incrementSeconds: Int) extends ClockConfig {
    private lazy val clockTimeGrace =
      if (increment > Centis(0)) FischerIncrementGrace(increment) else NoClockTimeGrace()
    lazy val timer                  = Timer(limit, clockTimeGrace)

    def berserkable          = incrementSeconds == 0 || limitSeconds > 0
    def emergSeconds         = math.min(60, math.max(10, limitSeconds / 8))
    def estimateTotalSeconds = limitSeconds + 40 * incrementSeconds
    def estimateTotalTime    = Centis.ofSeconds(estimateTotalSeconds)
    def hasIncrement         = incrementSeconds > 0
    def increment            = Centis.ofSeconds(incrementSeconds)
    def graceSeconds         = incrementSeconds

    def limit          = Centis.ofSeconds(limitSeconds)
    def limitInMinutes = limitSeconds / 60d
    def toClock        = Clock(this)

    def limitString: String =
      limitSeconds match {
        case l if l % 60 == 0 => (l / 60).toString
        case 15 => "¼"
        case 30 => "½"
        case 45 => "¾"
        case 90 => "1.5"
        case _  => limitFormatter.format(limitSeconds / 60d)
      }

    // TODO: I don't know if this is correct for fischer clocks, but this certainly unifies the interface
    def startsAtZero = limitSeconds == 0

    def show = toString

    override def toString = s"$limitString+$incrementSeconds"

    def berserkPenalty =
      if (limitSeconds < 40 * incrementSeconds) Centis(0)
      else Centis(limitSeconds * (100 / 2))

    def initTime = {
      if (limitSeconds == 0) increment atLeast Centis(300)
      else limit
    }
  }

  // TODO: someone needs to review all of these various settings in here and ensure they still
  //       make sense for bronstein
  case class BronsteinConfig(limitSeconds: Int, delaySeconds: Int) extends ClockConfig {
    private lazy val clockTimeGrace =
      if (delay > Centis(0)) BronsteinDelayGrace(delay) else NoClockTimeGrace()
    lazy val timer                  = Timer(limit, clockTimeGrace)

    def berserkable          = delaySeconds == 0 || limitSeconds > 0
    def emergSeconds         = math.min(60, math.max(10, limitSeconds / 8))
    def estimateTotalSeconds = limitSeconds + 40 * delaySeconds
    def estimateTotalTime    = Centis.ofSeconds(estimateTotalSeconds)
    def delay                = Centis.ofSeconds(delaySeconds)
    def graceSeconds         = delaySeconds
    def limit                = Centis.ofSeconds(limitSeconds)
    def limitInMinutes       = limitSeconds / 60d
    def toClock              = Clock(this)

    def limitString: String =
      limitSeconds match {
        case l if l % 60 == 0 => (l / 60).toString
        case 15 => "¼"
        case 30 => "½"
        case 45 => "¾"
        case 90 => "1.5"
        case _  => limitFormatter.format(limitSeconds / 60d)
      }

    // TODO: I don't know if this is correct for fischer clocks, but this certainly unifies the interface
    def startsAtZero = limitSeconds == 0

    def show = toString

    override def toString = s"${limitString} d+${delaySeconds}"

    def berserkPenalty =
      if (limitSeconds < 40 * delaySeconds) Centis(0)
      else Centis(limitSeconds * (100 / 2))

    def initTime = {
      if (limitSeconds == 0) delay.atLeast(Centis(300))
      else limit
    }
  }

  case class SimpleDelayConfig(limitSeconds: Int, delaySeconds: Int) extends ClockConfig {
    private lazy val clockTimeGrace =
      if (delay > Centis(0)) SimpleDelayGrace(delay) else NoClockTimeGrace()
    lazy val timer                  = Timer(limit, clockTimeGrace)

    def berserkable          = delaySeconds == 0 || limitSeconds > 0
    def emergSeconds         = math.min(60, math.max(10, limitSeconds / 8))
    def estimateTotalSeconds = limitSeconds + 40 * delaySeconds
    def estimateTotalTime    = Centis.ofSeconds(estimateTotalSeconds)
    def delay                = Centis.ofSeconds(delaySeconds)
    def limit                = Centis.ofSeconds(limitSeconds)
    def limitInMinutes       = limitSeconds / 60d
    def graceSeconds         = delaySeconds
    def toClock              = Clock(this)

    def limitString: String =
      limitSeconds match {
        case l if l % 60 == 0 => (l / 60).toString
        case 15 => "¼"
        case 30 => "½"
        case 45 => "¾"
        case 90 => "1.5"
        case _  => limitFormatter.format(limitSeconds / 60d)
      }

    // TODO: I don't know if this is correct for fischer clocks, but this certainly unifies the interface
    def startsAtZero = limitSeconds == 0

    def show = toString

    override def toString = s"${limitString} d/${delaySeconds}"

    def berserkPenalty =
      if (limitSeconds < 40 * delaySeconds) Centis(graceSeconds)
      else Centis(limitSeconds * (100 / 2)) + Centis(graceSeconds)

    def initTime = {
      if (limitSeconds == 0) delay.atLeast(Centis(300))
      else limit
    }
  }

  // [TimeControl "600+2"] -> 10+2
  def readPgnConfig(str: String): Option[Config] =
    str.split('+') match {
      case Array(initStr, incStr) =>
        for {
          init <- initStr.toIntOption
          inc  <- incStr.toIntOption
        } yield Config(init, inc)
      case _                      => none
    }
  def readPdnConfig(str: String)                 = readPgnConfig(str)

  def apply(limit: Int, increment: Int): Clock = apply(Config(limit, increment))

  def apply(config: Config): Clock = {
    val player = ClockPlayer.withConfig(config)
    Clock(
      config = config,
      player = Player.P1,
      players = Player.Map(player, player),
      timestamp = None
    )
  }

  def apply(config: BronsteinConfig): Clock = {
    val player = ClockPlayer.withConfig(config)
    Clock(
      config = config,
      player = Player.P1,
      players = Player.Map(player, player),
      timestamp = None
    )
  }

  def apply(config: SimpleDelayConfig): Clock = {
    val player = ClockPlayer.withConfig(config)
    Clock(
      config = config,
      player = Player.P1,
      players = Player.Map(player, player),
      timestamp = None
    )
  }
}

case class ByoyomiClockInfo(time: Centis, periods: Int) extends ClockInfoBase

// All unspecified durations are expressed in seconds
case class ByoyomiClock(
    config: ByoyomiClock.Config,
    player: Player,
    players: Player.Map[ByoyomiClockPlayer],
    timestamp: Option[Timestamp] = None,
    timestamper: Timestamper = RealTimestamper,
    paused: Boolean = false
) extends ClockBase {
  import timestamper.{ now, toNow }

  def clockPlayer(c: Player)                           = players(c)
  def clockPlayerExists(f: PlayerTimerBase => Boolean) = players.exists(f)
  def allClockPlayers: Seq[ByoyomiClockPlayer]         = players.all
  def lagCompAvg                                       = players map { ~_.lag.compAvg } reduce (_ avg _)
  def incrementSeconds                                 = config.incrementSeconds

  private def periodsInUse(c: Player, t: Centis): Int = {
    val player          = players(c)
    val remainingAfterT = player.remaining - t
    if (isRunning && !remainingAfterT.isPositive && player.byoyomi.isPositive)
      math.min((-remainingAfterT.centis / player.byoyomi.centis) + 1, player.periodsLeft)
    else 0
  }

  def currentClockFor(c: Player) = {
    val elapsed               = pending(c)
    val remainingAfterElapsed = players(c).remaining - elapsed
    val periods               = periodsInUse(c, elapsed)
    ByoyomiClockInfo(
      (remainingAfterElapsed + players(c).byoyomi * periods) nonNeg,
      periods + players(c).spentPeriods
    )
  }

  def outOfTime(c: Player, withGrace: Boolean) =
    if (paused) false
    else {
      val player        = players(c)
      val timeUsed      = timestampFor(c).fold(Centis(0))(t =>
        if (withGrace) (toNow(t) - (players(c).lag.quota atMost Centis(200))) nonNeg
        else toNow(t)
      )
      val timeRemaining = player.remaining + player.periodsLeft * player.byoyomi

      timeRemaining <= timeUsed
    }

  def hardStop = copy(timestamp = None)

  def start = if (isRunning) this else copy(timestamp = Option(now), paused = false)

  def pause = stop(true)

  def isPaused = paused

  def stop(pause: Boolean = false) =
    if (paused) this
    else
      timestamp.fold(this) { t =>
        val curT    = toNow(t)
        val periods = periodsInUse(player, curT)
        copy(
          players = players.update(
            player,
            _.takeTime(curT)
              .giveTime(byoyomiOf(player) * periods)
              .spendPeriods(periods)
              .copy(lastMoveTime = curT)
          ),
          timestamp = None,
          paused = pause
        )
      }
  def stop                         = stop()

  def withTimestamper(timestamper: Timestamper) = copy(timestamper = timestamper)

  private def updatePlayer(c: Player)(f: ByoyomiClockPlayer => ByoyomiClockPlayer) =
    copy(players = players.update(c, f))

  def goBerserk(c: Player) = updatePlayer(c) { _.goBerserk }

  def setRemainingTime(c: Player, centis: Centis) =
    updatePlayer(c) {
      _.setRemaining(centis)
    }

  def giveTime(c: Player, t: Centis) =
    updatePlayer(c) {
      _.giveTime(t)
    }

  def switch(switchPlayer: Boolean = true) =
    copy(
      player = if (switchPlayer) !player else player,
      timestamp = timestamp.map(_ => now)
    )

  def step(
      metrics: MoveMetrics = MoveMetrics(),
      gameActive: Boolean = true,
      switchClock: Boolean = true
  ) =
    (timestamp match {
      case None    =>
        metrics.clientLag.fold(this) { l =>
          updatePlayer(player) { _.recordLag(l) }
        }
      case Some(t) => {
        val elapsed = toNow(t)
        val lag     = ~metrics.reportedLag(elapsed) nonNeg

        val competitor          = players(player)
        val remaining           = competitor.remaining
        val (lagComp, lagTrack) = competitor.lag.onMove(lag)
        val moveTime            = (elapsed - lagComp) nonNeg

        // As long as game is still in progress, and we have enough time left (including byoyomi and periods)
        val clockActive  = gameActive && moveTime < remaining + competitor.periodsLeft * competitor.byoyomi
        // The number of periods the move stretched over
        val periodSpan   = periodsInUse(player, moveTime)
        // TODO: If we could assume you were _always_ using byoyomi, that would simplify this (and other code)
        val usingByoyomi =
          competitor.byoyomi.isPositive && (competitor.spentPeriods > 0 || periodSpan > 0)

        val timeRemainingAfterMove = (remaining - moveTime) + periodSpan * competitor.byoyomi
        val newC                   =
          if (usingByoyomi)
            updatePlayer(player) {
              _.setRemaining(
                (remaining - moveTime) atLeast (if (switchClock) competitor.byoyomi
                                                else timeRemainingAfterMove)
              )
                .spendPeriods(periodSpan)
                .copy(lag = lagTrack, lastMoveTime = moveTime)
            }
          else
            updatePlayer(player) {
              _.takeTime(moveTime)
                .spendPeriods(periodSpan)
                .copy(lag = lagTrack, lastMoveTime = moveTime)
            }

        if (clockActive) newC else newC.hardStop
      }
    }).switch(switchClock)

  def refundPeriods(c: Player, p: Int) =
    updatePlayer(c) {
      _.refundPeriods(p)
    }

  def byoyomiOf(c: Player)               = players(c).byoyomi
  def spentPeriodsOf(c: Player)          = players(c).spentPeriods
  override def lastMoveTimeOf(c: Player) = players(c).lastMoveTime

  def byoyomi        = config.byoyomi
  def byoyomiSeconds = config.byoyomiSeconds
  def periodsTotal   = config.periodsTotal

}

case class ByoyomiClockPlayer(
    config: ByoyomiClock.Config,
    lag: LagTracker,
    elapsed: Centis = Centis(0),
    spentPeriods: Int = 0,
    berserk: Boolean = false,
    lastMoveTime: Centis = Centis(0)
) extends PlayerTimerBase {

  def recordLag(l: Centis) = copy(lag = lag.recordLag(l))

  def periodsLeft = math.max(periodsTotal - spentPeriods, 0)

  // TODO: use the useIncrement value
  def takeTime(t: Centis): ByoyomiClockPlayer = copy(elapsed = elapsed + t)

  def giveTime(t: Centis): ByoyomiClockPlayer = takeTime(-t)

  def setRemaining(t: Centis) = copy(elapsed = limit - t)

  def setPeriods(p: Int) = copy(spentPeriods = p)

  def spendPeriods(p: Int) = copy(spentPeriods = spentPeriods + p)

  def refundPeriods(p: Int) = spendPeriods(-math.min(p, spentPeriods))

  def byoyomi = if (berserk) Centis(0) else config.byoyomi

  def periodsTotal = if (berserk) 0 else config.periodsTotal

  def goBerserk = copy(berserk = true)

  def remaining: Centis = limit - elapsed

  def graceSeconds: Int = 0
}

object ByoyomiClockPlayer {
  def withConfig(config: ByoyomiClock.Config) =
    ByoyomiClockPlayer(
      config,
      LagTracker.init(config)
    ).setPeriods(config.initPeriod)
}

object ByoyomiClock {
  private val limitFormatter = new DecimalFormat("#.##")

  // All unspecified durations are expressed in seconds
  case class Config(limitSeconds: Int, incrementSeconds: Int, byoyomiSeconds: Int, periods: Int)
      extends ClockConfig {

    // TODO: We need to remove the ByoyomiClock completely, I think.
    lazy val timer = Timer(limit)

    def berserkable = (incrementSeconds == 0 && byoyomiSeconds == 0) || limitSeconds > 0

    // Activate low time warning when between 10 and 90 seconds remain
    def emergSeconds = math.min(90, math.max(10, limitSeconds / 8))

    // Estimate 60 moves (per player) per game
    def estimateTotalSeconds = limitSeconds + 60 * incrementSeconds + 25 * periodsTotal * byoyomiSeconds
    def estimateTotalTime    = Centis.ofSeconds(estimateTotalSeconds)
    def hasIncrement         = incrementSeconds > 0
    def hasByoyomi           = byoyomiSeconds > 0
    def increment            = Centis.ofSeconds(incrementSeconds)
    def graceSeconds         = incrementSeconds
    def byoyomi              = Centis.ofSeconds(byoyomiSeconds)

    def limit = Centis.ofSeconds(limitSeconds)

    def periodsTotal =
      if (hasByoyomi) math.max(periods, 1)
      else 0

    def limitInMinutes = limitSeconds / 60d

    def toClock = ByoyomiClock(this)

    def limitString: String =
      limitSeconds match {
        case l if l % 60 == 0 => (l / 60).toString
        case 15 => "¼"
        case 30 => "½"
        case 45 => "¾"
        case 90 => "1.5"
        case _  => limitFormatter.format(limitSeconds / 60d)
      }

    def startsAtZero = limitSeconds == 0 && hasByoyomi

    def berserkPenalty =
      if (limitSeconds < 60 * incrementSeconds || limitSeconds < 25 * byoyomiSeconds) Centis(0)
      else Centis(limitSeconds * (100 / 2))

    def initTime =
      if (limitSeconds == 0 && hasByoyomi) byoyomi atLeast Centis(500)
      else if (limitSeconds == 0) increment atLeast Centis(500)
      else limit

    def initPeriod = if (startsAtZero) 1 else 0

    def baseString: String = if (hasIncrement) s"${limitString}+${incrementSeconds}" else s"${limitString}"

    def periodsString: String = if (periodsTotal > 1) s"(${periodsTotal}x)" else ""

    def show: String = if (hasByoyomi) s"${baseString}|${byoyomiSeconds}${periodsString}"
    else if (hasIncrement) baseString
    else s"${baseString}|0"

    override def toString = s"${limitSeconds}.${incrementSeconds}.${byoyomiSeconds}.${periodsTotal}"
  }

  def parseJPTime(str: String): Option[Int] = {
    if (str contains "時間")
      str
        .takeWhile(_ != '時')
        .toIntOption
        .map(_ * 3600 + (parseJPTime(str.reverse.takeWhile(_ != '間').reverse) | 0))
    else if (str contains "分")
      str
        .takeWhile(_ != '分')
        .toIntOption
        .map(_ * 60 + (parseJPTime(str.reverse.takeWhile(_ != '分').reverse) | 0))
    else str.filterNot(_ == '秒').toIntOption
  }

  val kifTime          = """(?:\d+(?:秒|分|時間)?)+"""
  lazy val KifClkRegex = raw"""($kifTime)(?:[\+|\|]($kifTime))?(?:\((\d)\))?(?:[\+|\|]($kifTime))?""".r

  // 持ち時間: 10分|20秒(1)+10 -> 600 init, 10inc, 20 byo, 1 per
  def readKifConfig(str: String): Option[Config] =
    str match {
      case KifClkRegex(initStr, byoStr, perStr, incStr) =>
        for {
          init <- parseJPTime(initStr)
          byo  <- Option(byoStr).fold(0.some)(parseJPTime _)
          per  <- Option(perStr).fold(1.some)(_ toIntOption)
          inc  <- Option(incStr).fold(0.some)(parseJPTime _)
        } yield Config(init, inc, byo, per)
      case _                                            => none
    }

  def readCsaConfig(str: String): Option[Config] =
    str.split("""\+|\|""") match {
      case Array(initStr, byoStr)         =>
        for {
          init <- initStr.toIntOption
          byo  <- byoStr.toIntOption
        } yield Config(init, 0, byo, 1)
      case Array(initStr, byoStr, incStr) =>
        for {
          init <- initStr.toIntOption
          byo  <- byoStr.toIntOption
          inc  <- incStr.toIntOption
        } yield Config(init, inc, byo, 1)
      case _                              => none
    }

  def apply(limit: Int, increment: Int, byoyomi: Int, periods: Int): ByoyomiClock = {
    apply(Config(limit, increment, byoyomi, periods))
  }

  def apply(config: Config): ByoyomiClock = {
    val player = ByoyomiClockPlayer.withConfig(config)
    ByoyomiClock(
      config = config,
      player = Player.P1,
      players = Player.Map(player, player),
      timestamp = None
    )
  }
}
