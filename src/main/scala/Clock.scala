package strategygames

import java.text.DecimalFormat
import cats.syntax.option._

sealed trait ClockConfig {
  // Abstract attributes
  def estimateTotalSeconds: Int
  def estimateTotalTime: Centis
  def increment: Centis
  def incrementSeconds: Int
  def limit: Centis
  def limitInMinutes: Double
  def limitSeconds: Int
  def initTime: Centis
  def berserkPenalty: Centis
  def berserkable: Boolean
  def emergSeconds: Int
  def show: String
  def toClock: Clock
  def startsAtZero: Boolean
}

sealed trait ClockInfo {
  val time: Centis
  val periods: Int
}

sealed trait ClockPlayer {
  // Abstract attributes
  val config: ClockConfig
  val lag: LagTracker
  val elapsed: Centis
  val berserk: Boolean
  val lastMoveTime: Centis

  def recordLag(l: Centis): ClockPlayer
  def takeTime(t: Centis): ClockPlayer
  def setRemaining(t: Centis): ClockPlayer
  def goBerserk: ClockPlayer
  def giveTime(t: Centis): ClockPlayer

  // Implemented attributes
  def limit = {
    if (berserk) config.initTime - config.berserkPenalty
    else config.initTime
  }

  def remaining: Centis = limit - elapsed
  def increment: Centis = if (berserk) Centis(0) else config.increment
}

sealed trait Clock {
  import timestamper.toNow

  // Abstract values
  val player: Player
  // val players: Player.Map[ClockPlayer]
  val timestamper: Timestamper
  val timer: Option[Timestamp]
  val config: ClockConfig

  // Abstract methods
  def start: Clock
  def stop: Clock
  def hardStop: Clock
  def switch(switchPlayer: Boolean = true): Clock
  def step(
      metrics: MoveMetrics = MoveMetrics(),
      gameActive: Boolean = true,
      switchClock: Boolean = true
  ): Clock
  def withTimestamper(timestamper: Timestamper): Clock
  def outOfTime(c: Player, withGrace: Boolean): Boolean
  def giveTime(c: Player, t: Centis): Clock
  def goBerserk(c: Player): Clock
  def clockPlayer(c: Player): ClockPlayer
  def clockPlayerExists(f: ClockPlayer => Boolean): Boolean
  def allClockPlayers: Seq[ClockPlayer]
  def lagCompAvg: Centis
  def setRemainingTime(p: Player, t: Centis): Clock

  def currentClockFor(c: Player): ClockInfo

  // Implemented attributes
  def remainingTime(c: Player)  = (clockPlayer(c).remaining - pending(c)) nonNeg
  def moretimeable(c: Player)   = clockPlayer(c).remaining.centis < 100 * 60 * 60 * 2
  def incrementOf(c: Player)    = clockPlayer(c).increment
  def lastMoveTimeOf(c: Player) = clockPlayer(c).lastMoveTime

  def takeback(switchPlayer: Boolean = true) = switch(switchPlayer)

  def isRunning            = timer.isDefined
  def berserked(c: Player) = clockPlayer(c).berserk
  def lag(c: Player)       = clockPlayer(c).lag

  // Lowball estimate of next move's lag comp for UI butter.
  def lagCompEstimate(c: Player) = clockPlayer(c).lag.compEstimate

  @inline def timerFor(c: Player) = if (c == player) timer else None
  @inline def pending(c: Player)  = timerFor(c).fold(Centis(0))(toNow)

  def estimateTotalSeconds = config.estimateTotalSeconds
  def estimateTotalTime    = config.estimateTotalTime
  def increment            = config.increment
  def incrementSeconds     = config.incrementSeconds
  def limit                = config.limit
  def limitInMinutes       = config.limitInMinutes
  def limitSeconds         = config.limitSeconds
}

case class FischerClockInfo(time: Centis) extends ClockInfo {
  val periods: Int = 0
}

// All unspecified durations are expressed in seconds
case class FischerClock(
    config: FischerClock.Config,
    player: Player,
    players: Player.Map[FischerClockPlayer],
    timer: Option[Timestamp] = None,
    timestamper: Timestamper = RealTimestamper
) extends Clock {
  import timestamper.{ now, toNow }

  def clockPlayer(c: Player)                       = players(c)
  def clockPlayerExists(f: ClockPlayer => Boolean) = players.exists(f)
  def allClockPlayers: Seq[FischerClockPlayer]     = players.all
  def lagCompAvg                                   = players map { ~_.lag.compAvg } reduce (_ avg _)

  def currentClockFor(c: Player) = {
    val elapsed               = pending(c)
    val remainingAfterElapsed = players(c).remaining - elapsed
    FischerClockInfo(remainingAfterElapsed)
  }

  def outOfTime(c: Player, withGrace: Boolean) =
    players(c).remaining <=
      timerFor(c).fold(Centis(0)) { t =>
        if (withGrace) (toNow(t) - (players(c).lag.quota atMost Centis(200))) nonNeg
        else toNow(t)
      }

  def start = if (isRunning) this else copy(timer = Option(now))

  def stop = {
    timer.fold(this) { t =>
      val curT = toNow(t)
      copy(
        players = players.update(
          player,
          _.takeTime(curT).copy(lastMoveTime = curT)
        ),
        timer = None
      )
    }
  }

  def hardStop = copy(timer = None)

  def updatePlayer(c: Player)(f: FischerClockPlayer => FischerClockPlayer) =
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
      timer = timer.map(_ => now)
    )

  def step(
      metrics: MoveMetrics = MoveMetrics(),
      gameActive: Boolean = true,
      switchClock: Boolean = true
  ) =
    (timer match {
      case None    =>
        metrics.clientLag.fold(this) { l =>
          updatePlayer(player) { _.recordLag(l) }
        }
      case Some(t) =>
        val elapsed = toNow(t)
        val lag     = ~metrics.reportedLag(elapsed) nonNeg

        val competitor          = players(player)
        val (lagComp, lagTrack) = competitor.lag.onMove(lag)

        val moveTime = (elapsed - lagComp) nonNeg

        val clockActive = gameActive && moveTime < competitor.remaining
        val inc         = (clockActive && switchClock) ?? competitor.increment

        val newC = updatePlayer(player) {
          _.takeTime(moveTime - inc)
            .copy(lag = lagTrack, lastMoveTime = moveTime)
        }

        if (clockActive) newC else newC.hardStop
    }).switch(switchClock)

  // To do: safely add this to takeback to remove inc from player.
  // def deinc = updatePlayer(player, _.giveTime(-incrementOf(player)))

}

case class FischerClockPlayer(
    config: FischerClock.Config,
    lag: LagTracker,
    elapsed: Centis = Centis(0),
    berserk: Boolean = false,
    lastMoveTime: Centis = Centis(0)
) extends ClockPlayer {

  def recordLag(l: Centis) = copy(lag = lag.recordLag(l))

  def takeTime(t: Centis) = copy(elapsed = elapsed + t)

  def setRemaining(t: Centis) = copy(elapsed = limit - t)

  def goBerserk = copy(berserk = true)

  def giveTime(t: Centis): FischerClockPlayer = takeTime(-t)
}

object FischerClockPlayer {
  def withConfig(config: FischerClock.Config) =
    FischerClockPlayer(
      config,
      LagTracker.init(config)
    )
}

object FischerClock {
  private val limitFormatter = new DecimalFormat("#.##")

  // All unspecified durations are expressed in seconds
  case class Config(limitSeconds: Int, incrementSeconds: Int) extends ClockConfig {

    def berserkable = incrementSeconds == 0 || limitSeconds > 0

    def emergSeconds = math.min(60, math.max(10, limitSeconds / 8))

    def estimateTotalSeconds = limitSeconds + 40 * incrementSeconds

    def estimateTotalTime = Centis.ofSeconds(estimateTotalSeconds)

    def hasIncrement = incrementSeconds > 0

    def increment = Centis.ofSeconds(incrementSeconds)

    def limit = Centis.ofSeconds(limitSeconds)

    def limitInMinutes = limitSeconds / 60d

    def toClock = FischerClock(this)

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

  def apply(limit: Int, increment: Int): FischerClock = apply(Config(limit, increment))

  def apply(config: Config): FischerClock = {
    val player = FischerClockPlayer.withConfig(config)
    FischerClock(
      config = config,
      player = Player.P1,
      players = Player.Map(player, player),
      timer = None
    )
  }
}

case class ByoyomiClockInfo(time: Centis, periods: Int) extends ClockInfo

// All unspecified durations are expressed in seconds
case class ByoyomiClock(
    config: ByoyomiClock.Config,
    player: Player,
    players: Player.Map[ByoyomiClockPlayer],
    timer: Option[Timestamp] = None,
    timestamper: Timestamper = RealTimestamper
) extends Clock {
  import timestamper.{ now, toNow }

  def clockPlayer(c: Player)                       = players(c)
  def clockPlayerExists(f: ClockPlayer => Boolean) = players.exists(f)
  def allClockPlayers: Seq[ByoyomiClockPlayer]     = players.all
  def lagCompAvg                                   = players map { ~_.lag.compAvg } reduce (_ avg _)

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

  def outOfTime(c: Player, withGrace: Boolean) = {
    val player        = players(c)
    val timeUsed      = timerFor(c).fold(Centis(0))(t =>
      if (withGrace) (toNow(t) - (players(c).lag.quota atMost Centis(200))) nonNeg
      else toNow(t)
    )
    val timeRemaining = player.remaining + player.periodsLeft * player.byoyomi

    timeRemaining <= timeUsed
  }

  def start = if (isRunning) this else copy(timer = Option(now))

  def stop =
    timer.fold(this) { t =>
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
        timer = None
      )
    }

  def hardStop = copy(timer = None)

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
      timer = timer.map(_ => now)
    )

  def step(
      metrics: MoveMetrics = MoveMetrics(),
      gameActive: Boolean = true,
      switchClock: Boolean = true
  ) =
    (timer match {
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
              _.takeTime(moveTime - ((clockActive && switchClock) ?? competitor.increment))
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
) extends ClockPlayer {

  def recordLag(l: Centis) = copy(lag = lag.recordLag(l))

  def periodsLeft = math.max(periodsTotal - spentPeriods, 0)

  def takeTime(t: Centis) = copy(elapsed = elapsed + t)

  def giveTime(t: Centis): ByoyomiClockPlayer = takeTime(-t)

  def setRemaining(t: Centis) = copy(elapsed = limit - t)

  def setPeriods(p: Int) = copy(spentPeriods = p)

  def spendPeriods(p: Int) = copy(spentPeriods = spentPeriods + p)

  def refundPeriods(p: Int) = spendPeriods(-math.min(p, spentPeriods))

  def byoyomi = if (berserk) Centis(0) else config.byoyomi

  def periodsTotal = if (berserk) 0 else config.periodsTotal

  def goBerserk = copy(berserk = true)
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

    def berserkable = (incrementSeconds == 0 && byoyomiSeconds == 0) || limitSeconds > 0

    // Activate low time warning when between 10 and 90 seconds remain
    def emergSeconds = math.min(90, math.max(10, limitSeconds / 8))

    // Estimate 60 moves (per player) per game
    def estimateTotalSeconds = limitSeconds + 60 * incrementSeconds + 25 * periodsTotal * byoyomiSeconds

    def estimateTotalTime = Centis.ofSeconds(estimateTotalSeconds)

    def hasIncrement = incrementSeconds > 0

    def hasByoyomi = byoyomiSeconds > 0

    def increment = Centis.ofSeconds(incrementSeconds)

    def byoyomi = Centis.ofSeconds(byoyomiSeconds)

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
      timer = None
    )
  }
}
