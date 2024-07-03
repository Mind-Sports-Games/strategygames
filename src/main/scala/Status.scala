package strategygames

sealed abstract class Status(val id: Int) extends Ordered[Status] {

  val name = s"${toString.head.toLower}${toString.tail}"

  def compare(other: Status) = id compare other.id

  def is(s: Status): Boolean = this == s

  def is(f: Status.type => Status): Boolean = is(f(Status))

}

object Status {

  case object Created             extends Status(10)
  case object Started             extends Status(20)
  case object Aborted             extends Status(25) // from this point the game is finished
  case object Mate                extends Status(30)
  case object Resign              extends Status(31)
  case object Stalemate           extends Status(32)
  case object Timeout             extends Status(33) // when player leaves the game
  case object Draw                extends Status(34)
  case object Outoftime           extends Status(35) // clock flag
  case object Cheat               extends Status(36)
  case object NoStart             extends Status(37) // the player did not make the first move in time
  case object UnknownFinish       extends Status(38) // we don't know why the game ended
  case object PerpetualCheck      extends Status(39) // For Shogi/Xiangqi
  case object SingleWin           extends Status(40) // For Backgammon
  case object GammonWin           extends Status(41) // For Backgammon
  case object BackgammonWin       extends Status(42) // For Backgammon
  case object ResignGammon        extends Status(43) // For Backgammon
  case object ResignBackgammon    extends Status(44) // For Backgammon
  case object RuleOfGin           extends Status(45) // clock flag but still awarded to player who flagged
  case object GinGammon           extends Status(46) // clock flag but still awarded to player who flagged
  case object GinBackgammon       extends Status(47) // clock flag but still awarded to player who flagged
  case object OutoftimeGammon     extends Status(48) // clock flag and loss for Backgammon
  case object OutoftimeBackgammon extends Status(49) // clock flag and loss for Backgammon
  case object VariantEnd          extends Status(60) // the variant has a special ending

  val all = List(
    Created,
    Started,
    Aborted,
    Mate,
    Resign,
    Stalemate,
    Timeout,
    Draw,
    Outoftime,
    Cheat,
    NoStart,
    UnknownFinish,
    PerpetualCheck,
    SingleWin,
    GammonWin,
    BackgammonWin,
    ResignGammon,
    ResignBackgammon,
    RuleOfGin,
    GinGammon,
    GinBackgammon,
    OutoftimeGammon,
    OutoftimeBackgammon,
    VariantEnd
  )

  val finishedNotCheated = all filter { s =>
    s.id >= Mate.id && s.id != Cheat.id
  }

  val finishedWithWinner = List(
    Mate,
    Resign,
    ResignGammon,
    ResignBackgammon,
    Timeout,
    Outoftime,
    OutoftimeGammon,
    OutoftimeBackgammon,
    RuleOfGin,
    GinGammon,
    GinBackgammon,
    Cheat,
    NoStart,
    VariantEnd
  )

  val flagged = List(Outoftime, OutoftimeGammon, OutoftimeBackgammon, RuleOfGin, GinGammon, GinBackgammon)

  val resigned = List(Resign, ResignGammon, ResignBackgammon)

  val byId = all map { v =>
    (v.id, v)
  } toMap

  def apply(id: Int): Option[Status] = byId get id

}
