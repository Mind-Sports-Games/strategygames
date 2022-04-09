package strategygames.tiebreaks

sealed trait Participant
case class Player(id: String) extends Participant
case object Virtual           extends Participant

sealed trait Presence
case object Absent  extends Presence
case object Present extends Presence

sealed trait POV
case class Hero(participant: Player, presence: Presence)     extends POV
case class Foe(participant: Participant, presence: Presence) extends POV

// Results are always from a particualar point-of-view. So we only need
// the three cases.
sealed trait Outcome
case object WonAgainst extends Outcome
case object DrewWith   extends Outcome
case object LostTo     extends Outcome

case class Result(player: Hero, outcome: Outcome, opponent: Foe, round: Int)

trait Tournament {
  val nbRounds: Int
  val players: List[Participant]
  def resultsForPlayer(player: Player): List[Result]
}

class Tiebreak(val tournament: Tournament) {

  val Win: Int  = 2
  val Draw: Int = 1
  val Loss: Int = 0

  // In the following we distinguish between score and tiebreak.
  // Score is what you get for the Result directly.
  // Tiebreak is what you get from your opponent for a tiebreak
  def scoreAtRound(p: Player, round: Int): Int =
    tournament.resultsForPlayer(p).take(round).map(score).sum

  def score(p: Player): Int =
    scoreAtRound(p, tournament.nbRounds)

  def virtualOpponentScoreAtRound(p: Player, round: Int): Int =
    scoreAtRound(p, round) + remainingRoundDraws(round)

  def remainingRoundDraws(round: Int): Int =
    (tournament.nbRounds - round - 1) * Draw

  def score(g: Result): Int =
    g match {
      case Result(_, WonAgainst, _, _) => Win
      case Result(_, DrewWith, _, _)   => Draw
      case Result(_, LostTo, _, _)     => Loss

      // Not sure any of the rest of these make any sense.
      case _ => Loss
    }

  // When used for tie-breaking among players with the same raw score, no
  // multiplying is necessary and the sum of the raw scores of the opponents
  // played is used to break ties (Golombek 1977). When used as a tie-break
  // system, it is equivalent to the Solkoff system.
  // - https://en.wikipedia.org/wiki/Buchholz_system
  def buccholz(p: Player): Int =
    tournament.resultsForPlayer(p).map(buccholzForGame).sum

  def buccholzForGame(g: Result): Int =
    g match {
      // Regular game played with regular result
      case Result(Hero(Player(_), Present), _, Foe(p: Player, Present), _) => score(p)

      case Result(Hero(p: Player, Present), WonAgainst, Foe(Virtual, _), _) =>
        virtualOpponentScoreAtRound(p, g.round)

      // Not sure any of the rest of these make any sense.
      case _ => 0
    }

  def sonnenbornBerger(p: Player): Int =
    tournament.resultsForPlayer(p).map(sonnenbornBergerForGame).sum

  def sonnenbornBergerForGame(g: Result): Int =
    g match {
      // Regular game played with regular result
      case Result(Hero(Player(_), Present), WonAgainst, Foe(p: Player, Present), _) => score(p)
      case Result(Hero(Player(_), Present), DrewWith, Foe(p: Player, Present), _)   => score(p) / 2
      case Result(Hero(Player(_), Present), LostTo, Foe(Player(_), Present), _)     => 0

      // All other results are not worth anything for the current lila implementation
      case Result(Hero(Player(_), Present), _, Foe(Virtual, _), _) => 0

      // Not sure any of the rest of these make any sense.
      case _ => 0
    }

}

object Tiebreak {
  def bye(p: Player, r: Int)                   = Result(Hero(p, Present), WonAgainst, Foe(Virtual, Absent), r)
  def withdrawn(p: Player, r: Int)             = Result(Hero(p, Absent), LostTo, Foe(Virtual, Absent), r)
  def win(p: Player, o: Player, r: Int)        = Result(Hero(p, Present), WonAgainst, Foe(o, Present), r)
  def lose(p: Player, o: Player, r: Int)       = Result(Hero(p, Present), LostTo, Foe(o, Present), r)
  def draw(p: Player, o: Player, r: Int)       = Result(Hero(p, Present), DrewWith, Foe(o, Present), r)
  def absentLoss(p: Player, o: Player, r: Int) = Result(Hero(p, Absent), LostTo, Foe(o, Present), r)
  def presentWin(p: Player, o: Player, r: Int) = Result(Hero(p, Absent), WonAgainst, Foe(o, Absent), r)
}
