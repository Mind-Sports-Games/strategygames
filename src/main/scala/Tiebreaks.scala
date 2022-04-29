package strategygames.tiebreaks

sealed trait Participant
case class Player(id: String) extends Participant
case object Virtual           extends Participant

sealed trait Presence
case object Absent  extends Presence
case object Present extends Presence

sealed trait POV
case class Hero(participant: Player, presence: Presence) extends POV
case class Foe(participant: Participant)                 extends POV

// Results are always from a particular point-of-view. So we only need
// the three cases.
sealed trait Outcome
case object WonAgainst extends Outcome
case object DrewWith   extends Outcome
case object LostTo     extends Outcome

case class Result(round: Int, player: Hero, outcome: Outcome, opponent: Foe)

trait Tournament {
  val nbRounds: Int
  def resultsForPlayer(player: Player): List[Result]
}

class Tiebreak(val tournament: Tournament) {

  val Win: Double  = 2
  val Draw: Double = 1
  val Loss: Double = 0
  // NOTE: we use zero-base indexing here.

  // In the following we distinguish between score and tiebreak.
  // Score is what you get for the Result directly.
  // Tiebreak is what you get from your opponent for a tiebreak
  def virtualOpponentScoreAtRound(p: Player, round: Int): Double =
    scoreAtRound(p, round) + remainingRoundDraws(round)

  def remainingRoundDraws(round: Int): Double =
    ((tournament.nbRounds) - round - 1) * Draw

  def scoreAtRound(p: Player, round: Int): Double =
    tournament.resultsForPlayer(p).take(round).map(score).sum

  def score(p: Player): Double =
    scoreAtRound(p, tournament.nbRounds)

  def scoreForOpponentTiebreak(scoreForOpponentTiebreakResult: Result => Double)(p: Player): Double =
    tournament.resultsForPlayer(p).map(scoreForOpponentTiebreakResult).sum

  val fideScoreForOpponentTiebreak = scoreForOpponentTiebreak(fideScoreForOpponentTiebreakResult) _

  def score(g: Result): Double =
    g match {
      case Result(_, _, WonAgainst, _) => Win
      case Result(_, _, DrewWith, _)   => Draw
      case Result(_, _, LostTo, _)     => Loss

      // Not sure any of the rest of these make any sense.
      case _ => Loss
    }

  private def fideScoreForOpponentTiebreakResult(g: Result): Double =
    g match {
      // Regular game played with regular result
      case Result(_, Hero(Player(_), Present), WonAgainst, Foe(Player(_))) => Win
      case Result(_, Hero(Player(_), Present), DrewWith, Foe(Player(_)))   => Draw
      case Result(_, Hero(Player(_), Present), LostTo, Foe(Player(_)))     => Loss

      // According to FIDE:
      // 13.15.3: For tie-break purposes all unplayed games in which players are
      // indirectely (sic) involved (results by forfeit of opponents) are consid-
      // ered to have been drawn"
      //
      // Both the extra quote AND the mispelling are part of the doc:
      // https://handbook.fide.com/files/handbook/C02Standards.pdf
      case Result(_, Hero(Player(_), _), _, Foe(Virtual)) => Draw

      // Not sure any of the rest of these make any sense.
      case _ => Loss
    }

  // When used for tie-breaking among players with the same raw score, no
  // multiplying is necessary and the sum of the raw scores of the opponents
  // played is used to break ties (Golombek 1977). When used as a tie-break
  // system, it is equivalent to the Solkoff system.
  // - https://en.wikipedia.org/wiki/Buchholz_system



  // MSO Rules (based on FIDE) as per MSO arbiter, Mike Dixon:
  // ---------------------------------------------------------------------------
  // If someone you played later withdraws you get 1/2 a point per round they
  // skipped added to your TB (ontop of what they scored in the tournament). You
  // get the same for a bye (0.5 per round ontop of your score going into that
  // match). And if you play somebody who got a bye their bye only counts as
  // 0.5 for your TB (not the 1.0 that they get for the bye)
  // 
  def fideBuchholz(p: Player): Double =
    tournament.resultsForPlayer(p).map(fideBuchholzForGame).sum

  def fideBuchholzForGame(g: Result): Double =
    g match {
      // Regular game played with regular result
      case Result(_, Hero(Player(_), Present), _, Foe(p: Player)) => fideScoreForOpponentTiebreak(p)

      case Result(round, Hero(p: Player, Present), WonAgainst, Foe(Virtual)) =>
        virtualOpponentScoreAtRound(p, round)

      // Not sure any of the rest of these make any sense.
      case _                                                                 => 0
    }

  // The name lila refers to the fact that this has some quirks specific to the
  // lila implementation. It doesn't deal with virtual opponents tiebreak values
  // for byes.
  def lilaSonnenbornBerger(p: Player): Double =
    tournament.resultsForPlayer(p).map(lilaSonnenbornBergerForGame).sum

  def lilaSonnenbornBergerForGame(g: Result): Double =
    g match {
      // Regular game played with regular result
      case Result(_, Hero(Player(_), Present), WonAgainst, Foe(p: Player)) => score(p)
      case Result(_, Hero(Player(_), Present), DrewWith, Foe(p: Player))   => score(p) / 2
      case Result(_, Hero(Player(_), Present), LostTo, Foe(Player(_)))     => 0

      // All other results are not worth anything for the current lila implementation
      case Result(_, Hero(Player(_), Present), _, Foe(Virtual)) => 0

      // Not sure any of the rest of these make any sense.
      case _ => 0
    }

}

object Tiebreak {
  case class Round(r: Int) {
    def bye(p: Player)                      = Result(r, Hero(p, Present), WonAgainst, Foe(Virtual))
    def withdrawn(p: Player)                = Result(r, Hero(p, Absent), LostTo, Foe(Virtual))
    def win(p: Player, o: Player)           = Result(r, Hero(p, Present), WonAgainst, Foe(o))
    def lose(p: Player, o: Player)          = Result(r, Hero(p, Present), LostTo, Foe(o))
    def draw(p: Player, o: Player)          = Result(r, Hero(p, Present), DrewWith, Foe(o))
    def noShowDraw(p: Player)               = Result(r, Hero(p, Absent), DrewWith, Foe(Virtual))
    def noShowLoss(p: Player, o: Player)    = Result(r, Hero(p, Absent), LostTo, Foe(o))
    def absentLoss(p: Player)               = Result(r, Hero(p, Absent), LostTo, Foe(Virtual))
    def noOpponentWin(p: Player, o: Player) = Result(r, Hero(p, Present), WonAgainst, Foe(o))
  }
}
