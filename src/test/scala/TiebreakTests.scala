package strategygames.tiebreaks

import org.specs2.matcher.ValidatedMatchers
import org.specs2.mutable.Specification

import Tiebreak._

// https://playstrategy.org/swiss/EY1k6otN
class TournamentEY1k6otN extends Tournament {
  val derarzt = Player("derarzt")
  val ayala   = Player("Ayala7")
  val rapha   = Player("raphael20odrich")
  val irina   = Player("Irina")
  val berserk = Player("berserker")
  val mega    = Player("megamau")
  val matt    = Player("statmatt")
  val manka   = Player("mankalacz")
  val fun     = Player("FunWithFlags")
  val egor    = Player("egormmaksymov341")

  val nbRounds = 7
  val players  = List(
    derarzt,
    ayala,
    rapha,
    irina,
    berserk,
    mega,
    matt,
    manka,
    fun,
    egor
  )

  def resultsForPlayer(player: Player): List[Result] =
    player.id match {
      case "derarzt"          =>
        List(
          Round(0).win(derarzt, matt),
          Round(1).win(derarzt, ayala),
          Round(2).win(derarzt, rapha),
          Round(3).win(derarzt, berserk),
          Round(4).win(derarzt, irina),
          Round(5).win(derarzt, mega),
          Round(6).win(derarzt, fun)
        )
      case "Ayala7"           =>
        List(
          Round(0).win(ayala, manka),
          Round(1).lose(ayala, derarzt),
          Round(2).win(ayala, mega),
          Round(3).win(ayala, rapha),
          Round(4).win(ayala, berserk),
          Round(5).win(ayala, irina),
          Round(6).bye(ayala)
        )
      case "raphael20odrich"  =>
        List(
          Round(0).win(rapha, mega),
          Round(1).win(rapha, irina),
          Round(2).lose(rapha, derarzt),
          Round(3).lose(rapha, ayala),
          Round(4).win(rapha, manka),
          Round(5).win(rapha, matt),
          Round(6).win(rapha, berserk)
        )
      case "Irina"            =>
        List(
          Round(0).win(irina, berserk),
          Round(1).lose(irina, rapha),
          Round(2).win(irina, fun),
          Round(3).win(irina, matt),
          Round(4).lose(irina, derarzt),
          Round(5).lose(irina, ayala),
          Round(6).win(irina, mega)
        )
      case "berserker"        =>
        List(
          Round(0).lose(berserk, irina),
          Round(1).bye(berserk),
          Round(2).win(berserk, matt),
          Round(3).lose(berserk, derarzt),
          Round(4).lose(berserk, ayala),
          Round(5).win(berserk, manka),
          Round(6).lose(berserk, rapha)
        )
      case "megamau"          =>
        List(
          Round(0).lose(mega, rapha),
          Round(1).win(mega, manka),
          Round(2).lose(mega, ayala),
          Round(3).bye(mega),
          Round(4).win(mega, fun),
          Round(5).lose(mega, derarzt),
          Round(6).lose(mega, irina)
        )
      case "statmatt"         =>
        List(
          Round(0).lose(matt, derarzt),
          Round(1).win(matt, fun),
          Round(2).lose(matt, berserk),
          Round(3).lose(matt, irina),
          Round(4).bye(matt),
          Round(5).lose(matt, rapha),
          Round(6).win(matt, manka)
        )
      case "mankalacz"        =>
        List(
          Round(0).lose(manka, ayala),
          Round(1).lose(manka, mega),
          Round(2).bye(manka),
          Round(3).win(manka, fun),
          Round(4).lose(manka, rapha),
          Round(5).lose(manka, berserk),
          Round(6).lose(manka, matt)
        )
      case "FunWithFlags"     =>
        List(
          Round(0).noOpponentWin(fun, egor),
          Round(1).lose(fun, matt),
          Round(2).lose(fun, irina),
          Round(3).lose(fun, manka),
          Round(4).lose(fun, mega),
          Round(5).bye(fun),
          Round(6).lose(fun, derarzt)
        )
      case "egormmaksymov341" =>
        List(
          Round(0).noShowLoss(egor, fun),
          Round(1).withdrawn(egor),
          Round(2).withdrawn(egor),
          Round(3).withdrawn(egor),
          Round(4).withdrawn(egor),
          Round(5).withdrawn(egor),
          Round(6).withdrawn(egor)
        )
    }
  List()
}

// https://playstrategy.dev/swiss/UW8K3BjW
class TournamentUW8K3BjW extends Tournament {
  val james_test = Player("james_test")
  val statmatt2  = Player("statmatt2")
  val jheps      = Player("jheps")
  val lakin      = Player("lakin")
  val statmatt   = Player("statmatt")
  val lakin2     = Player("lakin2")

  val nbRounds = 5
  val players  = List(
    james_test,
    statmatt2,
    jheps,
    lakin,
    statmatt,
    lakin2
  )

  def resultsForPlayer(player: Player): List[Result] =
    player.id match {
      case "james_test" =>
        List(
          Round(0).win(james_test, lakin2),
          Round(1).win(james_test, statmatt2),
          Round(2).win(james_test, jheps),
          Round(3).win(james_test, lakin),
          Round(4).draw(james_test, statmatt)
        )
      case "statmatt2"  =>
        List(
          Round(0).absentLoss(statmatt2),
          Round(1).lose(statmatt2, james_test),
          Round(2).win(statmatt2, lakin2),
          Round(3).win(statmatt2, statmatt),
          Round(4).win(statmatt2, lakin)
        )
      case "jheps"      =>
        List(
          Round(0).win(jheps, lakin),
          Round(1).win(jheps, statmatt),
          Round(2).lose(jheps, james_test),
          Round(3).bye(jheps),
          Round(4).absentLoss(jheps)
        )
      case "lakin"      =>
        List(
          Round(0).lose(lakin, jheps),
          Round(1).bye(lakin),
          Round(2).win(lakin, statmatt),
          Round(3).lose(lakin, james_test),
          Round(4).lose(lakin, statmatt2)
        )
      case "statmatt"   =>
        List(
          Round(0).bye(statmatt),
          Round(1).lose(statmatt, jheps),
          Round(2).lose(statmatt, lakin),
          Round(3).lose(statmatt, statmatt2),
          Round(4).draw(statmatt, james_test)
        )
      case "lakin2"     =>
        List(
          Round(0).lose(lakin2, james_test),
          Round(1).absentLoss(lakin2),
          Round(2).lose(lakin2, statmatt2),
          Round(3).absentLoss(lakin2),
          Round(4).absentLoss(lakin2)
        )
    }
  List()
}

class TiebreakTests extends Specification with ValidatedMatchers {
  val draughts         = new TournamentEY1k6otN();
  val tourney          = new Tiebreak(draughts);
  "derarzt results" should {
    tourney.score(draughts.derarzt) must_== 14
    tourney.lilaSonnenbornBerger(draughts.derarzt) must_== 52
    tourney.fideBuchholz(draughts.derarzt) must_== 47
  }
  "ayala results" should {
    tourney.score(draughts.ayala) must_== 12
    tourney.lilaSonnenbornBerger(draughts.ayala) must_== 34
    tourney.fideBuchholz(draughts.ayala) must_== 55
  }
  "rapha results" should {
    tourney.score(draughts.rapha) must_== 10
    tourney.lilaSonnenbornBerger(draughts.rapha) must_== 30
    tourney.fideBuchholz(draughts.rapha) must_== 51
  }
  "irina results" should {
    tourney.score(draughts.irina) must_== 8
    tourney.lilaSonnenbornBerger(draughts.irina) must_== 22
    tourney.fideBuchholz(draughts.irina) must_== 53
  }
  "berserker results" should {
    tourney.score(draughts.berserk) must_== 6
    tourney.lilaSonnenbornBerger(draughts.berserk) must_== 10
    tourney.fideBuchholz(draughts.berserk) must_== 56
  }
  "megamau results" should {
    tourney.score(draughts.mega) must_== 6
    tourney.lilaSonnenbornBerger(draughts.mega) must_== 8
    tourney.fideBuchholz(draughts.mega) must_== 54
  }
  "statmatt results" should {
    tourney.score(draughts.matt) must_== 6
    tourney.lilaSonnenbornBerger(draughts.matt) must_== 8
    tourney.fideBuchholz(draughts.matt) must_== 47
  }
  "mankalacz results" should {
    tourney.score(draughts.manka) must_== 4
    tourney.lilaSonnenbornBerger(draughts.manka) must_== 4
    tourney.fideBuchholz(draughts.manka) must_== 43
  }
  "funwithflags results" should {
    tourney.score(draughts.fun) must_== 4
    tourney.lilaSonnenbornBerger(draughts.fun) must_== 0
    tourney.fideBuchholz(draughts.fun) must_== 44
  }
  "egormmaksymov341 results" should {
    tourney.score(draughts.egor) must_== 0
    tourney.lilaSonnenbornBerger(draughts.egor) must_== 0
    tourney.fideBuchholz(draughts.egor) must_== 0
  }
  val flipello1        = new TournamentUW8K3BjW();
  val flipello1Tourney = new Tiebreak(flipello1);

  // --------------------------------------------------------------------------
  // This is the value that each player represents on a tie break with the
  // explanation. Remember all points are doubled.
  // lakin2     - 3 points => 3 points from the games where they were withdrawn
  //                         which count as a draw as per the FIDE rule.
  // statmatt   - 2 points => 1 from the first round bye, 1 from the draw.
  // lakin      - 3 points => 1 from bye
  //                       => 2 from win
  // statmatt2  - 7 points => 1 from first round based on the same FIDE rule
  //                       => 6 from their 3 wins
  // jheps      - 6 points => 4 from their wins
  //                       => 1 from their bye
  //                       => 1 from their withdrawn final round game
  // james_test - 9 points => 8 from wins
  //                       => 1 from draw
  // --------------------------------------------------------------------------
  "james_test results" should {
    flipello1Tourney.score(flipello1.james_test) must_== 9.0
    // played:    lakin2 + statmatt2 + jheps + lakin + statmatt
    // gh points: 3      + 7         + 6     + 3     + 2 = 21 points
    flipello1Tourney.fideBuchholz(flipello1.james_test) must_== 21
  }
  "jheps results" should {
    flipello1Tourney.score(flipello1.jheps) must_== 6.0
    // played:   lakin + statmatt + james_test + bye + absent
    // bh points: 3     + 7        + 6          + 3   + 0 = 19 points
    flipello1Tourney.fideBuchholz(flipello1.jheps) must_== 19
  }
  "statmatt2 results" should {
    flipello1Tourney.score(flipello1.statmatt2) must_== 6.0
    // played:   absent + james_test + lakin2 + stamatt + lakin
    // bh points: 0     + 9          + 3      + 2       + 3 = 17 points
    flipello1Tourney.fideBuchholz(flipello1.statmatt2) must_== 17
  }
  "lakin results" should {
    flipello1Tourney.score(flipello1.lakin) must_== 4.0
    // played:   jheps + bye + statmatt + james_test + statmatt2
    // bh points: 6    + 3   + 2        + 9          + 7 = 27 points
    flipello1Tourney.fideBuchholz(flipello1.lakin) must_== 27
  }
  "statmatt results" should {
    flipello1Tourney.score(flipello1.statmatt) must_== 3.0
    // played:   bye + jheps + lakin + statmatt2 + james_test
    // bh points: 4  + 6     + 3     + 7         + 9 = 29 points
    flipello1Tourney.fideBuchholz(flipello1.statmatt) must_== 29
  }
  "lakin2 results" should {
    flipello1Tourney.score(flipello1.lakin2) must_== 0.0
    // played:   james_test + absent + statmatt2 + absent + absent
    // bh points: 9         + 0      + 7         + 0      + 0 = 16 points
    flipello1Tourney.fideBuchholz(flipello1.lakin2) must_== 16
  }
}
