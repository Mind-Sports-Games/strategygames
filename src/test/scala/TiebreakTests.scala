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
  val players = List(
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
      case "derarzt" =>
        List(
          Round(0).win(derarzt, matt),
          Round(1).win(derarzt, ayala),
          Round(2).win(derarzt, rapha),
          Round(3).win(derarzt, berserk),
          Round(4).win(derarzt, irina),
          Round(5).win(derarzt, mega),
          Round(6).win(derarzt, fun)
        )
      case "Ayala7" =>
        List(
          Round(0).win(ayala, manka),
          Round(1).lose(ayala, derarzt),
          Round(2).win(ayala, mega),
          Round(3).win(ayala, rapha),
          Round(4).win(ayala, berserk),
          Round(5).win(ayala, irina),
          Round(6).bye(ayala),
        )
      case "raphael20odrich" =>
        List(
          Round(0).win(rapha, mega),
          Round(1).win(rapha, irina),
          Round(2).lose(rapha, derarzt),
          Round(3).lose(rapha, ayala),
          Round(4).win(rapha, manka),
          Round(5).win(rapha, matt),
          Round(6).win(rapha, berserk),
        )
      case "Irina" =>
        List(
          Round(0).win(irina, berserk),
          Round(1).lose(irina, rapha),
          Round(2).win(irina, fun),
          Round(3).win(irina, matt),
          Round(4).lose(irina, derarzt),
          Round(5).lose(irina, ayala),
          Round(6).win(irina, mega),
        )
      case "berserker" =>
        List(
          Round(0).lose(berserk, irina),
          Round(1).bye(berserk),
          Round(2).win(berserk, matt),
          Round(3).lose(berserk, derarzt),
          Round(4).lose(berserk, ayala),
          Round(5).win(berserk, manka),
          Round(6).lose(berserk, rapha),
        )
      case "megamau" =>
        List(
          Round(0).lose(mega, rapha),
          Round(1).win(mega, manka),
          Round(2).lose(mega, ayala),
          Round(3).bye(mega),
          Round(4).win(mega, fun),
          Round(5).lose(mega, derarzt),
          Round(6).lose(mega, irina),
        )
      case "statmatt" =>
        List(
          Round(0).lose(matt, derarzt),
          Round(1).win(matt, fun),
          Round(2).lose(matt, berserk),
          Round(3).lose(matt, irina),
          Round(4).bye(matt),
          Round(5).lose(matt, rapha),
          Round(6).win(matt, manka),
        )
      case "mankalacz" =>
        List(
          Round(0).lose(manka, ayala),
          Round(1).lose(manka, mega),
          Round(2).bye(manka),
          Round(3).win(manka, fun),
          Round(4).lose(manka, rapha),
          Round(5).lose(manka, berserk),
          Round(6).lose(manka, matt),
        )
      case "FunWithFlags" =>
        List(
          Round(0).noOpponentWin(fun, egor),
          Round(1).lose(fun, matt),
          Round(2).lose(fun, irina),
          Round(3).lose(fun, manka),
          Round(4).lose(fun, mega),
          Round(5).bye(fun),
          Round(6).lose(fun, derarzt),
        )
      case "egormmaksymov341" =>
        List(
          Round(0).noShowLoss(egor, fun),
          Round(1).withdrawn(egor),
          Round(2).withdrawn(egor),
          Round(3).withdrawn(egor),
          Round(4).withdrawn(egor),
          Round(5).withdrawn(egor),
          Round(6).withdrawn(egor),
        )
    }
  List()
}

class TiebreakTests extends Specification with ValidatedMatchers {
  val draughts = new TournamentEY1k6otN();
  val tourney  = new Tiebreak(draughts);
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
}
