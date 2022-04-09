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
          win(derarzt, matt, 0),
          win(derarzt, ayala, 1),
          win(derarzt, rapha, 2),
          win(derarzt, berserk, 3),
          win(derarzt, irina, 4),
          win(derarzt, mega, 5),
          win(derarzt, fun, 6)
        )
      case "Ayala7" =>
        List(
          win(ayala, manka, 0),
          lose(ayala, derarzt, 1),
          win(ayala, mega, 2),
          win(ayala, rapha, 3),
          win(ayala, berserk, 4),
          win(ayala, irina, 5),
          bye(ayala, 6)
        )
      case "raphael20odrich" =>
        List(
          win(rapha, mega, 0),
          win(rapha, irina, 1),
          lose(rapha, derarzt, 2),
          lose(rapha, ayala, 3),
          win(rapha, manka, 4),
          win(rapha, matt, 5),
          win(rapha, berserk, 6)
        )
      case "Irina" =>
        List(
          win(irina, berserk, 0),
          lose(irina, rapha, 1),
          win(irina, fun, 2),
          win(irina, matt, 3),
          lose(irina, derarzt, 4),
          lose(irina, ayala, 5),
          win(irina, mega, 6)
        )
      case "berserker" =>
        List(
          lose(berserk, irina, 0),
          bye(berserk, 1),
          win(berserk, matt, 2),
          lose(berserk, derarzt, 3),
          lose(berserk, ayala, 4),
          win(berserk, manka, 5),
          lose(berserk, rapha, 6)
        )
      case "megamau" =>
        List(
          lose(mega, rapha, 0),
          win(mega, manka, 1),
          lose(mega, ayala, 2),
          bye(mega, 3),
          win(mega, fun, 4),
          lose(mega, derarzt, 5),
          lose(mega, irina, 6)
        )
      case "statmatt" =>
        List(
          lose(matt, derarzt, 0),
          win(matt, fun, 1),
          lose(matt, berserk, 2),
          lose(matt, irina, 3),
          bye(matt, 4),
          lose(matt, rapha, 5),
          win(matt, manka, 6)
        )
      case "mankalacz" =>
        List(
          lose(manka, ayala, 0),
          lose(manka, mega, 1),
          bye(manka, 2),
          win(manka, fun, 3),
          lose(manka, rapha, 4),
          lose(manka, berserk, 5),
          lose(manka, matt, 6)
        )
      case "FunWithFlags" =>
        List(
          presentWin(fun, egor, 0),
          lose(fun, matt, 1),
          lose(fun, irina, 2),
          lose(fun, manka, 3),
          lose(fun, mega, 4),
          bye(fun, 5),
          lose(fun, derarzt, 6)
        )
      case "egormmaksymov341" =>
        List(
          absentLoss(egor, fun, 0),
          withdrawn(egor, 1),
          withdrawn(egor, 2),
          withdrawn(egor, 3),
          withdrawn(egor, 4),
          withdrawn(egor, 5),
          withdrawn(egor, 6)
        )
    }
  List()
}

class TiebreakTests extends Specification with ValidatedMatchers {
  val draughts = new TournamentEY1k6otN();
  val tourney  = new Tiebreak(draughts);
  "derarzt results" should {
    tourney.score(draughts.derarzt) must_== 14
    tourney.sonnenbornBerger(draughts.derarzt) must_== 52
    tourney.buccholz(draughts.derarzt) must_== 52
  }
  "ayala results" should {
    tourney.score(draughts.ayala) must_== 12
    tourney.sonnenbornBerger(draughts.ayala) must_== 34
    tourney.buccholz(draughts.ayala) must_== 58
  }
  "rapha results" should {
    tourney.score(draughts.rapha) must_== 10
    tourney.sonnenbornBerger(draughts.rapha) must_== 30
    tourney.buccholz(draughts.rapha) must_== 56
  }
  "irina results" should {
    tourney.score(draughts.irina) must_== 8
    tourney.sonnenbornBerger(draughts.irina) must_== 22
    tourney.buccholz(draughts.irina) must_== 58
  }
  "berserker results" should {
    tourney.score(draughts.berserk) must_== 6
    tourney.sonnenbornBerger(draughts.berserk) must_== 10
    tourney.buccholz(draughts.berserk) must_== 59
  }
  "megamau results" should {
    tourney.score(draughts.mega) must_== 6
    tourney.sonnenbornBerger(draughts.mega) must_== 8
    tourney.buccholz(draughts.mega) must_== 57
  }
  "statmatt results" should {
    tourney.score(draughts.matt) must_== 6
    tourney.sonnenbornBerger(draughts.matt) must_== 8
    tourney.buccholz(draughts.matt) must_== 50
  }
  "mankalacz results" should {
    tourney.score(draughts.manka) must_== 4
    tourney.sonnenbornBerger(draughts.manka) must_== 4
    tourney.buccholz(draughts.manka) must_== 48
  }
  "funwithflags results" should {
    tourney.score(draughts.fun) must_== 4
    tourney.sonnenbornBerger(draughts.fun) must_== 0
    tourney.buccholz(draughts.fun) must_== 41
  }
  "egormmaksymov341 results" should {
    tourney.score(draughts.egor) must_== 0
    tourney.sonnenbornBerger(draughts.egor) must_== 0
    tourney.buccholz(draughts.egor) must_== 0
  }
}
