package strategygames.go.engine

/** A [[GoState]] plus the things scoring and the FEN need but the rules do not: komi, elapsed plies, and
  * whether dead stones have been agreed.
  *
  * Scores come in two units. `p1Score`/`p2Score` are points as a player reads them; `p1FenScore`,
  * `p2FenScore`, `komiTenths` and `gameScore` are tenths of a point, the integer form the FEN format and the
  * wrapper layer speak.
  */
final case class GoGame(
    state: GoState,
    komi: Double,
    plyCount: Int,
    deadStonesSelected: Boolean
) {

  def play(move: Int): GoGame =
    copy(state = state(move), plyCount = plyCount + 1)

  def selectDeadStones(deadStoneMoves: Iterable[Int]): GoGame =
    copy(
      state = state.withoutStones(deadStoneMoves.toSet),
      plyCount = plyCount + 1,
      deadStonesSelected = true
    )

  def withKomi(newKomi: Double): GoGame = copy(komi = newKomi)

  /** Two passes only open dead stone selection; the game is over once the players have settled it, which is
    * what [[selectDeadStones]] records. An empty selection is a legal settlement.
    */
  def ended: Boolean = deadStonesSelected

  def inDeadStoneSelectionPhase: Boolean = state.inDeadStoneSelectionPhase && !deadStonesSelected

  def areaScore: AreaScore = state.areaScore

  def komiTenths: Int = Math.round(komi * 10).toInt

  def p1Score: Double = areaScore.black.toDouble

  def p2Score: Double = areaScore.white + komi

  def p1FenScore: Int = areaScore.black * 10

  def p2FenScore: Int = areaScore.white * 10 + komiTenths

  def gameScore: Int = p1FenScore - p2FenScore

  def gameOutcome: Int =
    if (gameScore > 0) GoGame.BlackWins
    else if (gameScore < 0) GoGame.WhiteWins
    else GoGame.Drawn

  def winningPlayer: Option[Int] =
    Option.unless(gameOutcome == GoGame.Drawn)(Integer.signum(gameOutcome))

  def fullMoveNumber: Int = plyCount / 2 + 1

  def fenPassCount: Int =
    if (deadStonesSelected) GoGame.DeadStonesSelectedPassCount
    else Math.min(state.consecutivePasses, GoGame.HighestOngoingPassCount)
}

object GoGame {

  val BlackWins = 1000
  val WhiteWins = -1000
  val Drawn     = 0

  val DeadStonesSelectedPassCount = 3
  val HighestOngoingPassCount     = 2

  def initial(size: Int, komi: Double): GoGame =
    GoGame(GoState.initial(size), komi, 0, false)
}
