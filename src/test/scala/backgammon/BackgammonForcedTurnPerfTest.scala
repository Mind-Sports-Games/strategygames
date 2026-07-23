package strategygames.backgammon

import org.specs2.mutable.Specification

/** Guards against the exponential-blowup regression in Situation.validTurns / forcedTurnAction.
  *
  * validTurns enumerates the whole turn tree, and at every node calls Action.lazySituationAfter,
  * which rebuilds a Situation (and, transitively, regenerates legal moves/lifts/drops for that
  * branch). If lazySituationAfter (or the action lists that feed Situation.actions) regress from
  * a cached lazy val back to a plain def, that same recomputation happens repeatedly per node
  * instead of once, and this test's threshold will start failing well before the numbers get bad
  * enough to matter in production.
  *
  * This is opted out of normal `sbt test` runs the same way the other *PerfTest suites in this
  * repo are - wall-clock thresholds are too flaky across machines (confirmed: this failed on
  * GitHub CI's runners despite passing comfortably locally). Gated on a system property rather
  * than specs2's own `-- skipAll false` CLI override, which doesn't reliably negate an in-code
  * args(skipAll = true) in this specs2 version. Run it explicitly:
  *
  *   sbt -Dbackgammon.perf=true "testOnly strategygames.backgammon.BackgammonForcedTurnPerfTest"
  */
class BackgammonForcedTurnPerfTest extends Specification {

  args(skipAll = sys.props.get("backgammon.perf").isEmpty)

  // doubles create the deepest turn trees (up to 4 sequential actions), so they're the
  // worst case for validTurns' recursive enumeration
  val doubles = List(1, 2, 3, 4, 5, 6)

  private def playOutTurn(g: Game): Game =
    g.situation.moves.values.flatten[Move].headOption match {
      case Some(m: Move) => playOutTurn(g.apply(m))
      case None           => g
    }

  private def endTurnIfPossible(g: Game): Game =
    g.endTurn().toOption.map(_._1).getOrElse(g)

  // the very first roll of the game can't be a double (see Variant.validDiceRolls'
  // isInitialPosition check), so play a full opening turn out first to reach a normal
  // mid-game position, then roll the double we actually want to test on top of that
  def situationAfterDiceRoll(die: Int): Situation = {
    val init: Game       = Game.apply(variant.Backgammon)
    val g0: Game         = init.copy(situation = init.situation.copy(board = init.board.initialiseCube))
    val openingRoll: DiceRoll = g0.situation.diceRolls.headOption
      .getOrElse(sys.error("expected a legal opening dice roll"))
    val g1: Game         = endTurnIfPossible(playOutTurn(g0.applyDiceRoll(openingRoll)))

    val dr: DiceRoll = g1.situation
      .diceRoll(List(die, die))
      .toOption
      .getOrElse(sys.error(s"expected a legal double $die-$die roll after the opening turn"))
    g1.applyDiceRoll(dr).situation
  }

  val warmupIterations = 200
  val timedIterations   = 500

  // Ceiling tuned against measured before/after numbers, not guessed:
  //   post-fix (lazySituationAfter/Situation.actions as lazy val): 0.6-4.0ms avg/call
  //   pre-fix  (same fields as plain def):                         1.6-9.2ms avg/call
  // The doubles that matter most (2-2/3-3/4-4, highest validTurns.size/branching) separate
  // cleanly: post-fix tops out at ~4.0ms, pre-fix bottoms out at ~7.2ms for those three. 6ms
  // sits with ~35% margin below the pre-fix floor and ~50% margin above the post-fix ceiling,
  // so normal machine-to-machine noise shouldn't flip the result either way.
  val maxAvgMicrosPerCall = 6000L

  def timeMicros[A](f: => A): (A, Long) = {
    val start  = System.nanoTime
    val result = f
    val micros = (System.nanoTime - start) / 1000
    (result, micros)
  }

  // forcedTurnAction/validTurns are lazy vals on Situation, so a fresh instance is built per
  // call below - reusing one Situation would just measure a memoized-field read after the
  // first call, not the actual tree-enumeration cost this test exists to catch
  def avgMicrosFor(die: Int): Long = {
    val template          = situationAfterDiceRoll(die)
    def freshSituation()  = Situation(template.board, template.player)

    // warm up the JIT before measuring
    (1 to warmupIterations).foreach(_ => freshSituation().forcedTurnAction)

    val micros    = (1 to timedIterations).map { _ =>
      val (_, us) = timeMicros(freshSituation().forcedTurnAction)
      us
    }
    val avgMicros = micros.sum / timedIterations

    println(
      s"double $die-$die: validTurns.size=${freshSituation().validTurns.size}, " +
        s"avg ${avgMicros}us/call over $timedIterations calls"
    )

    avgMicros
  }

  "forcedTurnAction on the worst-case (doubles) opening roll" should {
    "stay under the average-per-call ceiling for every double" in {
      doubles
        .map(die => (die, avgMicrosFor(die)))
        .map { case (die, avgMicros) =>
          avgMicros aka s"double $die-$die avg microseconds/call" must beLessThan(maxAvgMicrosPerCall)
        }
        .reduce(_ and _)
    }
  }
}
