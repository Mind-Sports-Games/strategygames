package strategygames.bench

import java.nio.charset.StandardCharsets
import java.nio.file.{ Files, Paths, StandardOpenOption }
import java.time.Instant

import cats.data.Validated

import strategygames.{ ActionStrs, Player }
import strategygames.go.Situation
import strategygames.go.variant.{ Variant => GoVariant }
import strategygames.go.{ Replay => GoReplay }

/** A wall-clock check of the go rules, for when a JMH run costs more time than the question is worth. Median
  * of a few rounds per workload over the corpus fixtures; it prints a table and appends a row per workload
  * and size to a CSV.
  *
  * Numbers from here are indicative, not a benchmark result — `GoRulesBenchmark` is the measure of record.
  */
object GoSmokeTiming {

  private val warmupRounds     = 2
  private val timedRounds      = 5
  private val samplesPerSize   = 7
  private val sampleRangeStart = 0.15
  private val sampleRangeEnd   = 0.80

  private val defaultOutputPath = "bench/target/go-smoke-results.csv"

  def main(args: Array[String]): Unit = {
    val outputPath = sys.props.get("smoke.output").orElse(args.headOption).getOrElse(defaultOutputPath)
    val startedAt  = System.nanoTime()

    val corpora = GoBoardSize.all.map(size => GoCorpusGame.load(size.key))

    val replayTimings  = corpora.map(corpus => corpus.size.key -> measureReplay(corpus))
    val movegenTimings = corpora.map(corpus => corpus.size.key -> measureMovegen(corpus))

    printTable("full-game replay (go.Replay.gameFromUciStrings)", replayTimings)
    printTable("legal-drop generation (go.Situation.dropsAsDrops)", movegenTimings)

    appendCsv(outputPath, replayTimings, movegenTimings)

    val elapsedMs = (System.nanoTime() - startedAt) / 1000000L
    println(s"\nharness wall time: ${elapsedMs} ms")
    println(s"results appended to: ${outputPath}")
  }

  private def median(samples: Seq[Long]): Long = {
    val sorted = samples.sorted
    sorted(sorted.size / 2)
  }

  private def replayOnce(corpus: GoCorpusGame, variant: GoVariant, activePlayer: Player): Int =
    GoReplay.gameFromUciStrings(corpus.actionStrs, activePlayer, corpus.initialFen, variant) match {
      case Validated.Valid(game)    => game.plies
      case Validated.Invalid(error) =>
        sys.error(s"replay failed for ${variant.key}: ${error}")
    }

  private def timeReplay(corpus: GoCorpusGame, variant: GoVariant, activePlayer: Player): Long = {
    var checksum = 0
    val samples  = Array.fill(warmupRounds + timedRounds) {
      val t0 = System.nanoTime()
      checksum += replayOnce(corpus, variant, activePlayer)
      System.nanoTime() - t0
    }
    if (checksum <= 0) sys.error("unreachable: replay produced no plies")
    median(samples.drop(warmupRounds).toIndexedSeq)
  }

  private def measureReplay(corpus: GoCorpusGame): Long = {
    val activePlayer = corpus.activePlayerAfter(corpus.actionStrs.size)
    timeReplay(corpus, corpus.size.variant, activePlayer)
  }

  private def sampleTurnCounts(totalTurns: Int, count: Int): Vector[Int] = {
    val lo = math.max(1, (totalTurns * sampleRangeStart).toInt)
    val hi = math.max(lo + 1, (totalTurns * sampleRangeEnd).toInt)
    if (hi <= lo) Vector(math.min(lo, totalTurns))
    else {
      val step = math.max(1, (hi - lo) / math.max(1, count - 1))
      (0 until count).map(i => math.min(hi, lo + i * step)).distinct.toVector
    }
  }

  private def situationAfter(corpus: GoCorpusGame, variant: GoVariant, turns: ActionStrs): Situation =
    GoCorpusGame.replay(corpus, variant, turns).situation

  private def timeValidDropsAt(situation: Situation): Long = {
    var checksum = 0
    val samples  = Array.fill(warmupRounds + timedRounds) {
      val t0 = System.nanoTime()
      checksum += situation.dropsAsDrops.size
      System.nanoTime() - t0
    }
    if (checksum < 0) sys.error("unreachable: negative valid drop count")
    median(samples.drop(warmupRounds).toIndexedSeq)
  }

  private def measureMovegen(corpus: GoCorpusGame): Long = {
    val situations = sampleTurnCounts(corpus.actionStrs.size, samplesPerSize)
      .map(turnCount => situationAfter(corpus, corpus.size.variant, corpus.actionStrs.take(turnCount)))
    median(situations.map(timeValidDropsAt))
  }

  private def printTable(title: String, rows: List[(String, Long)]): Unit = {
    println(s"\n=== ${title} ===")
    println(f"${"size"}%-12s ${"ns/op"}%14s")
    rows.foreach { case (label, ns) =>
      println(f"${label}%-12s ${ns}%14d")
    }
  }

  private def appendCsv(
      outputPath: String,
      replayTimings: List[(String, Long)],
      movegenTimings: List[(String, Long)]
  ): Unit = {
    val path      = Paths.get(outputPath)
    Option(path.getParent).foreach(Files.createDirectories(_))
    val isNewFile = !Files.exists(path)
    // NOTE: a BufferedWriter rather than a PrintWriter, which swallows every IO error and would
    // leave `main` announcing results it never wrote.
    val writer    = Files.newBufferedWriter(
      path,
      StandardCharsets.UTF_8,
      StandardOpenOption.CREATE,
      StandardOpenOption.APPEND
    )
    try {
      val timestamp                                                     = Instant.now().toString
      def writeRow(fields: String): Unit                                = {
        writer.write(fields)
        writer.newLine()
      }
      def writeRows(workload: String, rows: List[(String, Long)]): Unit =
        rows.foreach { case (label, ns) =>
          writeRow(s"${timestamp},${workload},${label},${ns}")
        }
      if (isNewFile) writeRow("timestamp,workload,size,ns_per_op")
      writeRows("replay", replayTimings)
      writeRows("movegen", movegenTimings)
    } finally writer.close()
  }
}
