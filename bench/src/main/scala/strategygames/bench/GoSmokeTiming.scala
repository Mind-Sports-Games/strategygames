package strategygames.bench

import java.nio.charset.StandardCharsets
import java.nio.file.{ Files, Paths, StandardOpenOption }
import java.time.Instant

import cats.data.Validated

import strategygames.Player
import strategygames.go.variant.{ Variant => GoVariant }
import strategygames.go.{ Replay => GoReplay }

/** A wall-clock check of the go engine, for when a JMH run costs more time than the question is worth. Median
  * of a few rounds per workload over the corpus fixtures; it prints a table and appends a row per workload
  * and size to a CSV.
  *
  * Numbers from here are indicative, not a benchmark result — `GoEngineBenchmark` is the measure of record.
  */
object GoSmokeTiming {

  private val warmupRounds = 2
  private val timedRounds  = 5

  private val defaultOutputPath = "bench/target/go-smoke-results.csv"

  def main(args: Array[String]): Unit = {
    val outputPath = sys.props.get("smoke.output").orElse(args.headOption).getOrElse(defaultOutputPath)
    val startedAt  = System.nanoTime()

    val corpora = GoBoardSize.all.map(size => GoCorpusGame.load(size.key))

    val replayTimings = corpora.map(corpus => corpus.size.key -> measureReplay(corpus))

    printTable("full-game replay (go.Replay.gameFromUciStrings)", replayTimings)

    appendCsv(outputPath, replayTimings)

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

  private def printTable(title: String, rows: List[(String, Long)]): Unit = {
    println(s"\n=== ${title} ===")
    println(f"${"size"}%-12s ${"ns/op"}%14s")
    rows.foreach { case (label, ns) =>
      println(f"${label}%-12s ${ns}%14d")
    }
  }

  private def appendCsv(outputPath: String, replayTimings: List[(String, Long)]): Unit = {
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
    } finally writer.close()
  }
}
