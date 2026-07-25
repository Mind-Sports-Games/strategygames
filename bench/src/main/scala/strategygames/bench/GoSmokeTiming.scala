package strategygames.bench

import java.io.PrintWriter
import java.nio.charset.StandardCharsets
import java.nio.file.{ Files, Paths, StandardOpenOption }
import java.time.Instant

import cats.data.Validated

import strategygames.{ ActionStrs, Player }
import strategygames.go.Api
import strategygames.go.format.FEN
import strategygames.go.variant.{
  Go13x13,
  Go13x13Scala,
  Go19x19,
  Go19x19Scala,
  Go9x9,
  Go9x9Scala,
  Variant => GoVariant
}
import strategygames.go.{ Replay => GoReplay }

object GoSmokeTiming {

  private val warmupRounds     = 2
  private val timedRounds      = 5
  private val samplesPerSize   = 7
  private val sampleRangeStart = 0.15
  private val sampleRangeEnd   = 0.80

  private val defaultOutputPath =
    "/tmp/claude-1000/-home-lakin-work-repos-playstrategy-strategygames" +
      "/5b3959c1-58f7-4996-9c63-e964e8e25240/scratchpad/overnight/smoke-results.csv"

  private case class SizeFixture(
      label: String,
      oldVariant: GoVariant,
      newVariant: GoVariant,
      initialFen: Option[FEN],
      actionStrs: ActionStrs,
      moves: Vector[String]
  )

  private case class Timing(oldNs: Long, newNs: Long) {
    def speedup: Double = oldNs.toDouble / newNs.toDouble
  }

  private val corpora: List[(String, String, GoVariant, GoVariant)] = List(
    ("go-go9x9", "long", Go9x9, Go9x9Scala),
    ("go-go13x13", "long", Go13x13, Go13x13Scala),
    ("go", "long", Go19x19, Go19x19Scala)
  )

  def main(args: Array[String]): Unit = {
    val outputPath = sys.props.get("smoke.output").orElse(args.headOption).getOrElse(defaultOutputPath)
    val startedAt  = System.nanoTime()

    val fixtures = corpora.map { case (family, size, oldVariant, newVariant) =>
      val fixture = CorpusFixture.load(family, size)
      SizeFixture(
        label = oldVariant.key,
        oldVariant = oldVariant,
        newVariant = newVariant,
        initialFen = fixture.initialFen.map(_.toGo),
        actionStrs = fixture.actionStrs,
        moves = fixture.actionStrs.flatten.toVector
      )
    }

    val replayTimings  = fixtures.map(f => f.label -> measureReplay(f))
    val movegenTimings = fixtures.map(f => f.label -> measureMovegen(f))

    printTable("full-game replay (go.Replay.gameFromUciStrings)", replayTimings)
    printTable("legal-move generation (Api.Position.legalDrops)", movegenTimings)

    appendCsv(outputPath, replayTimings, movegenTimings)

    val elapsedMs = (System.nanoTime() - startedAt) / 1000000L
    println(s"\nharness wall time: ${elapsedMs} ms")
    println(s"results appended to: ${outputPath}")
  }

  private def median(samples: Seq[Long]): Long = {
    val sorted = samples.sorted
    sorted(sorted.size / 2)
  }

  private def replayOnce(fixture: SizeFixture, variant: GoVariant, activePlayer: Player): Int = {
    GoReplay.gameFromUciStrings(fixture.actionStrs, activePlayer, fixture.initialFen, variant) match {
      case Validated.Valid(game)    => game.plies
      case Validated.Invalid(error) =>
        sys.error(s"replay failed for ${variant.key}: ${error}")
    }
  }

  private def timeReplay(fixture: SizeFixture, variant: GoVariant, activePlayer: Player): Long = {
    var checksum = 0
    val samples  = Array.fill(warmupRounds + timedRounds) {
      val t0 = System.nanoTime()
      checksum += replayOnce(fixture, variant, activePlayer)
      System.nanoTime() - t0
    }
    if (checksum <= 0) sys.error("unreachable: replay produced no plies")
    median(samples.drop(warmupRounds).toIndexedSeq)
  }

  private def measureReplay(fixture: SizeFixture): Timing = {
    val fen          = fixture.initialFen.getOrElse(fixture.oldVariant.initialFen)
    val startPlayer  = fen.player.getOrElse(Player.P1)
    val activePlayer = Player.fromTurnCount(fixture.actionStrs.size + startPlayer.fold(0, 1))
    Timing(
      oldNs = timeReplay(fixture, fixture.oldVariant, activePlayer),
      newNs = timeReplay(fixture, fixture.newVariant, activePlayer)
    )
  }

  private def sampleIndices(totalMoves: Int, count: Int): Vector[Int] = {
    val lo = math.max(1, (totalMoves * sampleRangeStart).toInt)
    val hi = math.max(lo + 1, (totalMoves * sampleRangeEnd).toInt)
    if (hi <= lo) Vector(math.min(lo, totalMoves))
    else {
      val step = math.max(1, (hi - lo) / math.max(1, count - 1))
      (0 until count).map(i => math.min(hi, lo + i * step)).distinct.toVector
    }
  }

  private def buildPosition(fixture: SizeFixture, variant: GoVariant, prefix: List[String]): Api.Position =
    fixture.initialFen match {
      case Some(fen) => Api.positionFromVariantStartingFenAndMoves(variant, fen, prefix)
      case None      => Api.positionFromVariantAndMoves(variant, prefix)
    }

  private def timeLegalDropsAt(fixture: SizeFixture, variant: GoVariant, prefix: List[String]): Long = {
    var checksum = 0
    val samples  = Array.fill(warmupRounds + timedRounds) {
      val position = buildPosition(fixture, variant, prefix)
      val t0       = System.nanoTime()
      checksum += position.legalDrops.length
      System.nanoTime() - t0
    }
    if (checksum < 0) sys.error("unreachable: negative legalDrops count")
    median(samples.drop(warmupRounds).toIndexedSeq)
  }

  private def measureMovegen(fixture: SizeFixture): Timing = {
    val indices  = sampleIndices(fixture.moves.size, samplesPerSize)
    val prefixes = indices.map(i => fixture.moves.take(i).toList)

    val oldSamples = prefixes.map(prefix => timeLegalDropsAt(fixture, fixture.oldVariant, prefix))
    val newSamples = prefixes.map(prefix => timeLegalDropsAt(fixture, fixture.newVariant, prefix))

    Timing(oldNs = median(oldSamples), newNs = median(newSamples))
  }

  private def printTable(title: String, rows: List[(String, Timing)]): Unit = {
    println(s"\n=== ${title} ===")
    println(f"${"size"}%-12s ${"old ns/op"}%14s ${"new ns/op"}%14s ${"speedup"}%10s")
    rows.foreach { case (label, timing) =>
      println(
        f"${label}%-12s ${timing.oldNs}%14d ${timing.newNs}%14d ${timing.speedup}%9.2fx"
      )
    }
  }

  private def appendCsv(
      outputPath: String,
      replayTimings: List[(String, Timing)],
      movegenTimings: List[(String, Timing)]
  ): Unit = {
    val path      = Paths.get(outputPath)
    Option(path.getParent).foreach(Files.createDirectories(_))
    val isNewFile = !Files.exists(path)
    val writer    = new PrintWriter(
      Files.newBufferedWriter(
        path,
        StandardCharsets.UTF_8,
        StandardOpenOption.CREATE,
        StandardOpenOption.APPEND
      )
    )
    try {
      if (isNewFile) writer.println("timestamp,workload,size,old_ns_per_op,new_ns_per_op,speedup")
      val timestamp                                                       = Instant.now().toString
      def writeRows(workload: String, rows: List[(String, Timing)]): Unit =
        rows.foreach { case (label, timing) =>
          writer.println(
            s"${timestamp},${workload},${label},${timing.oldNs},${timing.newNs},${"%.4f".format(timing.speedup)}"
          )
        }
      writeRows("replay", replayTimings)
      writeRows("movegen", movegenTimings)
    } finally writer.close()
  }
}
