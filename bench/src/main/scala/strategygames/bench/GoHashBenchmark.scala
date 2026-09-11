package strategygames.bench

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole

import strategygames.Player
import strategygames.go.{ Board, Hash, History, Piece, PieceMap, Pos, Situation, Stone }

/** Zobrist position hashing for Go, across board occupancy.
  *
  * `Hash.get` folds over every actor on the board, so cost scales with the number of stones -- a full 19x19
  * board is the worst case at 361. Parametrised by stone count so a regression shows up as a change in slope,
  * not just in one number.
  *
  * Position hashes are recomputed per position (repetition detection), so this sits on the replay and analysis
  * paths for every Go game.
  */
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 5, time = 1)
@Fork(2)
class GoHashBenchmark {

  @Param(Array("10", "100", "361"))
  var stones: Int = 361

  var situation: Situation         = null
  var table: Hash.ZobristConstants = null

  @Setup(Level.Trial)
  def setup(): Unit = {
    val variant          = strategygames.go.variant.Go19x19
    val pieces: PieceMap = Pos.all
      .take(stones)
      .zipWithIndex
      .map { case (p, i) => p -> Piece(if (i % 2 == 0) Player.P1 else Player.P2, Stone) }
      .toMap
    situation = Situation(Board(pieces, History(), variant, komi = variant.komi), Player.P1)
    situation.board.actors // force the lazy val outside the measured region
    table = new Hash.ZobristConstants(0)
  }

  @Benchmark
  def hashGet(bh: Blackhole): Unit = bh.consume(Hash.get(situation, table))

  @Benchmark
  def hashApply(bh: Blackhole): Unit = bh.consume(Hash(situation))
}
