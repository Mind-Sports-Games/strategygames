package strategygames.bench

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole

import strategygames.Player
import strategygames.go.{ Board, Hash, History, Piece, PieceMap, Pos, Situation, Stone }

/** Measures the cost of `List.size` on the `Pos.all` lookup tables versus a cached val.
  *
  * `Pos.all` is a `val`, so the list is built once -- but `scala.collection.immutable.::` stores its tail in a
  * NON-final field (`private List next` with a `next_$eq` setter, used by the stdlib's list builders). The JIT
  * therefore cannot treat the spine as constant and must re-walk it on every `.size` call.
  *
  * `vectorSize` is the control: `Vector.length` reads a plain int field, so if the harness were simply failing
  * to warm up, that case would look slow too.
  */
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 5, time = 1)
@Fork(2)
class AllSizeBenchmark {

  @Param(Array("361"))
  var stones: Int = 361

  var situation: Situation           = null
  var table: Hash.ZobristConstants   = null
  var posVector: Vector[Pos]         = Vector.empty
  var actors: Vector[(Piece, Int)]   = Vector.empty

  @Setup(Level.Trial)
  def setup(): Unit = {
    val variant          = strategygames.go.variant.Go19x19
    val pieces: PieceMap = Pos.all
      .take(stones)
      .zipWithIndex
      .map { case (p, i) => p -> Piece(if (i % 2 == 0) Player.P1 else Player.P2, Stone) }
      .toMap
    situation = Situation(Board(pieces, History(), variant), Player.P1)
    situation.board.actors // force the lazy val outside the measured region
    table = new Hash.ZobristConstants(0)
    posVector = Pos.all.toVector
    actors = situation.board.actors.values.map(a => (a.piece, a.pos.hashCode)).toVector
  }

  private def pieceIndex(piece: Piece): Int =
    piece.role.hashInt * 2 + piece.player.fold(1, 0)

  // ---- the primitive: what does `.size` actually cost? ----

  @Benchmark def sizeViaList(bh: Blackhole): Unit   = bh.consume(Pos.all.size)
  @Benchmark def sizeViaVector(bh: Blackhole): Unit = bh.consume(posVector.size)
  @Benchmark def sizeViaCachedVal(bh: Blackhole): Unit = bh.consume(Pos.allSize)

  // ---- end to end: the real Hash.get fold, both ways ----

  @Benchmark def hashGetWithListSize(bh: Blackhole): Unit =
    bh.consume(
      actors.view
        .map { case (piece, posHash) => table.actorMasks(Pos.all.size * pieceIndex(piece) + posHash) }
        .fold(0L)(_ ^ _)
    )

  @Benchmark def hashGetWithCachedVal(bh: Blackhole): Unit =
    bh.consume(
      actors.view
        .map { case (piece, posHash) => table.actorMasks(Pos.allSize * pieceIndex(piece) + posHash) }
        .fold(0L)(_ ^ _)
    )

  /** The actual shipped code path. */
  @Benchmark def hashGetReal(bh: Blackhole): Unit = bh.consume(Hash.get(situation, table))
}
