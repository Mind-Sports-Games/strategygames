package strategygames.go

import org.specs2.mutable.Specification

class GoMoveGenTest extends Specification with GoRulesTestSupport {

  private val runsOnlyWhenAskedFor = sys.props.get("go.movegen").isEmpty

  args(skipAll = runsOnlyWhenAskedFor)

  private def firstDropSetToDiverge(game: StoredGoGame): Option[String] =
    Replay
      .situationsFromUci(game.actions.flatMap(format.Uci(_)), Some(game.initialFen), game.variant)
      .fold(
        refusal => Some(s"${game.id}: ${refusal}"),
        situations =>
          situations
            .map(situation => StoredGoGames.dropSetHash(dropKeysOf(situation)))
            .zip(game.snapshottedDropSetHashes)
            .zipWithIndex
            .collectFirst {
              case ((generated, snapshotted), ply) if generated != snapshotted =>
                s"${game.id} ply ${ply}: generated ${generated}, snapshotted ${snapshotted}"
            }
      )

  "drop generation" should {
    "offer at every ply of every stored game what it offered when the corpus was snapshotted" in {
      forall(StoredGoGames.all) { game => firstDropSetToDiverge(game) must beNone }
    }
  }
}
