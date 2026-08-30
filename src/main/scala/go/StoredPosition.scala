package strategygames.go

import strategygames.go.format.FEN

// NOTE: lila stores a go game as the fen it started from and the uci moves played since, and has no
// column for komi, the ko point, the pass run or the settlement flag. This is that stored pair, and
// `Board.apply` works the missing state back out of it. See `docs/go-refactor.md`.
final case class StoredPosition(initialFen: FEN, uciMoves: List[String])

// NOTE: lila's stored-game reader calls `strategygames.go.Api.positionFromStartingFenAndMoves` by
// that name, in `modules/game/src/main/BSONHandlers.scala`. The name is all this object is for.
// TODO(lila): call `StoredPosition` directly, or persist the position state and drop both.
object Api {

  def positionFromStartingFenAndMoves(startingFen: FEN, uciMoves: List[String]): StoredPosition =
    StoredPosition(startingFen, uciMoves)

}
