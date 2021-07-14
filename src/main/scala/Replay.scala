package strategygames

sealed class Replay(val setup: Game, val moves: List[MoveOrDrop], val state: Game) {

  lazy val chronoMoves = moves.reverse

  def moveAtPly(ply: Int): Option[MoveOrDrop] =
    chronoMoves lift (ply - 1 - setup.startedAtTurn)

}

//lots of methods not wrapped, due to differences of Traversable/Iterable, and FEN/String
object Replay {

  final case class Chess(r: chess.Replay) extends Replay(
    Game.Chess(r.setup),
    r.moves.map(m => m match {
      case Left(m)  => Left(Move.Chess(m))
      case Right(m) => Right(m)
    }),
    Game.Chess(r.state)
  ){}

  final case class Draughts(r: draughts.Replay) extends Replay(
    Game.Draughts(r.setup),
    r.moves.map((m: draughts.Move) => Left(Move.Draughts(m))),
    Game.Draughts(r.state)
  ){}

  def apply(game: Game) = new Replay(game, Nil, game)

}
