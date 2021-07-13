package strategygames

case class Replay(setup: Game, moves: List[MoveOrDrop], state: Game) {

  lazy val chronoMoves = moves.reverse

  def moveAtPly(ply: Int): Option[MoveOrDrop] =
    chronoMoves lift (ply - 1 - setup.startedAtTurn)

}

//lots of methods not wrapped, due to differences of Traversable/Iterable, and FEN/String
object Replay {

  final case class Chess(r: chess.Replay) extends Replay(
    Game.Chess(r.setup),
    r.moves.map(m => m match {
      case(m: chess.Move) => Move.Chess(m)
      case(m: chess.Drop) => m
    }),
    Game.Chess(r.state)
  ){}

  final case class Draughts(r: draughts.Replay) extends Replay(
    Game.Draughts(r.setup),
    r.moves.map(Move.Draughts(_)),
    Game.Chess(r.state)
  ){}

  def apply(game: Game) = new Replay(game, Nil, game)

}
