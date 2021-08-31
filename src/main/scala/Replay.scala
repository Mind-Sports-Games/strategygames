package strategygames

import cats.data.Validated
import cats.implicits._

import variant.Variant
import format.{ FEN, Uci }

sealed abstract class Replay(val setup: Game, val moves: List[MoveOrDrop], val state: Game) {

  lazy val chronoMoves = moves.reverse

  def moveAtPly(ply: Int): Option[MoveOrDrop] =
    chronoMoves lift (ply - 1 - setup.startedAtTurn)

  // TODO: If we had a case class this would be automatic.
  def copy(state: Game): Replay

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
  ){
    def copy(state: Game): Replay = state match {
      case Game.Chess(state) => Replay.wrap(r.copy(state=state))
      case _ => sys.error("Unable to copy a chess replay with a non-chess state")
    }
  }

  final case class Draughts(r: draughts.Replay) extends Replay(
    Game.Draughts(r.setup),
    r.moves.map((m: draughts.Move) => Left(Move.Draughts(m))),
    Game.Draughts(r.state)
  ){
    def copy(state: Game): Replay = state match {
      case Game.Draughts(state) => Replay.wrap(r.copy(state=state))
      case _ => sys.error("Unable to copy a draughts replay with a non-draughts state")
    }
  }

  def apply(lib: GameLib, setup: Game, moves: List[MoveOrDrop], state: Game): Replay =
    (lib, setup, state) match {
      case (GameLib.Draughts(), Game.Draughts(setup), Game.Draughts(state))
        => Draughts(draughts.Replay(setup, moves.map(Move.toDraughts), state))
      case (GameLib.Chess(), Game.Chess(setup), Game.Chess(state))
        => Chess(chess.Replay(setup, moves.map(Move.toChess), state))
      case _ => sys.error("Mismatched gamelib types 5")
    }

  // TODO: I don't think this is quite correct, let's see if it's used.
  // def apply(game: Game) = new Replay(game, Nil, game)

  def games(
    lib: GameLib,
    moveStrs: Iterable[String],
    initialFen: Option[FEN],
    variant: Variant
  ): Validated[String, List[Game]] = (lib, variant) match {
    case (GameLib.Draughts(), Variant.Draughts(variant))
      => draughts.Replay.games(moveStrs, initialFen.map(_.toDraughts), variant).toEither.map(
        g => g.map(Game.Draughts)
      ).toValidated
    case (GameLib.Chess(), Variant.Chess(variant))
      => chess.Replay.games(moveStrs, initialFen.map(_.toChess), variant).toEither.map(
        g => g.map(Game.Chess)
      ).toValidated
    case _ => sys.error("Mismatched gamelib types 6")
  }

  def gameMoveWhileValid(
    lib: GameLib,
    moveStrs: Seq[String],
    initialFen: FEN,
    variant: Variant,
    iteratedCapts: Boolean = false
  ): (Game, List[(Game, Uci.WithSan)], Option[String]) = (lib, initialFen, variant) match {
    case (GameLib.Draughts(), FEN.Draughts(initialFen), Variant.Draughts(variant)) =>
      draughts.Replay.gameMoveWhileValid(moveStrs, initialFen, variant, iteratedCapts) match {
        case (game, gameswithsan, message) =>
          (
            Game.Draughts(game),
            gameswithsan.map { case (g, u) => (Game.Draughts(g), Uci.DraughtsWithSan(u)) },
            message
          )
      }
    case (GameLib.Chess(), FEN.Chess(initialFen), Variant.Chess(variant)) =>
      chess.Replay.gameMoveWhileValid(moveStrs, initialFen, variant) match {
        case (game, gameswithsan, message) =>
          (
            Game.Chess(game),
            gameswithsan.map { case (g, u) => (Game.Chess(g), Uci.ChessWithSan(u)) },
            message
          )
      }
    case _ => sys.error("Mismatched gamelib types 7")
  }

  def boards(
    lib: GameLib,
    moveStrs: Iterable[String],
    initialFen: Option[FEN],
    variant: Variant,
    finalSquare: Boolean = false
  ): Validated[String, List[Board]] =
    situations(lib, moveStrs, initialFen, variant, finalSquare) map (_ map (_.board))

  def situations(
    lib: GameLib,
    moveStrs: Iterable[String],
    initialFen: Option[FEN],
    variant: Variant,
    finalSquare: Boolean = false
  ): Validated[String, List[Situation]] = (lib, variant) match {
    case (GameLib.Draughts(), Variant.Draughts(variant)) =>
      draughts.Replay.situations(moveStrs, initialFen.map(_.toDraughts), variant, finalSquare)
        .toEither
        .map(s => s.map(Situation.Draughts))
        .toValidated
    case (GameLib.Chess(), Variant.Chess(variant)) =>
      chess.Replay.situations(moveStrs, initialFen.map(_.toChess), variant)
        .toEither
        .map(s => s.map(Situation.Chess))
        .toValidated
    case _ => sys.error("Mismatched gamelib types 8")
  }

  def draughtsUcis(ucis: List[Uci]): List[draughts.format.Uci] =
    ucis.flatMap(u =>
      u match {
        case u: Uci.Draughts => Some(u.unwrap)
        case _               => None
      }
    )

  def chessUcis(ucis: List[Uci]): List[chess.format.Uci] =
    ucis.flatMap(u =>
      u match {
        case u: Uci.Chess => Some(u.unwrap)
        case _            => None
      }
    )

  def boardsFromUci(
    lib: GameLib,
    moves: List[Uci],
    initialFen: Option[FEN],
    variant: Variant,
    finalSquare: Boolean = false
  ): Validated[String, List[Board]] = (lib, variant) match {
    case (GameLib.Draughts(), Variant.Draughts(variant)) =>
      draughts.Replay.boardsFromUci(
        draughtsUcis(moves),
        initialFen.map(_.toDraughts),
        variant,
        finalSquare
      ).toEither
        .map(b => b.map(Board.Draughts))
        .toValidated
    case (GameLib.Chess(), Variant.Chess(variant))
      => chess.Replay.boardsFromUci(chessUcis(moves), initialFen.map(_.toChess), variant)
        .toEither
        .map(b => b.map(Board.Chess))
        .toValidated
    case _ => sys.error("Mismatched gamelib types 8a")
  }

  def situationsFromUci(
    lib: GameLib,
    moves: List[Uci],
    initialFen: Option[FEN],
    variant: Variant,
    finalSquare: Boolean = false
  ): Validated[String, List[Situation]] = (lib, variant) match {
    case (GameLib.Draughts(), Variant.Draughts(variant)) =>
      draughts.Replay.situationsFromUci(draughtsUcis(moves), initialFen.map(_.toDraughts), variant, finalSquare)
        .toEither
        .map(s => s.map(Situation.Draughts))
        .toValidated
    case (GameLib.Chess(), Variant.Chess(variant)) =>
      chess.Replay.situationsFromUci(chessUcis(moves), initialFen.map(_.toChess), variant)
        .toEither
        .map(s => s.map(Situation.Chess))
        .toValidated
    case _ => sys.error("Mismatched gamelib types 9")
  }

  def apply(
    lib: GameLib,
    moves: List[Uci],
    initialFen: Option[FEN],
    variant: Variant,
    finalSquare: Boolean = false
  ): Validated[String, Replay] = (lib, variant) match {
    case (GameLib.Draughts(), Variant.Draughts(variant)) =>
      draughts.Replay.apply(draughtsUcis(moves), initialFen.map(_.toDraughts), variant, finalSquare)
        .toEither
        .map(r => Replay.Draughts(r))
        .toValidated
    case (GameLib.Chess(), Variant.Chess(variant)) =>
      chess.Replay.apply(chessUcis(moves), initialFen.map(_.toChess), variant)
        .toEither
        .map(r => Replay.Chess(r))
        .toValidated
    case _ => sys.error("Mismatched gamelib types 10")
  }

  def plyAtFen(
    lib: GameLib,
    moveStrs: Iterable[String],
    initialFen: Option[FEN],
    variant: Variant,
    atFen: FEN
  ): Validated[String, Int] = (lib, variant, atFen) match {
    case (GameLib.Draughts(), Variant.Draughts(variant), FEN.Draughts(atFen))
      => draughts.Replay.plyAtFen(moveStrs, initialFen.map(_.toDraughts), variant, atFen)
    case (GameLib.Chess(), Variant.Chess(variant), FEN.Chess(atFen))
      => chess.Replay.plyAtFen(moveStrs, initialFen.map(_.toChess), variant, atFen)
    case _ => sys.error("Mismatched gamelib types 10")
  }



  def wrap(r: chess.Replay) = Chess(r)
  def wrap(r: draughts.Replay) = Draughts(r)
}
