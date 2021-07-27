package strategygames

import cats.data.Validated
import cats.implicits._

import variant.Variant
import format.{ FEN, Uci }

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

  def apply(lib: GameLib, setup: Game, moves: List[MoveOrDrop], state: Game): Replay =
    (lib, setup, state) match {
      case (GameLib.Draughts(), Game.Draughts(setup), Game.Draughts(state))
        => Draughts(draughts.Replay(setup, moves.map(Move.toDraughts), state))
      case (GameLib.Chess(), Game.Chess(setup), Game.Chess(state))
        => Chess(chess.Replay(setup, moves.map(Move.toChess), state))
      case _ => sys.error("Mismatched gamelib types")
    }

  def apply(game: Game) = new Replay(game, Nil, game)

  def games(
    lib: GameLib,
    moveStrs: Iterable[String],
    initialFen: Option[FEN],
    variant: Variant
  ): Validated[String, List[Game]] = (lib, initialFen, variant) match {
    case (GameLib.Draughts(), Some(FEN.Draughts(initialFen)), Variant.Draughts(variant))
      => draughts.Replay.games(moveStrs, Some(initialFen), variant).toEither.map(
        g => g.map(Game.Draughts)
      ).toValidated
    case (GameLib.Chess(), Some(FEN.Chess(initialFen)), Variant.Chess(variant))
      => chess.Replay.games(moveStrs, Some(initialFen), variant).toEither.map(
        g => g.map(Game.Chess)
      ).toValidated
    case _ => sys.error("Mismatched gamelib types")
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
    case _ => sys.error("Mismatched gamelib types")
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
  ): Validated[String, List[Situation]] = (lib, initialFen, variant) match {
    case (GameLib.Draughts(), Some(FEN.Draughts(initialFen)), Variant.Draughts(variant))
      => draughts.Replay.situations(moveStrs, Some(initialFen), variant, finalSquare)
        .toEither
        .map(s => s.map(Situation.Draughts))
        .toValidated
    case (GameLib.Chess(), Some(FEN.Chess(initialFen)), Variant.Chess(variant))
      => chess.Replay.situations(moveStrs, Some(initialFen), variant)
        .toEither
        .map(s => s.map(Situation.Chess))
        .toValidated
    case _ => sys.error("Mismatched gamelib types")
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
    variant: Variant
  ): Validated[String, List[Board]] = (lib, initialFen, variant) match {
    case (GameLib.Draughts(), Some(FEN.Draughts(initialFen)), Variant.Draughts(variant))
      => draughts.Replay.boardsFromUci(draughtsUcis(moves), Some(initialFen), variant)
        .toEither
        .map(b => b.map(Board.Draughts))
        .toValidated
    case (GameLib.Chess(), Some(FEN.Chess(initialFen)), Variant.Chess(variant))
      => chess.Replay.boardsFromUci(chessUcis(moves), Some(initialFen), variant)
        .toEither
        .map(b => b.map(Board.Chess))
        .toValidated
    case _ => sys.error("Mismatched gamelib types")
  }

  def situationsFromUci(
    lib: GameLib,
    moves: List[Uci],
    initialFen: Option[FEN],
    variant: Variant,
    finalSquare: Boolean = false
  ): Validated[String, List[Situation]] = (lib, initialFen, variant) match {
    case (GameLib.Draughts(), Some(FEN.Draughts(initialFen)), Variant.Draughts(variant))
      => draughts.Replay.situationsFromUci(draughtsUcis(moves), Some(initialFen), variant, finalSquare)
        .toEither
        .map(s => s.map(Situation.Draughts))
        .toValidated
    case (GameLib.Chess(), Some(FEN.Chess(initialFen)), Variant.Chess(variant))
      => chess.Replay.situationsFromUci(chessUcis(moves), Some(initialFen), variant)
        .toEither
        .map(s => s.map(Situation.Chess))
        .toValidated
    case _ => sys.error("Mismatched gamelib types")
  }

  def apply(
    lib: GameLib,
    moves: List[Uci],
    initialFen: Option[FEN],
    variant: Variant,
    finalSquare: Boolean = false
  ): Validated[String, Replay] = (lib, initialFen, variant) match {
    case (GameLib.Draughts(), Some(FEN.Draughts(initialFen)), Variant.Draughts(variant))
      => draughts.Replay.apply(draughtsUcis(moves), Some(initialFen), variant, finalSquare)
        .toEither
        .map(r => Replay.Draughts(r))
        .toValidated
    case (GameLib.Chess(), Some(FEN.Chess(initialFen)), Variant.Chess(variant))
      => chess.Replay.apply(chessUcis(moves), Some(initialFen), variant)
        .toEither
        .map(r => Replay.Chess(r))
        .toValidated
    case _ => sys.error("Mismatched gamelib types")
  }

  def plyAtFen(
    lib: GameLib,
    moveStrs: Iterable[String],
    initialFen: Option[FEN],
    variant: Variant,
    atFen: FEN
  ): Validated[String, Int] = (lib, initialFen, variant, atFen) match {
    case (GameLib.Draughts(), Some(FEN.Draughts(initialFen)), Variant.Draughts(variant), FEN.Draughts(atFen))
      => draughts.Replay.plyAtFen(moveStrs, Some(initialFen), variant, atFen)
    case (GameLib.Chess(), Some(FEN.Chess(initialFen)), Variant.Chess(variant), FEN.Chess(atFen))
      => chess.Replay.plyAtFen(moveStrs, Some(initialFen), variant, atFen)
    case _ => sys.error("Mismatched gamelib types")
  }

}
