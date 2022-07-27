package strategygames

import strategygames.variant.Variant

import cats.data.Validated
import cats.implicits._

import strategygames.format.Uci

sealed abstract class Situation(val board: Board, val player: Player) {

  val moves: Map[Pos, List[Move]]

  val destinations: Map[Pos, List[Pos]]

  def drops: Option[List[Pos]]

  def dropsByRole: Option[Map[Role, List[Pos]]]

  def dropsAsDrops: List[Drop] =
    dropsByRole
      .getOrElse(Map())
      .flatMap { case (role, listPos) =>
        // This technically should valid the drops
        // but I'm going to assume they're correct at this point
        // because they came from our move generation.
        // So if they don't validate, we just filter them out here.
        listPos.flatMap(drop(role, _).toOption)
      }
      .toList

  def actions: List[MoveOrDrop] =
    moves.values.flatten.map(Left(_)).toList ::: dropsAsDrops.map(Right(_)).toList

  def history = board.history

  val check: Boolean

  def checkSquare: Option[Pos]

  def checkMate: Boolean = board.variant checkmate this

  def opponentHasInsufficientMaterial: Boolean

  def threefoldRepetition: Boolean

  // only implemented for fairysf for Xiangqi
  lazy val perpetualPossible: Boolean = false

  def end: Boolean

  def winner: Option[Player]

  def playable(strict: Boolean): Boolean

  val status: Option[Status]

  def move(
      from: Pos,
      to: Pos,
      promotion: Option[PromotableRole] = None,
      finalSquare: Boolean = false,
      forbiddenUci: Option[List[String]] = None,
      captures: Option[List[Pos]] = None,
      partialCaptures: Boolean = false
  ): Validated[String, Move]

  def move(uci: Uci.Move): Validated[String, Move]

  def drop(role: Role, pos: Pos): Validated[String, Drop]

  def withVariant(variant: Variant): Situation

  def gameLogic: GameLogic

  // TODO: There is probably a better way to a more generalized version of this function ...
  def copy(board: Board): Situation

  // TODO: yup, still not typesafe
  def toChess: chess.Situation
  def toDraughts: draughts.Situation
  def toFairySF: fairysf.Situation
  def toMancala: mancala.Situation

}

object Situation {

  final case class Chess(s: chess.Situation)
      extends Situation(
        Board.Chess(s.board),
        s.player
      ) {

    lazy val moves: Map[Pos, List[Move]] = s.moves.map { case (p: chess.Pos, l: List[chess.Move]) =>
      (Pos.Chess(p), l.map(Move.Chess))
    }

    lazy val check: Boolean = s.check

    def checkSquare = s.checkSquare.map(Pos.Chess)

    def opponentHasInsufficientMaterial: Boolean = s.opponentHasInsufficientMaterial

    def threefoldRepetition: Boolean = s.threefoldRepetition

    def end: Boolean = s.end

    def winner: Option[Player] = s.winner

    lazy val destinations: Map[Pos, List[Pos]] = s.destinations.map {
      case (p: chess.Pos, l: List[chess.Pos]) => (Pos.Chess(p), l.map(Pos.Chess))
    }

    def drops: Option[List[Pos]] = s.drops.map(_.map(Pos.Chess))

    def dropsByRole: Option[Map[Role, List[Pos]]] = None

    def playable(strict: Boolean): Boolean = s.playable(strict)

    val status: Option[Status] = s.status

    def move(
        from: Pos,
        to: Pos,
        promotion: Option[PromotableRole] = None,
        finalSquare: Boolean = false,
        forbiddenUci: Option[List[String]] = None,
        captures: Option[List[Pos]] = None,
        partialCaptures: Boolean = false
    ): Validated[String, Move] = (from, to) match {
      case (Pos.Chess(from), Pos.Chess(to)) =>
        s.move(from, to, promotion.map(_.toChess)).toEither.map(m => Move.Chess(m)).toValidated
      case _                                => sys.error("Not passed Chess objects")
    }

    def move(uci: Uci.Move): Validated[String, Move] = uci match {
      case Uci.ChessMove(uci) => s.move(uci).toEither.map(m => Move.Chess(m)).toValidated
      case _                  => sys.error("Not passed Chess objects")
    }

    def drop(role: Role, pos: Pos): Validated[String, Drop] = (role, pos) match {
      case (Role.ChessRole(role), Pos.Chess(pos)) =>
        s.drop(role, pos).toEither.map(d => Drop.Chess(d)).toValidated
      case _                                      => sys.error("Not passed Chess objects")
    }

    def withVariant(variant: Variant): Situation = variant match {
      case Variant.Chess(variant) => Chess(s.withVariant(variant))
      case _                      => sys.error("Not passed Chess objects")
    }

    def unary_! : Situation = Chess(s.unary_!)

    def copy(board: Board): Situation = Chess(board match {
      case Board.Chess(board) => s.copy(board)
      case _                  => sys.error("Can't copy a chess situation with a non-chess board")
    })

    def gameLogic: GameLogic = GameLogic.Chess()

    def toChess    = s
    def toDraughts = sys.error("Can't make draughts situation from chess situation")
    def toFairySF  = sys.error("Can't make fairysf situation from chess situation")
    def toMancala  = sys.error("Can't make mancala situation from chess situation")
  }

  final case class Draughts(s: draughts.Situation)
      extends Situation(
        Board.Draughts(s.board),
        s.player
      ) {

    // TODO: DRAUGHTS I think that .validMoves is correct, but unsure. needs testing.
    lazy val moves: Map[Pos, List[Move]] = s.validMoves.map {
      case (p: draughts.Pos, l: List[draughts.Move]) => (Pos.Draughts(p), l.map(Move.Draughts))
    }

    lazy val check: Boolean = false

    def checkSquare = None

    // TODO: this probably needs to be properly implemented
    lazy val destinations: Map[Pos, List[Pos]] = Map()

    def drops: Option[List[Pos]] = None

    def dropsByRole: Option[Map[Role, List[Pos]]] = None

    // possibly need to do something for this
    def opponentHasInsufficientMaterial: Boolean = false

    def threefoldRepetition: Boolean = s.threefoldRepetition

    def end: Boolean = s.end

    def winner: Option[Player] = s.winner

    def playable(strict: Boolean): Boolean = s.playable(strict)

    val status: Option[Status] = s.status

    private def draughtsCaptures(captures: Option[List[Pos]]): Option[List[draughts.Pos]] =
      captures match {
        case Some(captures) =>
          Some(
            captures.flatMap(c =>
              c match {
                case Pos.Draughts(c) => Some(c)
                case _               => None
              }
            )
          )
        case None           => None
      }

    def move(
        from: Pos,
        to: Pos,
        promotion: Option[PromotableRole] = None,
        finalSquare: Boolean = false,
        forbiddenUci: Option[List[String]] = None,
        captures: Option[List[Pos]] = None,
        partialCaptures: Boolean = false
    ): Validated[String, Move] = (from, to) match {
      case (Pos.Draughts(from), Pos.Draughts(to)) =>
        s.move(
          from,
          to,
          promotion.map(_.toDraughts),
          finalSquare,
          forbiddenUci,
          draughtsCaptures(captures),
          partialCaptures
        ).toEither
          .map(m => Move.Draughts(m))
          .toValidated
      case _                                      => sys.error("Not passed Draughts objects")
    }

    def move(uci: Uci.Move): Validated[String, Move] = uci match {
      case Uci.DraughtsMove(uci) => s.move(uci).toEither.map(m => Move.Draughts(m)).toValidated
      case _                     => sys.error("Not passed Draughts objects")
    }

    def drop(role: Role, pos: Pos): Validated[String, Drop] =
      sys.error("Can't do a Drop for draughts")

    def withVariant(variant: Variant): Situation = variant match {
      case Variant.Draughts(variant) => Draughts(s.withVariant(variant))
      case _                         => sys.error("Not passed Draughts objects")
    }

    def unary_! : Situation = Draughts(s.unary_!)

    def copy(board: Board): Situation = Draughts(board match {
      case Board.Draughts(board) => s.copy(board)
      case _                     => sys.error("Can't copy a draughts situation with a non-draughts board")
    })

    def gameLogic: GameLogic = GameLogic.Draughts()

    def toDraughts = s
    def toChess    = sys.error("Can't make chess situation from draughts situation")
    def toFairySF  = sys.error("Can't make fairysf situation from draughts situation")
    def toMancala  = sys.error("Can't make mancala situation from draughts situation")

  }

  final case class FairySF(s: fairysf.Situation)
      extends Situation(
        Board.FairySF(s.board),
        s.player
      ) {

    lazy val moves: Map[Pos, List[Move]] = s.moves.map { case (p: fairysf.Pos, l: List[fairysf.Move]) =>
      (Pos.FairySF(p), l.map(Move.FairySF))
    }

    lazy val check: Boolean = s.check

    def checkSquare = s.checkSquare.map(Pos.FairySF)

    def opponentHasInsufficientMaterial: Boolean = s.opponentHasInsufficientMaterial

    def threefoldRepetition: Boolean = s.threefoldRepetition

    override lazy val perpetualPossible: Boolean = s.perpetualPossible

    def end: Boolean = s.end

    def winner: Option[Player] = s.winner

    lazy val destinations: Map[Pos, List[Pos]] = s.destinations.map {
      case (p: fairysf.Pos, l: List[fairysf.Pos]) => (Pos.FairySF(p), l.map(Pos.FairySF))
    }

    def drops: Option[List[Pos]] = s.drops.map(_.map(Pos.FairySF))

    def dropsByRole: Option[Map[Role, List[Pos]]] = s.dropsByRole.map(_.map {
      case (r: fairysf.Role, p: List[fairysf.Pos]) => (Role.FairySFRole(r), p.map(Pos.FairySF))
    })

    def playable(strict: Boolean): Boolean = s.playable(strict)

    val status: Option[Status] = s.status

    def move(
        from: Pos,
        to: Pos,
        promotion: Option[PromotableRole] = None,
        finalSquare: Boolean = false,
        forbiddenUci: Option[List[String]] = None,
        captures: Option[List[Pos]] = None,
        partialCaptures: Boolean = false
    ): Validated[String, Move] = (from, to) match {
      case (Pos.FairySF(from), Pos.FairySF(to)) =>
        s.move(from, to, promotion.map(_.toFairySF)).toEither.map(m => Move.FairySF(m)).toValidated
      case _                                    => sys.error("Not passed FairySF objects")
    }

    def move(uci: Uci.Move): Validated[String, Move] = uci match {
      case Uci.FairySFMove(uci) => s.move(uci).toEither.map(m => Move.FairySF(m)).toValidated
      case _                    => sys.error("Not passed FairySF objects")
    }

    def drop(role: Role, pos: Pos): Validated[String, Drop] = (role, pos) match {
      case (Role.FairySFRole(role), Pos.FairySF(pos)) =>
        s.drop(role, pos).toEither.map(d => Drop.FairySF(d)).toValidated
      case _                                          => sys.error("Not passed FairySF objects")
    }

    def withVariant(variant: Variant): Situation = variant match {
      case Variant.FairySF(variant) => FairySF(s.withVariant(variant))
      case _                        => sys.error("Not passed FairySF objects")
    }

    def unary_! : Situation = FairySF(s.unary_!)

    def copy(board: Board): Situation = FairySF(board match {
      case Board.FairySF(board) => s.copy(board)
      case _                    => sys.error("Can't copy a fairysf situation with a non-fairysf board")
    })

    def gameLogic: GameLogic = GameLogic.FairySF()

    def toFairySF  = s
    def toChess    = sys.error("Can't make chess situation from fairysf situation")
    def toDraughts = sys.error("Can't make draughts situation from fairysf situation")
    def toMancala  = sys.error("Can't make mancala situation from fairy situation")
  }

  final case class Mancala(s: mancala.Situation)
      extends Situation(
        Board.Mancala(s.board),
        s.player
      ) {

    lazy val moves: Map[Pos, List[Move]] = s.moves.map { case (p: mancala.Pos, l: List[mancala.Move]) =>
      (Pos.Mancala(p), l.map(Move.Mancala))
    }

    lazy val check: Boolean = false

    def checkSquare = None

    def opponentHasInsufficientMaterial: Boolean = s.opponentHasInsufficientMaterial

    def threefoldRepetition: Boolean = false

    override lazy val perpetualPossible: Boolean = false

    def end: Boolean = s.end

    def winner: Option[Player] = s.winner

    lazy val destinations: Map[Pos, List[Pos]] = s.destinations.map {
      case (p: mancala.Pos, l: List[mancala.Pos]) => (Pos.Mancala(p), l.map(Pos.Mancala))
    }

    def drops: Option[List[Pos]] = None

    def dropsByRole: Option[Map[Role, List[Pos]]] = None

    def drop(role: Role, pos: Pos): Validated[String, Drop] =
      sys.error("Can't do a Drop for mancala")

    def playable(strict: Boolean): Boolean = s.playable(strict)

    val status: Option[Status] = s.status

    def move(
        from: Pos,
        to: Pos,
        promotion: Option[PromotableRole] = None,
        finalSquare: Boolean = false,
        forbiddenUci: Option[List[String]] = None,
        captures: Option[List[Pos]] = None,
        partialCaptures: Boolean = false
    ): Validated[String, Move] = (from, to) match {
      case (Pos.Mancala(from), Pos.Mancala(to)) =>
        s.move(from, to, promotion.map(_.toMancala)).toEither.map(m => Move.Mancala(m)).toValidated
      case _                                    => sys.error("Not passed Mancala objects")
    }

    def move(uci: Uci.Move): Validated[String, Move] = uci match {
      case Uci.MancalaMove(uci) => s.move(uci).toEither.map(m => Move.Mancala(m)).toValidated
      case _                    => sys.error("Not passed Mancala objects")
    }

    def withVariant(variant: Variant): Situation = variant match {
      case Variant.Mancala(variant) => Mancala(s.withVariant(variant))
      case _                        => sys.error("Not passed Mancala objects")
    }

    def unary_! : Situation = Mancala(s.unary_!)

    def copy(board: Board): Situation = Mancala(board match {
      case Board.Mancala(board) => s.copy(board)
      case _                    => sys.error("Can't copy a mancala situation with a non-mancala board")
    })

    def gameLogic: GameLogic = GameLogic.Mancala()

    def toFairySF  = sys.error("Can't make fairysf situation from mancala situation")
    def toChess    = sys.error("Can't make chess situation from mancala situation")
    def toDraughts = sys.error("Can't make draughts situation from mancala situation")
    def toMancala  = s
  }

  def apply(lib: GameLogic, board: Board, player: Player): Situation = (lib, board) match {
    case (GameLogic.Draughts(), Board.Draughts(board)) => Draughts(draughts.Situation(board, player))
    case (GameLogic.Chess(), Board.Chess(board))       => Chess(chess.Situation(board, player))
    case (GameLogic.FairySF(), Board.FairySF(board))   => FairySF(fairysf.Situation(board, player))
    case (GameLogic.Mancala(), Board.Mancala(board))   => Mancala(mancala.Situation(board, player))
    case _                                             => sys.error("Mismatched gamelogic types 3")
  }

  def apply(lib: GameLogic, variant: Variant): Situation = (lib, variant) match {
    case (GameLogic.Draughts(), Variant.Draughts(variant)) => Draughts(draughts.Situation.apply(variant))
    case (GameLogic.Chess(), Variant.Chess(variant))       => Chess(chess.Situation.apply(variant))
    case (GameLogic.FairySF(), Variant.FairySF(variant))   => FairySF(fairysf.Situation.apply(variant))
    case (GameLogic.Mancala(), Variant.Mancala(variant))   => Mancala(mancala.Situation.apply(variant))
    case _                                                 => sys.error("Mismatched gamelogic types 4")
  }

  def wrap(s: chess.Situation)    = Chess(s)
  def wrap(s: draughts.Situation) = Draughts(s)
  def wrap(s: fairysf.Situation)  = FairySF(s)
  def wrap(s: mancala.Situation)  = Mancala(s)

}
