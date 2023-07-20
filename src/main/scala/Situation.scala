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

  def dropsAsDrops: List[Drop]

  def passes: List[Pass]

  def actions: List[Action] =
    moves.values.flatten.toList ::: dropsAsDrops ::: passes

  def history = board.history

  val check: Boolean

  def checkSquare: Option[Pos]

  def checkMate: Boolean = board.variant checkmate this

  def opponentHasInsufficientMaterial: Boolean

  def threefoldRepetition: Boolean

  def isRepetition: Boolean

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

  def pass: Validated[String, Pass]

  def withVariant(variant: Variant): Situation

  def gameLogic: GameLogic

  // TODO: There is probably a better way to a more generalized version of this function ...
  def copy(board: Board): Situation

  // TODO: yup, still not typesafe
  def toChess: chess.Situation
  def toDraughts: draughts.Situation
  def toFairySF: fairysf.Situation
  def toSamurai: samurai.Situation
  def toTogyzkumalak: togyzkumalak.Situation
  def toGo: go.Situation

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

    def isRepetition: Boolean = s.threefoldRepetition

    def end: Boolean = s.end

    def winner: Option[Player] = s.winner

    lazy val destinations: Map[Pos, List[Pos]] = s.destinations.map {
      case (p: chess.Pos, l: List[chess.Pos]) => (Pos.Chess(p), l.map(Pos.Chess))
    }

    def drops: Option[List[Pos]] = s.drops.map(_.map(Pos.Chess))

    def dropsByRole: Option[Map[Role, List[Pos]]] =
      s.dropsByRole.map(_.map { case (r: chess.Role, p: List[chess.Pos]) =>
        (Role.ChessRole(r), p.map(Pos.Chess))
      })

    // this is really inefficient but harder to otherwise generate for chess
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

    def passes: List[Pass] = List.empty

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

    def pass: Validated[String, Pass] = sys.error("Can't do a Pass for chess")

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

    def toChess        = s
    def toDraughts     = sys.error("Can't make draughts situation from chess situation")
    def toFairySF      = sys.error("Can't make fairysf situation from chess situation")
    def toSamurai      = sys.error("Can't make samurai situation from chess situation")
    def toTogyzkumalak = sys.error("Can't make togyzkumalak situation from chess situation")
    def toGo           = sys.error("Can't make go situation from chess situation")
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

    // TODO: we should be able to implement this in the same way chess etc as
    // validMoves now combines validMoves and validMovesFrom as it does in lila
    lazy val destinations: Map[Pos, List[Pos]] = Map()

    def drops: Option[List[Pos]] = None

    def dropsByRole: Option[Map[Role, List[Pos]]] = None

    def dropsAsDrops: List[Drop] = List.empty

    def passes: List[Pass] = List.empty

    // possibly need to do something for this
    def opponentHasInsufficientMaterial: Boolean = false

    def threefoldRepetition: Boolean = s.threefoldRepetition

    def isRepetition: Boolean = s.threefoldRepetition

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

    // todo convert these to validated error message?
    def drop(role: Role, pos: Pos): Validated[String, Drop] =
      sys.error("Can't do a Drop for draughts")

    def pass: Validated[String, Pass] = sys.error("Can't do a Pass for draughts")

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

    def toDraughts     = s
    def toChess        = sys.error("Can't make chess situation from draughts situation")
    def toFairySF      = sys.error("Can't make fairysf situation from draughts situation")
    def toSamurai      = sys.error("Can't make samurai situation from draughts situation")
    def toTogyzkumalak = sys.error("Can't make togyzkumalak situation from draughts situation")
    def toGo           = sys.error("Can't make go situation from draughts situation")

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

    def isRepetition: Boolean = s.threefoldRepetition

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

    def dropsAsDrops: List[Drop] = s.dropsAsDrops.map(Drop.FairySF)

    def passes: List[Pass] = List.empty

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

    def pass: Validated[String, Pass] = sys.error("Can't do a Pass for fairysf")

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

    def toFairySF      = s
    def toChess        = sys.error("Can't make chess situation from fairysf situation")
    def toDraughts     = sys.error("Can't make draughts situation from fairysf situation")
    def toSamurai      = sys.error("Can't make samurai situation from fairysf situation")
    def toTogyzkumalak = sys.error("Can't make togyzkumalak situation from fairysf situation")
    def toGo           = sys.error("Can't make go situation from fairysf situation")
  }

  final case class Samurai(s: samurai.Situation)
      extends Situation(
        Board.Samurai(s.board),
        s.player
      ) {

    lazy val moves: Map[Pos, List[Move]] = s.moves.map { case (p: samurai.Pos, l: List[samurai.Move]) =>
      (Pos.Samurai(p), l.map(Move.Samurai))
    }

    lazy val check: Boolean = false

    def checkSquare = None

    def opponentHasInsufficientMaterial: Boolean = s.opponentHasInsufficientMaterial

    def threefoldRepetition: Boolean = false

    def isRepetition: Boolean = s.isRepetition

    override lazy val perpetualPossible: Boolean = false

    def end: Boolean = s.end

    def winner: Option[Player] = s.winner

    lazy val destinations: Map[Pos, List[Pos]] = s.destinations.map {
      case (p: samurai.Pos, l: List[samurai.Pos]) => (Pos.Samurai(p), l.map(Pos.Samurai))
    }

    def drops: Option[List[Pos]] = None

    def dropsByRole: Option[Map[Role, List[Pos]]] = None

    def dropsAsDrops: List[Drop] = List.empty

    def passes: List[Pass] = List.empty

    def drop(role: Role, pos: Pos): Validated[String, Drop] =
      sys.error("Can't do a Drop for samurai")

    def pass: Validated[String, Pass] = sys.error("Can't do a Pass for samurai")

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
      case (Pos.Samurai(from), Pos.Samurai(to)) =>
        s.move(from, to, promotion.map(_.toSamurai)).toEither.map(m => Move.Samurai(m)).toValidated
      case _                                    => sys.error("Not passed Samurai objects")
    }

    def move(uci: Uci.Move): Validated[String, Move] = uci match {
      case Uci.SamuraiMove(uci) => s.move(uci).toEither.map(m => Move.Samurai(m)).toValidated
      case _                    => sys.error("Not passed Samurai objects")
    }

    def withVariant(variant: Variant): Situation = variant match {
      case Variant.Samurai(variant) => Samurai(s.withVariant(variant))
      case _                        => sys.error("Not passed Samurai objects")
    }

    def unary_! : Situation = Samurai(s.unary_!)

    def copy(board: Board): Situation = Samurai(board match {
      case Board.Samurai(board) => s.copy(board)
      case _                    => sys.error("Can't copy a samurai situation with a non-samurai board")
    })

    def gameLogic: GameLogic = GameLogic.Samurai()

    def toFairySF      = sys.error("Can't make fairysf situation from samurai situation")
    def toChess        = sys.error("Can't make chess situation from samurai situation")
    def toDraughts     = sys.error("Can't make draughts situation from samurai situation")
    def toSamurai      = s
    def toTogyzkumalak = sys.error("Can't make draughts situation from samurai situation")
    def toGo           = sys.error("Can't make go situation from samurai situation")
  }

  final case class Togyzkumalak(s: togyzkumalak.Situation)
      extends Situation(
        Board.Togyzkumalak(s.board),
        s.player
      ) {

    lazy val moves: Map[Pos, List[Move]] = s.moves.map {
      case (p: togyzkumalak.Pos, l: List[togyzkumalak.Move]) =>
        (Pos.Togyzkumalak(p), l.map(Move.Togyzkumalak))
    }

    lazy val check: Boolean = false

    def checkSquare = None

    def opponentHasInsufficientMaterial: Boolean = s.opponentHasInsufficientMaterial

    def threefoldRepetition: Boolean = false
    def isRepetition: Boolean        = false

    override lazy val perpetualPossible: Boolean = false

    def end: Boolean = s.end

    def winner: Option[Player] = s.winner

    lazy val destinations: Map[Pos, List[Pos]] = s.destinations.map {
      case (p: togyzkumalak.Pos, l: List[togyzkumalak.Pos]) => (Pos.Togyzkumalak(p), l.map(Pos.Togyzkumalak))
    }

    def drops: Option[List[Pos]] = None

    def dropsByRole: Option[Map[Role, List[Pos]]] = None

    def dropsAsDrops: List[Drop] = List.empty

    def passes: List[Pass] = List.empty

    def drop(role: Role, pos: Pos): Validated[String, Drop] =
      sys.error("Can't do a Drop for togyzkumalak")

    def pass: Validated[String, Pass] = sys.error("Can't do a Pass for togyzkumalak")

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
      case (Pos.Togyzkumalak(from), Pos.Togyzkumalak(to)) =>
        s.move(from, to, promotion.map(_.toTogyzkumalak)).toEither.map(m => Move.Togyzkumalak(m)).toValidated
      case _                                              => sys.error("Not passed Togyzkumalak objects")
    }

    def move(uci: Uci.Move): Validated[String, Move] = uci match {
      case Uci.TogyzkumalakMove(uci) => s.move(uci).toEither.map(m => Move.Togyzkumalak(m)).toValidated
      case _                         => sys.error("Not passed Togyzkumalak objects")
    }

    def withVariant(variant: Variant): Situation = variant match {
      case Variant.Togyzkumalak(variant) => Togyzkumalak(s.withVariant(variant))
      case _                             => sys.error("Not passed Togyzkumalak objects")
    }

    def unary_! : Situation = Togyzkumalak(s.unary_!)

    def copy(board: Board): Situation = Togyzkumalak(board match {
      case Board.Togyzkumalak(board) => s.copy(board)
      case _                         => sys.error("Can't copy a togyzkumalak situation with a non-togyzkumalak board")
    })

    def gameLogic: GameLogic = GameLogic.Togyzkumalak()

    def toFairySF      = sys.error("Can't make fairysf situation from togyzkumalak situation")
    def toChess        = sys.error("Can't make chess situation from togyzkumalak situation")
    def toDraughts     = sys.error("Can't make draughts situation from togyzkumalak situation")
    def toSamurai      = sys.error("Can't make samurai situation from togyzkumalak situation")
    def toTogyzkumalak = s
    def toGo           = sys.error("Can't make go situation from togyzkumalak situation")
  }

  final case class Go(s: go.Situation)
      extends Situation(
        Board.Go(s.board),
        s.player
      ) {

    lazy val moves: Map[Pos, List[Move]] = Map.empty[Pos, List[Move]]

    lazy val check: Boolean = false

    def checkSquare = None

    def opponentHasInsufficientMaterial: Boolean = s.opponentHasInsufficientMaterial

    def threefoldRepetition: Boolean = false

    def isRepetition: Boolean                    = false
    override lazy val perpetualPossible: Boolean = false // not allowed to repeat ko

    def end: Boolean = s.end

    def winner: Option[Player] = s.winner

    lazy val destinations: Map[Pos, List[Pos]] = Map.empty[Pos, List[Pos]]

    def drops: Option[List[Pos]] = s.drops.map(_.map(Pos.Go))

    def dropsByRole: Option[Map[Role, List[Pos]]] = s.dropsByRole.map(_.map {
      case (r: go.Role, p: List[go.Pos]) => (Role.GoRole(r), p.map(Pos.Go))
    })

    def dropsAsDrops: List[Drop] = s.dropsAsDrops.map(Drop.Go)

    def passes: List[Pass] = pass.fold[List[Pass]](_ => List.empty, p => List(p))

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
    ): Validated[String, Move] = sys.error("Can't do a move dest for go")

    def move(uci: Uci.Move): Validated[String, Move] = sys.error("Can't do a move dest for go")

    def drop(role: Role, pos: Pos): Validated[String, Drop] = (role, pos) match {
      case (Role.GoRole(role), Pos.Go(pos)) =>
        s.drop(role, pos).toEither.map(d => Drop.Go(d)).toValidated
      case _                                => sys.error("Not passed Go objects")
    }

    def pass: Validated[String, Pass] = s.pass().toEither.map(p => Pass.Go(p)).toValidated

    def withVariant(variant: Variant): Situation = variant match {
      case Variant.Go(variant) => Go(s.withVariant(variant))
      case _                   => sys.error("Not passed Go objects")
    }

    def unary_! : Situation = Go(s.unary_!)

    def copy(board: Board): Situation = Go(board match {
      case Board.Go(board) => s.copy(board)
      case _               => sys.error("Can't copy a go situation with a non-go board")
    })

    def gameLogic: GameLogic = GameLogic.Go()

    def toFairySF      = sys.error("Can't make fairysf situation from go situation")
    def toChess        = sys.error("Can't make chess situation from go situation")
    def toDraughts     = sys.error("Can't make draughts situation from go situation")
    def toSamurai      = sys.error("Can't make samurai situation from go situation")
    def toTogyzkumalak = sys.error("Can't make togyzkumalak situation from go situation")
    def toGo           = s
  }

  def apply(lib: GameLogic, board: Board, player: Player): Situation = (lib, board) match {
    case (GameLogic.Draughts(), Board.Draughts(board))         => Draughts(draughts.Situation(board, player))
    case (GameLogic.Chess(), Board.Chess(board))               => Chess(chess.Situation(board, player))
    case (GameLogic.FairySF(), Board.FairySF(board))           => FairySF(fairysf.Situation(board, player))
    case (GameLogic.Samurai(), Board.Samurai(board))           => Samurai(samurai.Situation(board, player))
    case (GameLogic.Togyzkumalak(), Board.Togyzkumalak(board)) =>
      Togyzkumalak(togyzkumalak.Situation(board, player))
    case (GameLogic.Go(), Board.Go(board))                     => Go(go.Situation(board, player))
    case _                                                     => sys.error("Mismatched gamelogic types 3")
  }

  def apply(lib: GameLogic, variant: Variant): Situation = (lib, variant) match {
    case (GameLogic.Draughts(), Variant.Draughts(variant))         => Draughts(draughts.Situation.apply(variant))
    case (GameLogic.Chess(), Variant.Chess(variant))               => Chess(chess.Situation.apply(variant))
    case (GameLogic.FairySF(), Variant.FairySF(variant))           => FairySF(fairysf.Situation.apply(variant))
    case (GameLogic.Samurai(), Variant.Samurai(variant))           => Samurai(samurai.Situation.apply(variant))
    case (GameLogic.Togyzkumalak(), Variant.Togyzkumalak(variant)) =>
      Togyzkumalak(togyzkumalak.Situation.apply(variant))
    case (GameLogic.Go(), Variant.Go(variant))                     => Go(go.Situation.apply(variant))
    case _                                                         => sys.error("Mismatched gamelogic types 4")
  }

  def wrap(s: chess.Situation)        = Chess(s)
  def wrap(s: draughts.Situation)     = Draughts(s)
  def wrap(s: fairysf.Situation)      = FairySF(s)
  def wrap(s: samurai.Situation)      = Samurai(s)
  def wrap(s: togyzkumalak.Situation) = Togyzkumalak(s)
  def wrap(s: go.Situation)           = Go(s)

}
