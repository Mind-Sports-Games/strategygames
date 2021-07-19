package strategygames

import strategygames.variant.Variant

import cats.data.Validated
import cats.implicits._

import strategygames.format.Uci

abstract sealed class Situation(val board: Board, val color: Color) {

  lazy val actors = board actorsOf color

  val moves: Map[Pos, List[Move]]

  val destinations: Map[Pos, List[Pos]]

  def drops: Option[List[Pos]]

  def history = board.history

  val check: Boolean

  def checkSquare: Option[Pos]

  def checkMate: Boolean = board.variant checkmate this

  def autoDraw: Boolean = board.autoDraw || board.variant.specialDraw(this)

  lazy val threefoldRepetition: Boolean = board.history.threefoldRepetition

  def variantEnd = board.variant specialEnd this

  def end: Boolean

  def winner: Option[Color] = board.variant.winner(this)

  def playable(strict: Boolean): Boolean

  val status: Option[Status]

  def withHistory(history: History): Situation

  def withVariant(variant: Variant): Situation

  def unary_! : Situation

}

object Situation {

  final case class Chess(s: chess.Situation) extends Situation(
    Board.Chess(s.board),
    s.color
  ) {

    lazy val moves: Map[Pos, List[Move]] = s.moves.map{
      case (p: chess.Pos, l: List[chess.Move]) => (Pos.Chess(p), l.map(Move.Chess))
    }

    lazy val check: Boolean = s.check

    def checkSquare = s.checkSquare.map(Pos.Chess)

    def end: Boolean = s.end

    val destinations: Map[Pos, List[Pos]] = s.destinations.map{
      case (p: chess.Pos, l: List[chess.Pos]) => (Pos.Chess(p), l.map(Pos.Chess))
    }

    def drops: Option[List[Pos]] = s.drops.map(_.map(Pos.Chess))

    def playable(strict: Boolean): Boolean = s.playable(strict)

    val status: Option[Status] = s.status
    
    def withHistory(history: History): Situation = history match {
      case History.Chess(history) => Chess(s.withHistory(history))
      case _ => sys.error("Not passed Chess objects")
    }

    def withVariant(variant: Variant): Situation = variant match {
      case Variant.Chess(variant) => Chess(s.withVariant(variant))
      case _ => sys.error("Not passed Chess objects")
    }

    def unary_! : Situation = Chess(s.unary_!)

  }

  final case class Draughts(s: draughts.Situation) extends Situation(
    Board.Draughts(s.board),
    s.color
  ) {

    // TODO: DRAUGHTS I think that .validMoves is correct, but unsure. needs testing.
    lazy val moves: Map[Pos, List[Move]] = s.validMoves.map{
      case (p: draughts.Pos, l: List[draughts.Move]) => (Pos.Draughts(p), l.map(Move.Draughts))
    }

    lazy val check: Boolean = false

    def checkSquare = None

    // TODO: this probably needs to be properly implemented
    val destinations: Map[Pos, List[Pos]] = Map()

    def drops: Option[List[Pos]] = None

    def end: Boolean = s.end

    def playable(strict: Boolean): Boolean = s.playable(strict)

    val status: Option[Status] = s.status
    
    def withHistory(history: History): Situation = history match {
      case History.Draughts(history) => Draughts(s.withHistory(history))
      case _ => sys.error("Not passed Draughts objects")
    }

    def withVariant(variant: Variant): Situation = variant match {
      case Variant.Draughts(variant) => Draughts(s.withVariant(variant))
      case _ => sys.error("Not passed Draughts objects")
    }

    def unary_! : Situation = Draughts(s.unary_!)

  }

  def apply(lib: GameLib, variant: Variant): Situation = (lib, variant) match {
    case (GameLib.Draughts(), Variant.Draughts(variant)) => Draughts(draughts.Situation.apply(variant))
    case (GameLib.Chess(), Variant.Chess(variant))       => Chess(chess.Situation.apply(variant))
    case _ => sys.error("Mismatched gamelib types")
  }

}
