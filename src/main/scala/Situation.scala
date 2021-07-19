package strategygames

import strategygames.variant.Variant

import cats.data.Validated
import cats.implicits._

import strategygames.format.Uci

abstract sealed class Situation(val board: Board, val color: Color) {

  lazy val actors = board actorsOf color

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
    Color.Chess(s.color)
  ) {

    lazy val check: Boolean = s.check

    def checkSquare = s.checkSquare.map(Pos.Chess)

    def end: Boolean = s.end

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
    Color.Draughts(s.color)
  ) {

    lazy val check: Boolean = false

    def checkSquare = None

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
