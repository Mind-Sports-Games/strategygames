package strategygames

import chess._
import draughts._

sealed trait Color {

  final def fold[A](w: => A, b: => A): A = if (white) w else b

  def unary_! : Color

  val letter: Char
  val name: String

  val white: Boolean
  val black: Boolean
}

object Color {

  // TODO: this is duplicated three times now.
  // TODO
  case class Map[A](white: A, black: A) {
    def apply(color: Color) = if (color.white) white else black

    def update(color: Color, f: A => A) = {
      if (color.white) copy(white = f(white))
      else copy(black = f(black))
    }

    def map[B](fw: A => B, fb: A => B) = copy(white = fw(white), black = fb(black))

    def map[B](f: A => B): Map[B] = map(f, f)

    def all: Seq[A] = Seq(white, black)

    def reduce[B](f: (A, A) => B) = f(white, black)

    def forall(pred: A => Boolean) = pred(white) && pred(black)

    def exists(pred: A => Boolean) = pred(white) || pred(black)
  }

  object Map {
    def apply[A](f: Color => A): Map[A] = Map(white = f(White), black = f(Black))
  }

  final case class Chess(c: chess.Color) extends Color {
    def unary_! = !c

    val letter = c.letter
    val name = c.name

    val white = c.white
    val black = c.black
  }

  final case class Draughts(c: draughts.Color) extends Color {
    def unary_! = !c

    val letter = c.letter
    val name = c.name

    val white = c.white
    val black = c.black
  }

  def fromPly(lib: GameLib, ply: Int) = lib match {
    case GameLib.Draughts => Draughts(draughts.fromPly(ply))
    case GameLib.Chess => Chess(chess.fromPly(ply))
  }

  def fromWhite(lib: GameLib, white: Boolean): Color = lib match {
    case GameLib.Draughts => Draughts(draughts.fromWhite(white))
    case GameLib.Chess => Chess(chess.fromWhite(white))
  }

  def fromName(lib: GameLib, n: String): Option[Color] = lib match {
    case GameLib.Draughts => Draughts(draughts.fromName(n))
    case GameLib.Chess => Chess(chess.fromName(n))
  }

  def apply(lib: GameLib, c: Char): Option[Color] = lib match {
    case GameLib.Draughts => Draughts(draughts.Color(c))
    case GameLib.Chess => Chess(chess.Color(c))
  }

  def white(lib: GameLib): Color = lib match {
    case GameLib.Draughts => Draughts(draughts.white)
    case GameLib.Chess => Chess(chess.white)
  }

  def black(lib: GameLib): Color = lib match {
    case GameLib.Draughts => Draughts(draughts.black)
    case GameLib.Chess => Chess(chess.black)
  }

  def all(lib: GameLib) = lib match {
    case GameLib.Draughts => Draughts(draughts.all(ply))
    case GameLib.Chess => Chess(chess.all(ply))
  }

  def showResult(lib: GameLib, color: Option[Color]) = lib match {
    case GameLib.Draughts => Draughts(draughts.showResult(ply))
    case GameLib.Chess => Chess(chess.showResult(ply))
  }

  def fromResult(lib: GameLib, result: String): Option[Color] = lib match {
    case GameLib.Draughts => Draughts(draughts.fromResult(result))
    case GameLib.Chess => Chess(chess.fromResult(result))
  }
}
