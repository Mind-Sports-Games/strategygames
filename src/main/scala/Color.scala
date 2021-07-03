package strategygames

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

  final case class Chess(c: chess.Color) extends Color {
    def unary_! = Chess(!c)

    val letter = c.letter
    val name   = c.name

    val white = c.white
    val black = c.black
  }

  final case class Draughts(c: draughts.Color) extends Color {
    def unary_! = Draughts(!c)

    val letter = c.letter
    val name   = c.name

    val white = c.white
    val black = c.black
  }

  def fromPly(lib: GameLib, ply: Int) = lib match {
    case GameLib.Draughts() => Draughts(draughts.Color.fromPly(ply))
    case GameLib.Chess()    => Chess(chess.Color.fromPly(ply))
  }

  def fromWhite(lib: GameLib, white: Boolean): Color = lib match {
    case GameLib.Draughts() => Draughts(draughts.Color.fromWhite(white))
    case GameLib.Chess()    => Chess(chess.Color.fromWhite(white))
  }

  def fromName(lib: GameLib, n: String): Option[Color] = lib match {
    case GameLib.Draughts() => draughts.Color.fromName(n).map(Draughts)
    case GameLib.Chess()    => chess.Color.fromName(n).map(Chess)
  }

  def apply(lib: GameLib, c: Char): Option[Color] = lib match {
    case GameLib.Draughts() => draughts.Color(c).map(Draughts)
    case GameLib.Chess()    => chess.Color(c).map(Chess)
  }

  def white(lib: GameLib): Color = lib match {
    case GameLib.Draughts() => Draughts(draughts.Color.white)
    case GameLib.Chess()    => Chess(chess.Color.white)
  }

  def black(lib: GameLib): Color = lib match {
    case GameLib.Draughts() => Draughts(draughts.Color.black)
    case GameLib.Chess()    => Chess(chess.Color.black)
  }

  def all(lib: GameLib) = lib match {
    case GameLib.Draughts() => draughts.Color.all.map(Draughts)
    case GameLib.Chess()    => chess.Color.all.map(Chess)
  }

  // TODO: these are different between draughts and chess.
  /*def showResult(lib: GameLib, color: Option[Color]) = lib match {
    case GameLib.Draughts() => draughts.Color.showResult(color).map(Draughts)
    case GameLib.Chess() => chess.Color.showResult(color).map(Chess)
  }*/

  def fromResult(lib: GameLib, result: String): Option[Color] = lib match {
    case GameLib.Draughts() => draughts.Color.fromResult(result).map(Draughts)
    case GameLib.Chess()    => chess.Color.fromResult(result).map(Chess)
  }
}
