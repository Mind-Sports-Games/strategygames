package strategygames

sealed trait Color {

  final def fold[A](w: => A, b: => A): A = if (white) w else b

  def unary_! : Color

  val letter: Char
  val name: String

  val white = this == Color.White
  val black = this == Color.Black
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

  case object White extends Color {

    lazy val unary_! = Black

    val letter = 'w'
    val name   = "white"

    override val hashCode = 1
  }

  case object Black extends Color {

    val unary_! = White

    val letter = 'b'
    val name   = "black"

    override val hashCode = 2
  }

  def fromPly(ply: Int) = fromWhite((ply & 1) == 0)

  def fromWhite(white: Boolean): Color = if (white) White else Black

  def fromName(n: String): Option[Color] =
    if (n == "white") Option(White)
    else if (n == "black") Option(Black)
    else None

  def apply(b: Boolean): Color = if (b) White else Black

  def apply(n: String): Option[Color] =
    if (n == "white") Some(White)
    else if (n == "black") Some(Black)
    else None

  def apply(c: Char): Option[Color] =
    if (c == 'W' || c == 'w') Some(White)
    else if (c == 'B' || c == 'b') Some(Black)
    else None

  val white: Color = White
  val black: Color = Black

  val all = List(White, Black)

  val names = all map (_.name)

  def exists(name: String) = all exists (_.name == name)

  //need to move this out of Color
  def showResult(color: Option[Color], draughtsResult: Boolean = false) = color match {
    case Some(White) => if (draughtsResult) "2-0" else "1-0"
    case Some(Black) => if (draughtsResult) "0-2" else "0-1"
    case None        => if (draughtsResult) "1-1" else "1/2-1/2"
  }

  //need to move this out of Color
  def fromResult(result: String): Option[Color] =
    result match {
      case "1-0" => Option(White)
      case "2-0" => Option(White)//draughts
      case "0-1" => Option(Black)
      case "0-2" => Option(Black)//draughts
      case _     => None
    }


  /*
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

  def all(lib: GameLib): List[Color] = lib match {
    case GameLib.Draughts() => draughts.Color.all.map(Draughts)
    case GameLib.Chess()    => chess.Color.all.map(Chess)
  }

  //result default set to currently work for chess lila
  //def showResult(lib: GameLib, color: Option[Color], result: Boolean = false): String = lib match {
  //  case GameLib.Draughts() => draughts.Color.showResult(color, result)
  //  case GameLib.Chess()    => chess.Color.allByResult.map{case(c, s) => (c.map(Chess), s)}.get(color)
  //}

  def fromResult(lib: GameLib, result: String): Option[Color] = lib match {
    case GameLib.Draughts() => draughts.Color.fromResult(result).map(Draughts)
    case GameLib.Chess()    => chess.Color.fromResult(result).map(Chess)
  }

  implicit def chessColor(c: chess.Color) = Chess(c)
  implicit def draughtsColor(c: draughts.Color) = Draughts(c)
  */
}
