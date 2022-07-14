package strategygames

sealed trait Player {

  final def fold[A](a: => A, b: => A): A = if (p1) a else b

  def unary_! : Player

  val letter: Char
  val name: String
  val classicName: String

  val p1 = this == Player.P1
  val p2 = this == Player.P2
}

object Player {

  // TODO: this is duplicated three times now.
  // TODO
  case class Map[A](p1: A, p2: A) {
    def apply(player: Player) = if (player.p1) p1 else p2

    def update(player: Player, f: A => A) = {
      if (player.p1) copy(p1 = f(p1))
      else copy(p2 = f(p2))
    }

    def map[B](f1: A => B, f2: A => B) = copy(p1 = f1(p1), p2 = f2(p2))

    def map[B](f: A => B): Map[B] = map(f, f)

    def all: Seq[A] = Seq(p1, p2)

    def reduce[B](f: (A, A) => B) = f(p1, p2)

    def forall(pred: A => Boolean) = pred(p1) && pred(p2)

    def exists(pred: A => Boolean) = pred(p1) || pred(p2)
  }

  object Map {
    def apply[A](f: Player => A): Map[A] = Map(p1 = f(P1), p2 = f(P2))
  }

  case object P1 extends Player {

    lazy val unary_! = P2

    // retain old color lettering
    val letter      = 'w'
    val name        = "p1"
    val classicName = "white"

    override val hashCode = 1
  }

  case object P2 extends Player {

    val unary_! = P1

    // retain old color lettering
    val letter      = 'b'
    val name        = "p2"
    val classicName = "black"

    override val hashCode = 2
  }

  def fromPly(ply: Int) = fromP1((ply & 1) == 0)

  def fromP1(p1: Boolean): Player = if (p1) P1 else P2

  def fromName(n: String): Option[Player] =
    if (n == "p1") Option(P1)
    else if (n == "p2") Option(P2)
    else None

  def apply(b: Boolean): Player = if (b) P1 else P2

  def apply(n: String): Option[Player] =
    if (n == "p1") Some(P1)
    else if (n == "p2") Some(P2)
    else None

  def apply(c: Char): Option[Player] =
    if (c == '1' || c == 'w' || c == 'W' || c == 'S') Some(P1)
    else if (c == '2' || c == 'b' || c == 'B' || c == 'N') Some(P2)
    else None

  val p1: Player = P1
  val p2: Player = P2

  val all = List(P1, P2)

  val names = all map (_.name)

  def exists(name: String) = all exists (_.name == name)

  // need to move this out of Player
  def showResult(player: Option[Player], draughtsResult: Boolean = false) = player match {
    case Some(P1) => if (draughtsResult) "2-0" else "1-0"
    case Some(P2) => if (draughtsResult) "0-2" else "0-1"
    case None     => if (draughtsResult) "1-1" else "1/2-1/2"
  }

  // need to move this out of Player
  def fromResult(result: String): Option[Player] =
    result match {
      case "1-0" => Option(P1)
      case "2-0" => Option(P1) // draughts
      case "0-1" => Option(P2)
      case "0-2" => Option(P2) // draughts
      case _     => None
    }

}
