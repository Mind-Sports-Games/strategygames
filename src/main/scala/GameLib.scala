package strategygames

sealed abstract class GameLib {
  def id: Int
  def name: String
  def displayOnly: Boolean

  override def toString = s"Lib($name)"
}

object GameLib {

  final case class Chess() extends GameLib {
    def id = 0
    def name = "Chess"
    def displayOnly = false
  }

  final case class Draughts() extends GameLib {
    def id = 1
    def name = "Draughts"
    def displayOnly = false
  }

  final case class LinesOfAction() extends GameLib {
    def id = 2
    def name = "Lines Of Action"
    def displayOnly = true
  }

  def all = List(Chess, Draughts, LinesOfAction)

  // TODO: I'm sure there is a better scala way of doing this
  def apply(id: Int): GameLib = id match {
    case 1 => Draughts()
    case 2 => LinesOfAction()
    case _ => Chess()
  }
}

