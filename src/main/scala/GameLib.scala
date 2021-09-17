package strategygames

sealed abstract class GameLib {
  def id: Int
  def name: String

  override def toString = s"Lib($name)"
}

object GameLib {

  final case class Chess() extends GameLib {
    def id = 0
    def name = "Chess"
  }

  final case class Draughts() extends GameLib {
    def id = 1
    def name = "Draughts"
  }

  def all = List(Chess, Draughts)

  // TODO: I'm sure there is a better scala way of doing this
  def apply(id: Int): GameLib = id match {
    case 1 => Draughts()
    case _ => Chess()
  }
}

sealed abstract class DisplayLib {
  def id: Int
  def name: String
  def shortName: String
  def codeLib: GameLib

  override def toString = s"DisplayLib($name)"
}

object DisplayLib {

  final case class Chess() extends DisplayLib {
    def id = GameLib.Chess().id
    def name = GameLib.Chess().name
    def shortName = GameLib.Chess().name
    def codeLib = GameLib.Chess()
  }

  final case class Draughts() extends DisplayLib {
    def id = GameLib.Draughts().id
    def name = GameLib.Draughts().name
    def shortName = GameLib.Draughts().name
    def codeLib = GameLib.Draughts()
  }

  final case class LinesOfAction() extends DisplayLib {
    def id = 2
    def name = "Lines Of Action"
    def shortName = "LOA"
    def codeLib = GameLib.Chess()
  }

  def all = List(Chess, Draughts, LinesOfAction)

  // TODO: I'm sure there is a better scala way of doing this
  def apply(id: Int): DisplayLib = id match {
    case 1 => Draughts()
    case 2 => LinesOfAction()
    case _ => Chess()
  }

}
