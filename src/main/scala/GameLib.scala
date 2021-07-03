package strategygames

sealed abstract class GameLib

object GameLib {
  final case class Draughts() extends GameLib
  final case class Chess()    extends GameLib
}
