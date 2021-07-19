package strategygames

sealed abstract class GameLib {
  //def white: Color
  //def black: Color
  def id: Int
}

object GameLib {
  final case class Draughts() extends GameLib {
    //def white = Color.Draughts(draughts.White)
    //def black = Color.Draughts(draughts.Black)
    def id = 0
  }
  final case class Chess() extends GameLib {
    //def white = Color.Chess(chess.White)
    //def black = Color.Chess(chess.Black)
    def id = 1
  }
}
