package strategygames

import cats.syntax.option.none

case class Division(middle: Option[Int], end: Option[Int], plies: Int) {

  def openingSize: Int = middle | plies
  def middleSize: Option[Int] =
    middle.map { m =>
      (end | plies) - m
    }
  def endSize = end.map(plies -)

  def openingBounds = middle.map(0 -> _)
  def middleBounds =
    for {
      m <- middle
      e <- end
    } yield m -> e
  def endBounds = end.map(_ -> plies)
}

object Division {
  val empty = Division(None, None, 0)
}

object Divider {

  //TODO: need to work out how to handle a List of strategygames.Board
  //def apply(lib: GameLib, boards: List[Board]): Division = (lib, boards) match {
  //  case (GameLib.Draughts(), List[Board.Draughts(b)]) => draughts.Divider.apply(b)
  //  case (GameLib.Chess(), List[Board.Chess(b)])       => chess.Divider.apply(b)
  //}

}
