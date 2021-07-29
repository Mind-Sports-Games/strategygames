package chess
package variant

case object FromPosition
    extends ChessVariant(
      id = 3,
      key = "fromPosition",
      name = "From Position",
      shortName = "FEN",
      title = "Custom starting position",
      standardInitialPosition = false,
      boardSize = Board.D64
    ) {

  def pieces = Standard.pieces
}
