package chess
package variant

case object Standard
    extends ChessVariant(
      id = 1,
      key = "standard",
      name = "Standard",
      shortName = "Std",
      title = "Standard rules of chess (FIDE)",
      standardInitialPosition = true,
      boardSize = Board.D64
    ) {

  val pieces: Map[Pos, ChessPiece] = ChessVariant.symmetricRank(backRank)
}
