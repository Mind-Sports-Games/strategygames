package strategygames.chess.variant

case object FromPosition
    extends Variant(
      id = 3,
      key = "fromPosition",
      name = "From Position",
      shortName = "FEN",
      title = "Custom starting position",
      standardInitialPosition = false
    ) {

  def perfId: Int    = Standard.perfId
  def perfIcon: Char = Standard.perfIcon

  def pieces = Standard.pieces

}
