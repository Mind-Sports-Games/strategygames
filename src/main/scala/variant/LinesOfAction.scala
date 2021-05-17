package chess
package variant

import chess.format.FEN

case object LinesOfAction
    extends Variant(
      //went for a high number to avoid potential clashing in the future
      id = 100,
      key = "linesofaction",
      name = "Lines Of Action",
      shortName = "LOA",
      title = "Lines of Action - Connect all of your pieces",
      standardInitialPosition = false
    ) {

  override val pieces: Map[Pos, Piece] = Map(
    Pos.B1 -> Black.queen,
    Pos.C1 -> Black.queen,
    Pos.D1 -> Black.queen,
    Pos.E1 -> Black.queen,
    Pos.F1 -> Black.queen,
    Pos.G1 -> Black.queen,
    Pos.B8 -> Black.queen,
    Pos.C8 -> Black.queen,
    Pos.D8 -> Black.queen,
    Pos.E8 -> Black.queen,
    Pos.F8 -> Black.queen,
    Pos.G8 -> Black.queen,
    Pos.A2 -> White.queen,
    Pos.A3 -> White.queen,
    Pos.A4 -> White.queen,
    Pos.A5 -> White.queen,
    Pos.A6 -> White.queen,
    Pos.A7 -> White.queen,
    Pos.H2 -> White.queen,
    Pos.H3 -> White.queen,
    Pos.H4 -> White.queen,
    Pos.H5 -> White.queen,
    Pos.H6 -> White.queen,
    Pos.H7 -> White.queen,
  )

  //picked queen to at least give us the option in early stages of being able to move pieces in all directions
  override val initialFen = FEN("1qqqqqq1/Q6Q/Q6Q/Q6Q/Q6Q/Q6Q/Q6Q/1qqqqqq1 b - - 0 1")

  override def allowsCastling = false

  override val castles = Castles.none

  //implement
  //override def specialEnd(situation: Situation) =
  //  situation.check

}
