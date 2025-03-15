package strategygames.abalone

import strategygames.abalone.util.geometry.Cell

final case class AActor(
                         piece: Piece,
                         pos: Cell,
                         board: BBoard
                       )
