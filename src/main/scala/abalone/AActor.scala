package strategygames.abalone

import strategygames.abalone.geometry.Cell

final case class AActor(
                         piece: Piece,
                         pos: Cell,
                         board: BBoard
                       )
