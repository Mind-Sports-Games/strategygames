package abalone

import abalone.util.geometry.Cell
import strategygames.abalone.Piece

final case class AActor(
                         piece: Piece,
                         pos: Cell,
                         board: BBoard
                       )
