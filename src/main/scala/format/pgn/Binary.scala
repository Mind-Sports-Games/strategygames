package strategygames.format.pgn

import strategygames.{ GameFamily, GameLogic }

object Binary {

  def writeMoves(gf: GameFamily, ms: Iterable[String]) = gf.gameLogic match {
    case GameLogic.Chess()        => strategygames.chess.format.pgn.Binary.writeMoves(ms)
    case GameLogic.Draughts()     => strategygames.draughts.format.pdn.Binary.writeMoves(ms)
    case GameLogic.FairySF()      => strategygames.fairysf.format.pgn.Binary.writeMoves(gf, ms)
    case GameLogic.Samurai()      => strategygames.samurai.format.pgn.Binary.writeMoves(gf, ms)
    case GameLogic.Togyzkumalak() => strategygames.togyzkumalak.format.pgn.Binary.writeMoves(gf, ms)
  }

  def readMoves(gl: GameLogic, bs: List[Byte]) = gl match {
    case GameLogic.Chess()        => strategygames.chess.format.pgn.Binary.readMoves(bs)
    case GameLogic.Draughts()     => strategygames.draughts.format.pdn.Binary.readMoves(bs)
    case GameLogic.FairySF()      => strategygames.fairysf.format.pgn.Binary.readMoves(bs)
    case GameLogic.Samurai()      => strategygames.samurai.format.pgn.Binary.readMoves(bs)
    case GameLogic.Togyzkumalak() => strategygames.togyzkumalak.format.pgn.Binary.readMoves(bs)
  }

  def readMoves(gl: GameLogic, bs: List[Byte], nb: Int) = gl match {
    case GameLogic.Chess()        => strategygames.chess.format.pgn.Binary.readMoves(bs, nb)
    case GameLogic.Draughts()     => strategygames.draughts.format.pdn.Binary.readMoves(bs, nb)
    case GameLogic.FairySF()      => strategygames.fairysf.format.pgn.Binary.readMoves(bs, nb)
    case GameLogic.Samurai()      => strategygames.samurai.format.pgn.Binary.readMoves(bs, nb)
    case GameLogic.Togyzkumalak() => strategygames.togyzkumalak.format.pgn.Binary.readMoves(bs, nb)
  }

}
