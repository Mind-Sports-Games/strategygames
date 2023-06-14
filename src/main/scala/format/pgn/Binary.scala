package strategygames.format.pgn

import strategygames.{ Actions, GameFamily, GameLogic }

object Binary {

  def writeMoves(gf: GameFamily, ms: Iterable[String]) = gf.gameLogic match {
    case GameLogic.Chess()        => strategygames.chess.format.pgn.Binary.writeMoves(ms)
    case GameLogic.Draughts()     => strategygames.draughts.format.pdn.Binary.writeMoves(ms)
    case GameLogic.FairySF()      => strategygames.fairysf.format.pgn.Binary.writeMoves(gf, ms)
    case GameLogic.Samurai()      => strategygames.samurai.format.pgn.Binary.writeMoves(gf, ms)
    case GameLogic.Togyzkumalak() => strategygames.togyzkumalak.format.pgn.Binary.writeMoves(gf, ms)
  }

  def writeActions(gf: GameFamily, ms: Actions) = gf.gameLogic match {
    case GameLogic.Chess()        => strategygames.chess.format.pgn.Binary.writeActions(ms)
    case GameLogic.Draughts()     => strategygames.draughts.format.pdn.Binary.writeActions(ms)
    case GameLogic.FairySF()      => strategygames.fairysf.format.pgn.Binary.writeActions(gf, ms)
    case GameLogic.Samurai()      => strategygames.samurai.format.pgn.Binary.writeActions(gf, ms)
    case GameLogic.Togyzkumalak() => strategygames.togyzkumalak.format.pgn.Binary.writeActions(gf, ms)
  }

  def readActions(gl: GameLogic, bs: List[Byte]) = gl match {
    case GameLogic.Chess()        => strategygames.chess.format.pgn.Binary.readActions(bs)
    case GameLogic.Draughts()     => strategygames.draughts.format.pdn.Binary.readActions(bs)
    case GameLogic.FairySF()      => strategygames.fairysf.format.pgn.Binary.readActions(bs)
    case GameLogic.Samurai()      => strategygames.samurai.format.pgn.Binary.readActions(bs)
    case GameLogic.Togyzkumalak() => strategygames.togyzkumalak.format.pgn.Binary.readActions(bs)
  }

  def readActions(gl: GameLogic, bs: List[Byte], nb: Int) = gl match {
    case GameLogic.Chess()        => strategygames.chess.format.pgn.Binary.readActions(bs, nb)
    case GameLogic.Draughts()     => strategygames.draughts.format.pdn.Binary.readActions(bs, nb)
    case GameLogic.FairySF()      => strategygames.fairysf.format.pgn.Binary.readActions(bs, nb)
    case GameLogic.Samurai()      => strategygames.samurai.format.pgn.Binary.readActions(bs, nb)
    case GameLogic.Togyzkumalak() => strategygames.togyzkumalak.format.pgn.Binary.readActions(bs, nb)
  }

}
