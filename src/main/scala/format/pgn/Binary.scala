package strategygames.format.pgn

import strategygames.{ ActionStrs, GameFamily, GameLogic }

object Binary {

  def writeMoves(gf: GameFamily, ms: Iterable[String]) = gf.gameLogic match {
    case GameLogic.Chess()        => strategygames.chess.format.pgn.Binary.writeMoves(ms)
    case GameLogic.Draughts()     => strategygames.draughts.format.pdn.Binary.writeMoves(ms)
    case GameLogic.FairySF()      => strategygames.fairysf.format.pgn.Binary.writeMoves(gf, ms)
    case GameLogic.Samurai()      => strategygames.samurai.format.pgn.Binary.writeMoves(gf, ms)
    case GameLogic.Togyzkumalak() => strategygames.togyzkumalak.format.pgn.Binary.writeMoves(gf, ms)
    case GameLogic.Go()           => strategygames.go.format.pgn.Binary.writeMoves(ms)
    case GameLogic.Backgammon()   => strategygames.backgammon.format.pgn.Binary.writeMoves(ms)
    case GameLogic.Abalone()      => strategygames.abalone.format.pgn.Binary.writeMoves(ms)
    case GameLogic.Dameo()        => strategygames.dameo.format.pdn.Binary.writeMoves(ms)
  }

  def writeActionStrs(gf: GameFamily, ms: ActionStrs) = gf.gameLogic match {
    case GameLogic.Chess()        => strategygames.chess.format.pgn.Binary.writeActionStrs(ms)
    case GameLogic.Draughts()     => strategygames.draughts.format.pdn.Binary.writeActionStrs(ms)
    case GameLogic.FairySF()      => strategygames.fairysf.format.pgn.Binary.writeActionStrs(gf, ms)
    case GameLogic.Samurai()      => strategygames.samurai.format.pgn.Binary.writeActionStrs(gf, ms)
    case GameLogic.Togyzkumalak() => strategygames.togyzkumalak.format.pgn.Binary.writeActionStrs(gf, ms)
    case GameLogic.Go()           => strategygames.go.format.pgn.Binary.writeActionStrs(ms)
    case GameLogic.Backgammon()   => strategygames.backgammon.format.pgn.Binary.writeActionStrs(ms)
    case GameLogic.Abalone()      => strategygames.abalone.format.pgn.Binary.writeActionStrs(ms)
    case GameLogic.Dameo()        => strategygames.dameo.format.pdn.Binary.writeActionStrs(ms)
  }

  def readActionStrs(gl: GameLogic, bs: List[Byte]) = gl match {
    case GameLogic.Chess()        => strategygames.chess.format.pgn.Binary.readActionStrs(bs)
    case GameLogic.Draughts()     => strategygames.draughts.format.pdn.Binary.readActionStrs(bs)
    case GameLogic.FairySF()      => strategygames.fairysf.format.pgn.Binary.readActionStrs(bs)
    case GameLogic.Samurai()      => strategygames.samurai.format.pgn.Binary.readActionStrs(bs)
    case GameLogic.Togyzkumalak() => strategygames.togyzkumalak.format.pgn.Binary.readActionStrs(bs)
    case GameLogic.Go()           => strategygames.go.format.pgn.Binary.readActionStrs(bs)
    case GameLogic.Backgammon()   => strategygames.backgammon.format.pgn.Binary.readActionStrs(bs)
    case GameLogic.Abalone()      => strategygames.abalone.format.pgn.Binary.readActionStrs(bs)
    case GameLogic.Dameo()        => strategygames.dameo.format.pdn.Binary.readActionStrs(bs)
  }

  def readActionStrs(gl: GameLogic, bs: List[Byte], nb: Int) = gl match {
    case GameLogic.Chess()        => strategygames.chess.format.pgn.Binary.readActionStrs(bs, nb)
    case GameLogic.Draughts()     => strategygames.draughts.format.pdn.Binary.readActionStrs(bs, nb)
    case GameLogic.FairySF()      => strategygames.fairysf.format.pgn.Binary.readActionStrs(bs, nb)
    case GameLogic.Samurai()      => strategygames.samurai.format.pgn.Binary.readActionStrs(bs, nb)
    case GameLogic.Togyzkumalak() => strategygames.togyzkumalak.format.pgn.Binary.readActionStrs(bs, nb)
    case GameLogic.Go()           => strategygames.go.format.pgn.Binary.readActionStrs(bs, nb)
    case GameLogic.Backgammon()   => strategygames.backgammon.format.pgn.Binary.readActionStrs(bs, nb)
    case GameLogic.Abalone()      => strategygames.abalone.format.pgn.Binary.readActionStrs(bs, nb)
    case GameLogic.Dameo()        => strategygames.dameo.format.pdn.Binary.readActionStrs(bs, nb)
  }

}
