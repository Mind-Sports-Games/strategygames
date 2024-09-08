package strategygames.chess

import strategygames.{ Game => StratGame, GameLogic, MoveMetrics }
import strategygames.format.{ FEN => StratFen, Forsyth => StratForsyth, Uci => StratUci }
import strategygames.variant.{ Variant => StratVariant }

import strategygames.{ ClockBase, Player }

import cats.data.Validated
import cats.syntax.option._
import cats.syntax.validated._
import org.specs2.matcher.Matcher
import org.specs2.matcher.ValidatedMatchers
import org.specs2.mutable.Specification

import strategygames.chess.format.{ Forsyth, Visual }
import strategygames.chess.variant.Variant
import strategygames.chess.format.FEN

case class GameFenIsometryData(
    game: StratGame,
    fenGame: StratGame,
    plies: Int = 0,
    turnCount: Int = 1
) {
  def nextPly(newGame: StratGame, newFenGame: StratGame): GameFenIsometryData =
    GameFenIsometryData(newGame, newFenGame, nextPlyCount, nextTurnCount(newGame))

  def nextPlyCount = plies + 1

  def nextTurnCount(newGame: StratGame) =
    if (game.situation.player != newGame.situation.player) turnCount + 1
    else turnCount
}

trait ChessTest extends Specification with ValidatedMatchers {

  implicit def stringToBoard(str: String): Board = Visual << str

  implicit def stringToBoardBuilder(str: String) =
    new {

      def chess960: Board = makeBoard(str, strategygames.chess.variant.Chess960)

      def kingOfTheHill: Board = makeBoard(str, strategygames.chess.variant.KingOfTheHill)

      def threeCheck: Board = makeBoard(str, strategygames.chess.variant.ThreeCheck)

      def fiveCheck: Board = makeBoard(str, strategygames.chess.variant.FiveCheck)
    }

  implicit def stringToSituationBuilder(str: String) =
    new {

      def as(player: Player): Situation = Situation(Visual << str, player)
    }

  case class RichActor(actor: Actor) {
    def threatens(to: Pos): Boolean =
      actor.piece.eyes(actor.pos, to) && {
        (!actor.piece.role.projection) ||
        actor.piece.role.dir(actor.pos, to).exists {
          Actor.longRangeThreatens(actor.board, actor.pos, _, to)
        }
      }
  }

  implicit def richActor(actor: Actor) = RichActor(actor)

  case class RichGame(game: Game) {
    def as(player: Player): Game = game.withPlayer(player)

    def playMoves(moves: (Pos, Pos)*): Validated[String, Game] = playMoveList(moves)

    def playMoveList(moves: Iterable[(Pos, Pos)]): Validated[String, Game] = {
      val vg = moves.foldLeft(Validated.valid(game): Validated[String, Game]) { (vg, move) =>
        // vg foreach { x =>
        // println(s"------------------------ ${x.turns} = $move")
        // }
        // because possible moves are asked for player highlight
        // before the move is played (on initial situation)
        // vg foreach { _.situation.destinations }
        val ng = vg flatMap { g =>
          g(move._1, move._2) map (_._1)
        }
        ng
      }
      // vg foreach { x => println("========= PGN: " + x.pgnMoves) }
      vg
    }

    def playMove(
        orig: Pos,
        dest: Pos,
        promotion: Option[PromotableRole] = None
    ): Validated[String, Game] =
      game.apply(orig, dest, promotion) map (_._1)

    def withClock(c: ClockBase) = game.copy(clock = Option(c))
  }

  implicit def richGame(game: Game) = RichGame(game)

  def fenToGame(positionString: FEN, variant: Variant) = {
    val situation = Forsyth.<<<@(variant, positionString)
    situation
      .toValid("Could not construct situation from FEN")
      .map(sitPlus =>
        Game(
          situation = sitPlus.situation,
          plies = sitPlus.plies,
          turnCount = sitPlus.turnCount
        )
      )
  }

  def makeBoard(pieces: (Pos, Piece)*): Board =
    Board(pieces toMap, History(), strategygames.chess.variant.Standard)

  def makeBoard(str: String, variant: Variant) =
    Visual << str withVariant variant

  def makeBoard: Board = Board init strategygames.chess.variant.Standard

  def makeEmptyBoard: Board = Board empty strategygames.chess.variant.Standard

  def bePoss(poss: Pos*): Matcher[Option[Iterable[Pos]]] =
    beSome.like { case p =>
      sortPoss(p.toList) must_== sortPoss(poss.toList)
    }

  def makeGame: Game = Game(makeBoard, P1)

  def bePoss(board: Board, visual: String): Matcher[Option[Iterable[Pos]]] =
    beSome.like { case p =>
      Visual.addNewLines(Visual.>>|(board, Map(p -> 'x'))) must_== visual
    }

  def beBoard(visual: String): Matcher[Validated[String, Board]] =
    beValid.like { case b =>
      b.visual must_== (Visual << visual).visual
    }

  def beSituation(visual: String): Matcher[Validated[String, Situation]] =
    beValid.like { case s =>
      s.board.visual must_== (Visual << visual).visual
    }

  def beGame(visual: String): Matcher[Validated[String, Game]] =
    beValid.like { case g =>
      g.board.visual must_== (Visual << visual).visual
    }

  def sortPoss(poss: Seq[Pos]): Seq[Pos] = poss sortBy (_.toString)

  def pieceMoves(piece: Piece, pos: Pos): Option[List[Pos]] =
    (makeEmptyBoard place (piece, pos)) flatMap { b =>
      b actorAt pos map (_.destinations)
    }

  // ------------------------------------------------------------------------------
  // Some test to ensure we can load / save from fen
  def stratFenToGame(lib: GameLogic, positionString: StratFen, variant: StratVariant) = {
    val situation = StratForsyth.<<<@(lib, variant, positionString)
    situation
      .toValid("Could not construct situation from FEN")
      .map(sitPlus =>
        StratGame(
          lib,
          situation = sitPlus.situation,
          plies = sitPlus.plies,
          turnCount = sitPlus.turnCount
        )
      )
  }
  def fensMustMatch(lib: GameLogic, g1: StratGame, g2: StratGame)                     = {
    val fen1 = StratForsyth.>>(lib, g1)
    val fen2 = StratForsyth.>>(lib, g2)
    fen1.toString must_== fen2.toString
  }

  def playerTurnMustMatch(g1: StratGame, g2: StratGame) =
    g1.situation.player must_== g2.situation.player

  def legalMovesMustMatch(g1: StratGame, g2: StratGame) = {
    // Ensure they have the same moves available
    val fromSquares2 = g1.situation.moves.keys.toSet
    val fromSquares3 = g2.situation.moves.keys.toSet
    fromSquares2 must_== fromSquares3
    fromSquares2.foreach(from => {
      g1.situation
        .moves(from)
        .zip(g2.situation.moves(from))
        .foreach(moves => {
          moves._1.orig must_== moves._2.orig
          moves._1.dest must_== moves._2.dest
          moves._1.piece must_== moves._2.piece
        })
    })
  }

  def situationStateIsEqual(g1: StratGame, g2: StratGame) = {
    g1.situation.canDrop must_== g2.situation.canDrop
    g1.situation.canOnlyDrop must_== g2.situation.canOnlyDrop
    g1.situation.canLift must_== g2.situation.canLift
    g1.situation.canOnlyLift must_== g2.situation.canOnlyLift
    g1.situation.canRollDice must_== g2.situation.canRollDice
  }

  def gamesAreEqual(lib: GameLogic, g1: StratGame, g2: StratGame) = {
    fensMustMatch(lib, g1, g2)
    playerTurnMustMatch(g1, g2)
    legalMovesMustMatch(g1, g2)
    situationStateIsEqual(g1, g2)
    // TODO: add in end game conditions, etc.
  }

  def turnsAreEqual(g1: StratGame, gameData: GameFenIsometryData) =
    gameData.turnCount must_== g1.turnCount

  def _testEveryMoveLoadFenIsometry(
      lib: GameLogic,
      initialFen: StratFen,
      v: StratVariant,
      basePlies: Int = 0,
      baseTurnCount: Int = 0
  )(moves: List[StratUci]) = {
    stratFenToGame(lib, initialFen, v).flatMap(game => {
      val gameData = GameFenIsometryData(game, game, basePlies, baseTurnCount).valid
      moves.foldLeft[Validated[String, GameFenIsometryData]](gameData) {
        case (vGame, uci) => {
          vGame.flatMap(gameData => {
            for {
              newBaseGame <- gameData.game.applyUci(uci, MoveMetrics()).map(_._1)
              newFenGame  <- gameData.fenGame.applyUci(uci, MoveMetrics()).map(_._1)
              fen1         = StratForsyth.>>(lib, newBaseGame)
              newFenGame2 <- stratFenToGame(lib, fen1, v)
            } yield {
              val newGameData = gameData.nextPly(newBaseGame, newFenGame)
              turnsAreEqual(newBaseGame, newGameData)
              turnsAreEqual(newFenGame, newGameData)
              turnsAreEqual(newFenGame2, newGameData)
              gamesAreEqual(lib, newBaseGame, newFenGame)
              gamesAreEqual(lib, newBaseGame, newFenGame2)
              gamesAreEqual(lib, newFenGame, newFenGame2)
              newGameData
            }
          })
        }
      }
    })
  }

}
