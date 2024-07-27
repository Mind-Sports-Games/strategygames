package strategygames.chess

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
    game: Game,
    fenGame: Game,
    plies: Int = 0,
    turnCount: Int = 1
) {
  def nextPly(newGame: Game, newFenGame: Game): GameFenIsometryData =
    GameFenIsometryData(newGame, newFenGame, nextPlyCount, nextTurnCount(newGame))

  def nextPlyCount = plies + 1

  def nextTurnCount(newGame: Game) =
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
  def fensMustMatch(g1: Game, g2: Game) = {
    val fen1 = Forsyth.>>(g1)
    val fen2 = Forsyth.>>(g2)
    fen1 must_== fen2
  }

  def playerTurnMustMatch(g1: Game, g2: Game) =
    g1.situation.player must_== g2.situation.player

  def legalMovesMustMatch(g1: Game, g2: Game) = {
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

  def gamesAreEqual(g1: Game, g2: Game) = {
    fensMustMatch(g1, g2)
    playerTurnMustMatch(g1, g2)
    legalMovesMustMatch(g1, g2)
    // TODO: add in end game conditions, etc.
  }

  def turnsAndPliesAreEqual(g1: Game, gameData: GameFenIsometryData) = {
    gameData.plies must_== g1.plies
    gameData.turnCount must_== g1.turnCount
  }

  def testEveryMoveLoadFenIsometry(
      initialFen: format.FEN,
      v: variant.Variant,
      basePlies: Int = 0,
      baseTurnCount: Int = 0
  )(moves: (Pos, Pos)*) = {
    fenToGame(initialFen, v).flatMap(game => {
      val gameData = GameFenIsometryData(game, game, basePlies, baseTurnCount).valid
      moves.foldLeft[Validated[String, GameFenIsometryData]](gameData) {
        case (vGame, move) => {
          vGame.flatMap(gameData => {
            for {
              newBaseGame <- gameData.game.playMoves(move)
              newFenGame  <- gameData.fenGame.playMoves(move)
              fen1         = Forsyth.>>(newBaseGame)
              newFenGame2 <- fenToGame(fen1, v)
            } yield {
              val newGameData = gameData.nextPly(newBaseGame, newFenGame)
              turnsAndPliesAreEqual(newBaseGame, newGameData)
              turnsAndPliesAreEqual(newFenGame, newGameData)
              turnsAndPliesAreEqual(newFenGame2, newGameData)
              gamesAreEqual(newBaseGame, newFenGame)
              gamesAreEqual(newBaseGame, newFenGame2)
              gamesAreEqual(newFenGame, newFenGame2)
              newGameData
            }
          })
        }
      }
    })
  }
}
