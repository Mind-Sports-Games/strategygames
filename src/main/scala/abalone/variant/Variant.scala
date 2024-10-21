package strategygames.abalone.variant

import cats.data.Validated
import cats.syntax.option._
import scala.annotation.nowarn

import strategygames.{ GameFamily, Player }
import strategygames.abalone._
import strategygames.abalone.format.FEN

// Correctness depends on singletons for each variant ID
abstract class Variant private[variant] (
    val id: Int,
    val key: String,
    val name: String,
    val standardInitialPosition: Boolean,
    val boardSize: Board.BoardSize
) {

  def exotic = true

  def baseVariant: Boolean      = false
  def fenVariant: Boolean       = false
  def hasAnalysisBoard: Boolean = true
  def hasFishnet: Boolean       = false

  def p1IsBetterVariant: Boolean = true
  def blindModeVariant: Boolean  = true

  def materialImbalanceVariant: Boolean = false

  def dropsVariant: Boolean     = false
  def onlyDropsVariant: Boolean = false
  def hasGameScore: Boolean     = true
  def canOfferDraw: Boolean     = true

  def repetitionEnabled: Boolean = true

  def perfId: Int
  def perfIcon: Char

  // pieces, scoreP1, scoreP2, turn, halfMovesSinceLastCapture (triggering condition could be when == 100 && total moves > 50 ? => draw), total moves
  def initialFen: FEN = format.FEN("SS1ss/SSSsss/1SS1ss1/8/9/8/1ss1SS1/sssSSS/ss1SS 0 0 b 0 0") // Belgian Daisy

  def pieces: PieceMap = initialFen.pieces

  def startPlayer: Player = P1

  val kingPiece: Option[Role] = None

  // looks like this is only to allow King to be a valid promotion piece
  // in just atomic, so can leave as true for now
  def isValidPromotion(@nowarn promotion: Option[PromotableRole]): Boolean = true

  /*
  In Abalone there are 3 kinds of moves.
  Let's use the top 2 rows of a board to illustrate (numbers are empty squares, Z is an opponent marble)

        Z a b c 7
       1 2 3 4 5 6
    
    - line moves are marbles moving in line to an empty square :
       'a' to '7' would move three marbles to the right. This is the same as if 'a' jumped over 'b' and 'c' to land on '7'.
       'c' to '7' would move one marble to the right.
    - pushes are line moves targeting a square hosting an opponent marble
       They will be processed as two line moves (one jump per player).
       'c' to 'Z' and 'b' to 'Z' are pushes to the left
    - side moves can only be described starting from the right origin (figure out the longest diagonal) :
       'a' to '5' is a downRight side move of three marbles.
       'c' to '2' is a downLeft side move of three marbles.
       'a' to '4' is a downRight side move of two marbles (only 'a' and 'b' would move).
       'c' to '3' is a downLeft side move of two marbles (only 'c' and 'b' would move)
       'a' to '3' or 'c' to '4' are line moves of a single marble

  we want to have :
  1. moves of 1 marble
  2. moves of 2 marbles, using moves of 1
  3. moves of 3 marbles, using moves of 2
  then merge these as valid moves.
  */
  def validMoves(situation: Situation): Map[Pos, List[Move]] = {
    val movesOf1 = validMovesOf1(situation)
    val lineMoves = movesOf1 ++ validLineMoves(situation).map {
      case (k, v) => k -> (v ++ movesOf1.getOrElse(k, Iterable.empty))
    }
    val lineMovesAndPushes = lineMoves ++ validPushes(situation).map {
      case (k, v) => k -> (v ++ lineMoves.getOrElse(k, Iterable.empty))
    }
    lineMovesAndPushes ++ validSideMoves(situation).map { // @TODO: this should reuse what was computed in validMovesOf1
      case (k, v) => k -> (v ++ lineMovesAndPushes.getOrElse(k, Iterable.empty))
    }
  }

  def validMovesOf1(situation: Situation): Map[Pos, List[Move]] =
    situation.board.piecesOf(situation.player).flatMap {
      case ((pos, piece)) =>
        Map(pos ->
          pos.neighbours.flatten
          .filterNot(situation.board.pieces.contains(_))
          .map(landingSquare => 
            Move(piece, pos, landingSquare, situation, boardAfter(situation, pos, landingSquare), false)
          )
        )
    }.toMap

  def validMovesOf2(situation: Situation, validMoves1: Map[Pos, List[Move]]): Map[Pos, List[(String, Move)]] =
    situation.board.piecesOf(situation.player).flatMap {
      case ((pos, piece)) => {
        Map(
          pos -> pos.neighbours.flatMap {
            case Some(neighbour) if(situation.board.piecesOf(situation.player).contains(neighbour)) => {
              neighbour.neighbours.flatMap {
                case Some(neighbourOfNeighbour)
                  if (situation.board.isEmptySquare(Some(neighbourOfNeighbour))
                  && (pos.isInLine(neighbour, neighbourOfNeighbour) || situation.board.isEmptySquare(pos.dir(neighbour.dir(neighbourOfNeighbour))))
                  && !validMoves1.get(pos).toList.flatMap(_.map(_.toUci)).exists(m => m.toString == s"Move(${pos},${neighbourOfNeighbour},None)")) =>
                    Some( ("nopush", Move(piece, pos, neighbourOfNeighbour, situation, boardAfter(situation, neighbour, neighbourOfNeighbour), false)) )
                case Some(neighbourOfNeighbour)
                  if (pos.isInLine(neighbour, neighbourOfNeighbour) && situation.board.piecesOf(!situation.player).contains(neighbourOfNeighbour)) =>
                    if (neighbourOfNeighbour.dir(pos.dir(neighbour)) == None)
                      Some( ("pushout", Move(piece, pos, neighbourOfNeighbour, situation, boardAfter(situation, neighbour, neighbourOfNeighbour), false, neighbour.dir(pos.dir(neighbour)))) )
                    else
                      if (situation.board.isEmptySquare(neighbourOfNeighbour.dir(pos.dir(neighbour))))
                        Some( ("push", Move(piece, pos, neighbourOfNeighbour, situation, boardAfter(situation, neighbour, neighbourOfNeighbour), false)) )
                      else None
                case _ => None
              }
            }
            case _ => None
          }
        )
      }
    }.toMap

  // def validMovesOf3(situation: Situation, @nowarn validMoves2: Map[Pos, List[(String, Move)]]): Map[Pos, List[(String, Move)]] =
  //   situation.board.piecesOf(situation.player).flatMap {
  //     case ((pos, piece)) => {
  //       Map(
  //         pos -> pos.neighbours.flatMap {
  //           case Some(neighbour)
  //             if(situation.board.piecesOf(situation.player).contains(neighbour)
  //             && (neighbour.dir(pos.dir(neighbour).getOrElse("")) != None)
  //             && situation.board.piecesOf(situation.player).contains((neighbour.dir(pos.dir(neighbour).getOrElse("")).get))
  //           ) => { // in case we could potentially have a 3rd marble...
  //             // if (neighbour.dir(pos.dir(neighbour)).getOrElse("") != None) {
  //               val thirdMarblePos = neighbour.dir(pos.dir(neighbour).getOrElse("")).get
  //               thirdMarblePos.neighbours.flatMap {
  //                 case Some(neighbourOf3rdMarble)
  //                   if (
  //                     !situation.board.pieces.contains(neighbourOf3rdMarble)

  //                     // && !validMoves2.contains()
  //                   ) => { // line or side move, but we need to remove the 2 marbles moves
  //                   // if (neighbour)
  //                   Some(
  //                     (
  //                       "lineOrSide",
  //                       Move(
  //                         situation.board.pieces.getOrElse(
  //                           pos,
  //                           Piece(!piece.player, piece.role)
  //                         ),
  //                         pos,
  //                         neighbourOf3rdMarble,
  //                         situation,
  //                         situation.board.variant.boardAfter(situation, neighbour, neighbourOf3rdMarble),
  //                         false
  //                       )
  //                     )
  //                   )
  //                 }
  //                 case Some(neighbourOf3rdMarble) if (situation.board.piecesOf(!situation.player).contains(neighbourOf3rdMarble)) => { // push
  //                   Some(
  //                     (
  //                       "push",
  //                       Move(
  //                         situation.board.pieces.getOrElse(
  //                           pos,
  //                           Piece(!piece.player, piece.role)
  //                         ),
  //                         pos,
  //                         neighbourOf3rdMarble,
  //                         situation,
  //                         situation.board.variant.boardAfter(situation, neighbour, neighbourOf3rdMarble),
  //                         false
  //                       )
  //                     )
  //                   )
  //                 }
  //                 case _ => None
  //               // }
  //             }
  //           }
  //           case _ => None
  //         }
  //       )
  //     }
  //   }


  // def validMovesOf3(situation: Situation, validMoves2: Map[Pos, List[Move]], validMoves1: Map[Pos, List[Move]]): Map[Pos, List[Move]] =
  //   situation.board.piecesOf(situation.player).flatMap {
  //     case ((pos, piece)) => {
  //       Map(
  //         pos -> pos.neighbours.flatMap {
  //           case Some(neighbour) if(situation.board.piecesOf(situation.player).contains(neighbour)) => {
  //             neighbour.neighbours.flatMap {
  //               case Some(neighbourOfNeighbour) => {
  //                 if (
  //                   !situation.board.pieces.contains(neighbourOfNeighbour)
  //                   && pos.dir(neighbour.dir(neighbourOfNeighbour).getOrElse("")) != None
  //                   && (
  //                     (
  //                       !situation.board.piecesOf(!situation.player).contains(pos.dir(neighbour.dir(neighbourOfNeighbour).getOrElse("")).getOrElse(pos))
  //                       && !situation.board.piecesOf(situation.player).contains(pos.dir(neighbour.dir(neighbourOfNeighbour).getOrElse("")).getOrElse(pos))
  //                     )
  //                     || pos.dir(neighbour) == neighbour.dir(neighbourOfNeighbour) // line moves
  //                   )
  //                   && !(validMoves1.get(pos).get.contains(
  //                     Move(
  //                       situation.board.pieces.getOrElse(
  //                         pos,
  //                         Piece(
  //                           !piece.player, piece.role)
  //                       ),
  //                       pos,
  //                       neighbourOfNeighbour,
  //                       situation,
  //                       situation.board.variant.boardAfter(situation, neighbour, neighbourOfNeighbour),
  //                       false
  //                 )))
  //                 ) {
  //                   Some(move)
  //                 }
  //               }
  //             }
  //           }
  //         }
  //       )
  //     }
  //   }


  def validSideMoves(@nowarn situation: Situation):  Map[Pos, List[Move]] = {
    Map()
  }

  def validLineMoves(@nowarn situation: Situation):  Map[Pos, List[Move]] = {
    Map()
  }
    

  def validPushes(@nowarn situation: Situation):  Map[Pos, List[Move]] = {
    Map()
  }

  def boardAfter(situation: Situation, orig: Pos, dest: Pos): Board = {
    // 1. move pieces and get the score
    val (pieces, capture)   = piecesAfterAction(situation.board.pieces, situation.player, orig, dest)

    // 2. update the board
    situation.board.copy(
      pieces = pieces,
      history = History(
        situation.history.currentTurn,
        situation.history.currentTurn, // @TODO: fix this, understand how to create the new Uci with the move being played
        situation.history.positionHashes,
        if (capture) situation.history.score.add(situation.player) else situation.history.score,
        situation.history.halfMoveClock + 1
      )
    )
  }

  // @TODO: adapt
  // will take care of moving the marbles and determine if a capture has been made
  def piecesAfterAction(pieces: PieceMap, player: Player, @nowarn orig: Pos, dest: Pos): (PieceMap, Boolean) = {
    var capture = false

    if (pieces.contains(dest)) { // push
      // compute direction between orig and dest to push the opponent marble

      // apply that direction from orig, until we find an empty square or get out of the board (considering a max distance of 3 from dest)

      // then move the 2 marbles :
      // pieces(finalDest) = pieces(dest)
      // pieces(dest) = pieces(orig)
      // pieces(orig) = None
      capture = true
    }
    if (player == P1)
      (pieces, capture)
    else
      (pieces, capture)
  }

  def move(
      situation: Situation,
      from: Pos,
      to: Pos,
      promotion: Option[PromotableRole] // @TODO: try to see if it can be removed, check if it needs an update on the wrapperLayer. Not mandatory though
  ): Validated[String, Move] = {
    // Find the move in the variant specific list of valid moves !
    situation.moves get from flatMap (_.find(m => m.dest == to && m.promotion == promotion)) toValid
      s"Not a valid move: ${from}${to} with prom: ${promotion}. Allowed moves: ${situation.moves}"
  }

  // if a player runs out of move, the match is a draw
  def stalemateIsDraw = true

  def winner(situation: Situation): Option[Player] = {
    if (situation.board.history.score.p1 == 6) Some(P1)
    else if (situation.board.history.score.p2 == 6) Some(P2)
    else None
  }

  def specialEnd(situation: Situation) =
    (situation.board.history.score.p1 == 6) ||
      (situation.board.history.score.p2 == 6)

  def specialDraw(situation: Situation) = situation.moves.size == 0

  // TODO Abalone Set
  def materialImbalance(@nowarn board: Board): Int = 0

  // Some variants have an extra effect on the board on a move. For example, in Atomic, some
  // pieces surrounding a capture explode
  def hasMoveEffects = false

  def addVariantEffect(move: Move): Move = move

  /** Once a move has been decided upon from the available legal moves, the board is finalized
    */
  @nowarn def finalizeBoard(board: Board, uci: format.Uci, captured: Option[Piece]): Board =
    board

  // TODO: Abalone. Add some sensible validation checks here if appropriate
  def valid(@nowarn board: Board, @nowarn strict: Boolean): Boolean = true

  val roles: List[Role] = Role.all

  lazy val rolesByPgn: Map[Char, Role] = roles
    .map { r =>
      (r.pgn, r)
    }
    .to(Map)

  override def toString = s"Variant($name)"

  override def equals(that: Any): Boolean = this eq that.asInstanceOf[AnyRef]

  override def hashCode: Int = id

  def defaultRole: Role = Role.defaultRole

  def gameFamily: GameFamily
}

object Variant {

  lazy val all: List[Variant] = List(
    Abalone
  )
  val byId                    = all map { v =>
    (v.id, v)
  } toMap
  val byKey                   = all map { v =>
    (v.key, v)
  } toMap

  val default = Abalone

  def apply(id: Int): Option[Variant]     = byId get id
  def apply(key: String): Option[Variant] = byKey get key
  def orDefault(id: Int): Variant         = apply(id) | default
  def orDefault(key: String): Variant     = apply(key) | default

  def byName(name: String): Option[Variant] =
    all find (_.name.toLowerCase == name.toLowerCase)

  def exists(id: Int): Boolean = byId contains id

  val openingSensibleVariants: Set[Variant] = Set(strategygames.abalone.variant.Abalone)

  val divisionSensibleVariants: Set[Variant] = Set()

}
