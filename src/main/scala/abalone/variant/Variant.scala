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

  // looks like this is only to allow King to be a valid promotion piece
  // in just atomic, so can leave as true for now
  def isValidPromotion(@nowarn promotion: Option[PromotableRole]): Boolean = true

  /*
  In Abalone there are 3 kinds of moves.
  Let's use the top 3 rows of a board to illustrate (capital letters are empty squares, 1 is an opponent marble)

         A B C D E
        1 a b c G H 
       I J K L M N O 
    
    - line moves are marbles moving in line to an empty square :
       'a' to 'G' would move three marbles to the right.
         This is the same as if 'a' jumped over 'b' and 'c' to land on 'G'.
       'c' to 'G' would move one marble to the right.
    - pushes are line moves targeting a square hosting an opponent marble
       They will be processed later on as two line moves (one jump per player).
       'c' to '1' and 'b' to '1' are pushes to the left.
       In case we play a move from 'c' to '1', c will land on 1 and 1 will be removed from the board.
       When a piece is pushed off the board, the Move is created with an extra parameter.
    - side moves can only be described starting from the right origin (figure out the longest diagonal) :
       'a' to 'M' is a downRight side move of three marbles.
       'c' to 'J' is a downLeft side move of three marbles.
       'a' to 'L' is a downRight side move of two marbles (only 'a' and 'b' would move).
       'c' to 'K' is a downLeft side move of two marbles (only 'c' and 'b' would move)
       'a' to 'K' or 'c' to 'L' are line moves of a single marble

  For moves of 2 marbles or more, once you get the direction of the line,
   you can easily generate the side moves as being the one before and the one after,
   following a rotation :
    \   /
   -  o  -
    /   \
  e.g. if you are moving upRight the side moves to consider are "upLeft" and "right".

  1. moves of 1 marble
  2. generate any possible pair of marbles and use it to generate moves of 2 and 3 marbles
  then merge these as valid moves.
  */
  def validMoves(situation: Situation): Map[Pos, List[Move]] =
    (validMovesOf1(situation).toList ++ validMovesOf2And3(situation).toList).groupBy(_._1).map{case(k, v) => k -> v.map(_._2).toSeq.flatten}.toMap

  def validMovesOf1(situation: Situation): Map[Pos, List[Move]] =
    turnPieces(situation).flatMap {
      case ((pos, piece)) =>
        Map(pos ->
          pos.neighboursAsDirs.flatMap(d => d(pos)).filterNot(situation.board.pieces.contains(_))
          .map(landingSquare =>
            Move(piece, pos, landingSquare, situation, boardAfter(situation, pos, landingSquare), true)
          )
        )
    }.toMap

  def validMovesOf2And3(situation: Situation): Map[Pos, List[Move]] = {
    val activePlayerPieces = situation.board.piecesOf(situation.player)
    val opponentPieces = situation.board.piecesOf(!situation.player)

    def createMove(orig: Pos, dest: Pos, capture: Boolean = false) = Move(Piece(situation.player, Role.defaultRole), orig, dest, situation, boardAfter(situation, orig, dest), true, if(capture) Some(dest) else None)

    def generateSideMoves(lineOfMarbles: List[Pos], direction: Direction): List[Move] = {
      def canMoveTowards(pos: Pos, direction: Direction): Boolean = situation.board.isEmptySquare(direction(pos))

      def possibleSideMoves: List[(Pos, Pos)] = Pos.sideMovesDirsFromDir(direction).flatMap(
        dir =>
          if (lineOfMarbles.map(
            (pos) => canMoveTowards(pos, dir)
          ).contains(false)) None
          else (
            lineOfMarbles.headOption,
            lineOfMarbles.reverse.headOption.flatMap(dir(_))
          ) match {
            case ( (Some(head), Some(tail)) ) => Some( (head, tail) )
            case _ => None
          }
      )

      List(
        if (lineOfMarbles.size == 3) generateSideMoves(lineOfMarbles.dropRight(1), direction) else List(),
        possibleSideMoves.flatMap {
          case ( (orig, dest) ) => Some(createMove(orig, dest))
          case _ => None
        }
      ).flatten
    }

    def generateMovesForNeighbours(pos: Pos, neighbour: Pos): List[Move] = {
      val direction = Pos.dirFromString(pos.dir(neighbour))
      val moves = List(
        direction(neighbour).toList.flatMap {
          case (thirdSquareInLine) => {
            if (situation.board.isEmptySquare(Some(thirdSquareInLine))) // xx.
              Some(createMove(pos, thirdSquareInLine))
            else if (opponentPieces.contains(thirdSquareInLine)) // xxo
              direction(thirdSquareInLine) match { // xxo?
                case None => Some(createMove(pos, thirdSquareInLine, true)) // xxo\
                case Some(emptySquare) if situation.board.isEmptySquare(Some(emptySquare)) => {
                  Some(createMove(pos, thirdSquareInLine)) // xxo.
                }
                case _ => None
              }
            else if (activePlayerPieces.contains(thirdSquareInLine)) // xxx
              direction(thirdSquareInLine).flatMap {
                case (fourthSquareInLine) => // xxx_
                  if (situation.board.isEmptySquare(Some(fourthSquareInLine))) // xxx.
                    Some(createMove(pos, fourthSquareInLine))
                  else if (opponentPieces.contains(fourthSquareInLine)) // xxxo
                    direction(fourthSquareInLine) match { // xxxo?
                      case None => Some(createMove(pos, fourthSquareInLine, true)) // xxxo\
                      case Some(emptyPos) if (situation.board.isEmptySquare(Some(emptyPos))) => Some(createMove(pos, fourthSquareInLine)) // xxxo.
                      case _ => direction(fourthSquareInLine).flatMap { // xxxo?
                        case (fifthSquareInLine) =>
                          if (opponentPieces.contains(fifthSquareInLine)) // xxxoo
                            direction(fifthSquareInLine) match {
                              case None => Some(createMove(pos, fourthSquareInLine, true)) // xxxoo\
                              case Some(emptySquare) if situation.board.isEmptySquare(Some(emptySquare)) => Some(createMove(pos, emptySquare)) // xxxoo.
                              case _ => None
                            }
                          else None
                      }
                    }
                  else None
              }
            else None
          }
        }
      )

      moves.flatten ++ generateSideMoves(
        List(Some(pos), Some(neighbour), direction(neighbour).flatMap{x => if (activePlayerPieces.contains(x)) Some(x) else None}).flatten,
        direction
      )
    }

    turnPieces(situation).flatMap {
      case (pos, _) =>
        Map(pos -> pos.neighbours.collect {
          case Some(neighbour) if activePlayerPieces.contains(neighbour) => (pos, neighbour)
        }.flatMap {
          case (pos, neighbour) =>
            generateMovesForNeighbours(pos, neighbour)
        }).view.toList
    }
  }

  // move pieces on the board. Other bits (including score) are handled by Move.finalizeAfter()
  def boardAfter(situation: Situation, orig: Pos, dest: Pos): Board = {
    situation.board.copy(pieces = piecesAfterAction(situation.board.pieces, orig, dest))
  }

  /*
    How to move pieces based on orig, dest :
      A. Find direction between orig and dest (in case of a sideMove this will always be "upY" or "downX")
      B. Determine the kind of move (side move or not)
      C. play the move
        - side move :
          Based on the 2 directions computed in A (vertical + horizontal), applied to origin to land on a square,
          find out which square :
            - is the empty square
            - contains the marble used to do a side move
        - else:
          - push :
            - dest contains a marble
              - apply the direction from dest until there is an empty square or being off the board
                - in case of being off the board, increment the score (capture = true)
              - do the line move (orig to dest)
          - line move :
            - move from orig to dest, without considering the direction
  */
  private def piecesAfterAction(pieces: PieceMap, orig: Pos, dest: Pos): (PieceMap) = {
    val origToDestDir: Direction = Pos.dirFromString(orig.dir(dest))

    if (isSideMove(orig, dest)) {
      val lineDir: Option[Direction] = Pos.possibleLineDirsFromSideMoveDir(origToDestDir).flatMap {
        direction => direction(orig) match {
          case (Some(squareInLine)) if(pieces.contains(squareInLine) && pieces(squareInLine).player == pieces(orig).player) => Some(direction)
          case _ => None
        }
      }.headOption
      val sideDir: Option[Direction] = Pos.possibleLineDirsFromSideMoveDir(origToDestDir).flatMap {
        direction => direction(orig) match {
          case (Some(squareInLine)) if(!pieces.contains(squareInLine)) => Some(direction)
          case _ => None
        }
      }.headOption
      val lineDirSecondPos: Option[Pos] = lineDir.flatMap(direction => direction(orig))
      val lineDirThirdPos: Option[Pos] = lineDirSecondPos.flatMap(direction => lineDir.flatMap(_(direction)))
      val sideMovePosFromOrig: Option[Pos] = sideDir.flatMap(direction => direction(orig))
      val diagonalPosFromOrig: Option[Pos] = (lineDir, sideMovePosFromOrig) match {
        case ( Some(someLineDir), Some(origSideMove) ) => someLineDir(origSideMove)
        case _ => None
      }
      (lineDirSecondPos, lineDirThirdPos, sideMovePosFromOrig, diagonalPosFromOrig) match {
        case ( Some(lineDirSecondPos), _, Some(sideMovePosFromOrig), Some(diagonalPosFromOrig) ) if (diagonalPosFromOrig.index == dest.index) => // oo
          pieces +
            (sideMovePosFromOrig -> pieces(orig)) - orig +
            (diagonalPosFromOrig -> pieces(lineDirSecondPos)) - lineDirSecondPos
        case ( Some(lineDirSecondPos), Some(lineDirThirdPos), Some(sideMovePosFromOrig), Some(diagonalPosFromOrig) ) => // ooo
          pieces +
            (sideMovePosFromOrig -> pieces(orig)) - orig +
            (diagonalPosFromOrig -> pieces(lineDirSecondPos)) - lineDirSecondPos +
            (dest -> pieces(lineDirThirdPos)) - lineDirThirdPos
        case _ => pieces
      }
    } else {
      if (pieces.contains(dest)) {
        val destLineMove: Option[Pos]     = origToDestDir(dest)
        val destLineOneMove: Option[Pos]  = destLineMove.flatMap(direction => origToDestDir(direction))
        val destLineTwoMove: Option[Pos]  = destLineOneMove.flatMap(direction => origToDestDir(direction))      
        ( destLineMove, destLineOneMove, destLineTwoMove ) match {
          case ( (None, _, _) ) => pieces + (dest -> pieces(orig)) - orig // __(_)o\
          case ( (Some(destLinePos), _, _) ) if (!pieces.contains(destLinePos)) => pieces + (destLinePos -> pieces(dest)) + (dest -> pieces(orig)) - orig // __(_)o.
          case ( (Some(destLinePos), None, _) ) => pieces + (destLinePos -> pieces(dest)) + (dest -> pieces(orig)) - orig // ___oo\
          case ( (_, _, Some(desLineTwoPos)) ) => pieces + (desLineTwoPos -> pieces(dest)) + (dest -> pieces(orig)) - orig // ___oo.
          case _ => pieces
        }
      } else pieces + (dest -> pieces(orig)) - (orig)
    }
  }

  def move(
      situation: Situation,
      from: Pos,
      to: Pos
  ): Validated[String, Move] = {
    // Find the move in the variant specific list of valid moves !
    situation.moves get from flatMap (_.find(m => m.dest == to)) toValid
      s"Not a valid move: ${from}${to}. Allowed moves: ${situation.moves}"
  }

  // if a player runs out of move, the match is a draw
  def stalemateIsDraw = true

  // @TODO: might want to use this winner method in specialEnd method below ? code is duplicated...
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
  @nowarn def finalizeBoard(board: Board, uci: format.Uci, captured: Option[Piece]): Board = board

  def valid(@nowarn board: Board, @nowarn strict: Boolean): Boolean = true

  def isIrreversible(move: Move): Boolean = move.capture.nonEmpty

  def defaultRole: Role = Role.defaultRole

  def gameFamily: GameFamily

  override def toString = s"Variant($name)"

  override def equals(that: Any): Boolean = this eq that.asInstanceOf[AnyRef]

  override def hashCode: Int = id

  private def isSideMove(orig: Pos, dest: Pos): Boolean = orig.dir(dest) match {
    case dirString if (dirString == "upRight" || dirString == "downLeft") =>
      if (orig.|<>|((square) => square.index == dest.index, Pos.dirFromString(orig.dir(dest))) == Nil) true // if we can't reach dest from orig using only the same direction.
      else false
    case _ => orig.rank.index != dest.rank.index && orig.file.index != dest.file.index
  }

  private def turnPieces(situation: Situation): PieceMap = situation.board.piecesOf(situation.player)

  val kingPiece: Option[Role] = None

  val roles: List[Role] = Role.all

  lazy val rolesByPgn: Map[Char, Role] = roles
    .map { r =>
      (r.pgn, r)
    }
    .to(Map)
}

object Variant {
  def apply(id: Int): Option[Variant]     = byId get id
  def apply(key: String): Option[Variant] = byKey get key
  def orDefault(id: Int): Variant         = apply(id) | default
  def orDefault(key: String): Variant     = apply(key) | default

  def byName(name: String): Option[Variant] =
    all find (_.name.toLowerCase == name.toLowerCase)

  def exists(id: Int): Boolean = byId contains id

  val openingSensibleVariants: Set[Variant] = Set(strategygames.abalone.variant.Abalone)

  val divisionSensibleVariants: Set[Variant] = Set()

  val byId                    = all map { v =>
    (v.id, v)
  } toMap
  val byKey                   = all map { v =>
    (v.key, v)
  } toMap

  val default = Abalone

  lazy val all: List[Variant] = List(
    Abalone
  )
}
