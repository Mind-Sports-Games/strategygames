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
    (validMovesOf1(situation).toList ++ validMovesOf2And3(situation).view.mapValues(_.map(_._2)).toList)
      .groupBy(_._1).map{case(k, v) => k -> v.map(_._2).toSeq.flatten}.toMap

  def validMovesOf1(situation: Situation): Map[Pos, List[Move]] =
    turnPieces(situation).flatMap {
      case ((pos, piece)) =>
        Map(pos ->
          pos.neighboursAsDirs.flatMap(d => d(pos)).filterNot(situation.board.pieces.contains(_))
          .map(landingSquare =>
            Move(piece, pos, landingSquare, situation, boardAfter(situation, pos, landingSquare), false)
          )
        )
    }.toMap

  def validMovesOf2And3(situation: Situation): Map[Pos, List[(String, Move)]] = {
    val activePlayerPieces = situation.board.piecesOf(situation.player)
    val opponentPieces = situation.board.piecesOf(!situation.player)

    def createMove(category: String, orig: Pos, dest: Pos, directions: Directions = List()): (String, Move) =
      (category, Move(Piece(situation.player, Role.defaultRole), orig, dest, situation, boardAfter(situation, orig, dest, directions), true, if (category == "pushout") Some(dest) else None))

    def generateSideMoves(lineOfMarbles: List[Pos], direction: Direction): List[(String, Move)] = {
      def canMoveTowards(pos: Pos, direction: Direction): Boolean = situation.board.isEmptySquare(direction(pos))

      def possibleSideMoves: List[(Pos, Pos, Direction)] = Pos.sideMovesDirsFromDir(direction).flatMap(
        dir =>
          if (lineOfMarbles.map(
            (pos) => canMoveTowards(pos, dir)
          ).contains(false)) None
          else (
            lineOfMarbles.headOption,
            lineOfMarbles.reverse.headOption.flatMap(dir(_)),
            dir
          ) match {
            case ( (Some(head), Some(tail), dir) ) => Some( (head, tail, dir) )
            case _ => None
          }
      )

      List(
        if (lineOfMarbles.size == 3) generateSideMoves(lineOfMarbles.dropRight(1), direction) else List(),
        possibleSideMoves.flatMap {
          case ( (orig, dest, dir) ) => Some(createMove("side", orig, dest, List(direction, dir)))
          case _ => None
        }
      ).flatten
    }

    def generateMovesForNeighbours(pos: Pos, neighbour: Pos): List[(String, Move)] = {
      val direction = Pos.dirsFromString(pos.dir(neighbour))
      val moves = List(
        direction(neighbour).toList.flatMap {
          case (thirdSquareInLine) => {
            if (situation.board.isEmptySquare(Some(thirdSquareInLine))) // xx.
              Some(createMove("line", pos, thirdSquareInLine))
            else if (opponentPieces.contains(thirdSquareInLine)) // xxo
              direction(thirdSquareInLine) match { // xxo?
                case None => Some(createMove("pushout", pos, thirdSquareInLine, List(direction))) // xxo\
                case Some(emptySquare) if situation.board.isEmptySquare(Some(emptySquare)) => {
                  Some(createMove("push", pos, thirdSquareInLine, List(direction))) // xxo.
                }
                case _ => None
              }
            else if (activePlayerPieces.contains(thirdSquareInLine)) // xxx
              direction(thirdSquareInLine).flatMap {
                case (fourthSquareInLine) => // xxx_
                  if (situation.board.isEmptySquare(Some(fourthSquareInLine))) // xxx.
                    Some(createMove("line", pos, fourthSquareInLine))
                  else if (opponentPieces.contains(fourthSquareInLine)) // xxxo
                    direction(fourthSquareInLine) match { // xxxo?
                      case None => Some(createMove("pushout", pos, fourthSquareInLine, List(direction))) // xxxo\
                      case Some(emptyPos) if (situation.board.isEmptySquare(Some(emptyPos))) => Some(createMove("push", pos, fourthSquareInLine, List(direction))) // xxxo.
                      case _ => direction(fourthSquareInLine).flatMap { // xxxo?
                        case (fifthSquareInLine) =>
                          if (opponentPieces.contains(fifthSquareInLine)) // xxxoo
                            direction(fifthSquareInLine) match {
                              case None => Some(createMove("pushout", pos, fourthSquareInLine, List(direction))) // xxxoo\
                              case Some(emptySquare) if situation.board.isEmptySquare(Some(emptySquare)) => Some(createMove("push", pos, emptySquare, List(direction))) // xxxoo.
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
  def boardAfter(situation: Situation, orig: Pos, dest: Pos, directions: Directions = List()): Board = {
    situation.board.copy(pieces = piecesAfterAction(situation.board.pieces, orig, dest, directions))
  }

  /*
    How to move pieces based on orig, dest, and directions :
      1. identify the type of move
        - line move : no direction to apply (a line move is just 1 marble moving)
        - push : 1 direction, and dest contains a marble
        - side move : 2 directions
          - HEAD contains the direction to apply to move through all these marbles from orig
          - TAIL contains the direction to apply the direction to apply to each marble for doing effectively the side move

      2. play the move
        - line move :
          - move from orig to dest, without considering the direction
        - push :
          - dest contains a marble
          - we have the direction :
            - apply the direction from dest until there is an empty square or being off the board
              - in case of being off the board, increment the score (capture = true)
            - do the line move (orig to dest)
        - side move :
          - we received both directions :
            - 1st direction : we know how to move through all these marbles
            - 2nd direction : we know what direction we want to apply to all these 2 or 3 marbles
  */
  private def piecesAfterAction(pieces: PieceMap, orig: Pos, dest: Pos, directions: Directions): (PieceMap) = {
    val lineDir: Option[Direction] = directions.headOption
    val sideDir: Option[Direction] = directions.reverse.headOption
    val origLineMove: Option[Pos] = lineDir.map((direction) => direction(orig).get)
    val origSideMove: Option[Pos] = sideDir.map((direction) => direction(orig).get)
    val diagonalMove: Option[Pos] = (lineDir, origSideMove) match {
      case ( Some(lineDir), Some(sideMove) ) => lineDir.apply(sideMove)
      case _ => None
    }

    if (directions.size == 0) // line move
      pieces + (dest -> pieces(orig)) - (orig)
    else if (directions.size == 1) // push
      if(directions(0)(dest) == None) // __(_)o\
        pieces + (dest -> pieces(orig)) - orig
      else if (!pieces.contains(directions(0)(dest).get)) // __(_)o.
        pieces + (directions(0)(dest).get -> pieces(dest)) + (dest -> pieces(orig)) - orig
      else if (directions(0)((directions(0)(dest)).get) == None) // ___oo\
        pieces + ((directions(0)(dest)).get -> pieces(dest)) + (dest -> pieces(orig)) - orig
      else // ___oo.
        pieces + (directions(0)((directions(0)(dest)).get).get -> pieces(dest)) + (dest -> pieces(orig)) - orig
    else // side move
      (origLineMove, origSideMove, diagonalMove)  match {
        case ( Some(lineMovePos), Some(sideMovePos), Some(diagonalPos) ) =>
          if (diagonalPos.index == dest.index) // oo
            pieces + (sideMovePos -> pieces(orig)) - orig +
              (diagonalPos -> pieces(lineMovePos)) - lineMovePos
          else // ooo
            pieces + (sideMovePos -> pieces(orig)) - orig +
              (diagonalPos -> pieces(lineMovePos)) - lineMovePos +
              (directions(1)(lineMovePos).get -> pieces(lineMovePos)) - diagonalPos
        case _ => pieces
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

  def isIrreversible(move: Move): Boolean = move.capture != None

  def defaultRole: Role = Role.defaultRole

  def gameFamily: GameFamily

  override def toString = s"Variant($name)"

  override def equals(that: Any): Boolean = this eq that.asInstanceOf[AnyRef]

  override def hashCode: Int = id

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
