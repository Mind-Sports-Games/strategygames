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
          pos.neighbours.flatten
          .filterNot(situation.board.pieces.contains(_))
          .map(landingSquare =>
            Move(piece, pos, landingSquare, situation, boardAfter(situation, pos, landingSquare), false)
          )
        )
    }.toMap

  def validMovesOf2And3(situation: Situation): Map[Pos, List[(String, Move)]] = {
    val activePlayerPieces = situation.board.piecesOf(situation.player)
    val opponentPieces = situation.board.piecesOf(!situation.player)

    def createMove(orig: Pos, dest: Pos, category: String): (String, Move) =
      (category, Move(Piece(situation.player, Role.defaultRole), orig, dest, situation, boardAfter(situation, orig, dest), true, if (category == "pushout") Some(dest) else None))

    def generateSideMoves(lineOfMarbles: List[Pos], direction: Option[String]): List[(String, Move)] = {
      def canMoveTowards(pos: Pos, dir: String): Boolean = {
        situation.board.isEmptySquare(pos.dir(dir))
      }

      val possibleSideMovesDirections = Pos.sideMovesDirsFromDir(direction)
      def possibleSideMoves: List[Option[(Pos, Pos)]] = possibleSideMovesDirections.map(
        dir =>
          if (lineOfMarbles.flatMap(
            (pos) => Some(canMoveTowards(pos, dir))
          ).contains(false)) None
          else List(
            lineOfMarbles.headOption,
            lineOfMarbles.reverse.headOption.flatMap(_.dir(dir))
          ) match {
            case List(Some(head), Some(tail)) => Some( (head, tail) )
            case _ => None
          }
      )

      List(
        if (lineOfMarbles.size == 3) generateSideMoves(lineOfMarbles.dropRight(1), direction)
        else None,
        possibleSideMoves.flatMap {
          case Some((orig, dest)) => Some(createMove(orig, dest, "side"))
          case _ => None
        }
      ).flatten
    }

    def generateMovesForNeighbours(pos: Pos, neighbour: Pos): List[(String, Move)] = {
      val direction = pos.dir(neighbour)
      val moves = List(
        neighbour.dir(direction).toList.flatMap {
          case (thirdSquareInLine) => {
            if (situation.board.isEmptySquare(Some(thirdSquareInLine))) // xx.
              Some(createMove(pos, thirdSquareInLine, "line"))
            else if (opponentPieces.contains(thirdSquareInLine)) // xxo
              thirdSquareInLine.dir(direction) match { // xxo?
                case None => Some(createMove(pos, thirdSquareInLine, "pushout")) // xxo\
                case Some(emptySquare) if situation.board.isEmptySquare(Some(emptySquare)) => {
                  Some(createMove(pos, thirdSquareInLine, "push")) // xxo.
                }
                case _ => None
              }
            else if (activePlayerPieces.contains(thirdSquareInLine)) // xxx
              thirdSquareInLine.dir(direction).flatMap {
                case (fourthSquareInLine) => // xxx_
                  if (situation.board.isEmptySquare(Some(fourthSquareInLine))) // xxx.
                    Some(createMove(pos, fourthSquareInLine, "line"))
                  else if (opponentPieces.contains(fourthSquareInLine)) // xxxo
                    fourthSquareInLine.dir(direction) match { // xxxo?
                      case None => Some(createMove(pos, fourthSquareInLine, "pushout")) // xxxo\
                      case Some(emptyPos) if (situation.board.isEmptySquare(Some(emptyPos))) => Some(createMove(pos, fourthSquareInLine, "push")) // xxxo.
                      case _ => fourthSquareInLine.dir(direction).flatMap { // xxxo?
                        case (fifthSquareInLine) =>
                          if (opponentPieces.contains(fifthSquareInLine)) // xxxoo
                            fifthSquareInLine.dir(direction) match {
                              case None => Some(createMove(pos, fourthSquareInLine, "pushout")) // xxxoo\
                              case Some(emptySquare) if situation.board.isEmptySquare(Some(emptySquare)) => Some(createMove(pos, emptySquare, "push")) // xxxoo.
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
        List(Some(pos), Some(neighbour), neighbour.dir(direction).flatMap{x => if (activePlayerPieces.contains(x)) Some(x) else None}).flatten,
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

  // @TODO: would be interesting to pass the direction so we can easily differenciate a side move from a line move (orig meets dest in case of line move)
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

    /*

        * * * * *
       * a A B C 1
      * * * * * 2 *
      How to move pieces based on orig and dest :
        1. identify the type of move :
          - push : dest contains a marble (orig=C, dest=a)
          - line move : orig meets dest thanks to the direction we passed as extra parameter
          - side move : NOT a push or a side move

        2. play the move
          - line move :
              - move from orig to dest
          - push :
            - dest contains a marble
            - we know the direction :
              - apply the direction from dest until there is an empty square or being off the board
                - in case of being off the board, increment the score (capture = true)
              - do the line move (orig to dest)
          - side move :
            - apply the direction to orig and dest : but still have to find the marble in the middle in case of a move of 3 marbles ?
    
      in case of a side move, orig=A and dest=1, how do we determine how to move A and B ?
    
    */

    if (pieces.contains(dest)) { // push
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
