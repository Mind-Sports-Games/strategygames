package strategygames.abalone.variant

import cats.data.Validated
import cats.syntax.option._
import scala.annotation.nowarn
import scala.collection.mutable.ListBuffer

import strategygames.{ GameFamily, Player }
import strategygames.abalone._
import strategygames.abalone.format.FEN

case class AbaloneName(val name: String)

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


  // private def validMovesFromPos(situation: Situation, pos: Pos): List[Move] = {
  //   val moves = List()
  //   if(pos.downLeft != None && situation.board.pieces.get(pos.downLeft.get) == None) {
  //     moves :: Move()
  //   }
  //   return moves.toList
  // }

        // Move(
        //   piece = Piece(situation.player, Role.defaultRole),
        //   orig = orig,
        //   dest = dest,
        //   situationBefore = situation,
        //   after = boardAfter(situation, Some(orig), Some(dest), die),
        //   capture = situation.board.piecesOf(!situation.player).get(dest).map(_ => dest)
        // )

  // def validMovesComplete(situation: Situation): Map[Pos, List[Move]] = {

  // }

  // we want to have :
  // 1. moves of 1 marble, distinguished from pushes (2 or 3 marbles in the same line)
  // 2. side moves, based on these moves of 1 marbles already generated (can move in side if both neighbours can move in the same direction as 1) does it mean it will be a lazy val ?
  def validMoves(situation: Situation): Map[Pos, List[Move]] = {
    validMovesOf1(situation)

    // return combination of validMovesOf1 + validPushes + validSideMoves (based on validMovesOf1)
  }


  /* 
  Pushes :
      - A B C a b
  from B or C, we can not push towards the right
  A can move towards the right (as a push) because there is enough "weight"
  that means the move is coordsAcoordsa (and then a is moved from coordsa to coordsb + 1 right (off the board in this example)

  when generating side moves, imagine we have the following :
    - - - - -
   - A B C - -
  1 2 3 4 5 6 7
  We want to consider the longest diagonal, that means (coordsOfA, 5) is a side move of 3 marbles but (coordsOfC, 5) means only C will move to 5
  Same for :
    (B, 2) is a side move while (A, 2) or (A, 3) move 1 single marble (the player has to find the correct orig square to unlock the possibility of the correct side move)
  */

    def validMovesOf1(situation: Situation): Map[Pos, List[Move]] = {
    situation.board.pieces.filter(_._2.player == situation.player).map { // for each piece of active player
      case ((pos: Pos, piece: Piece))  => {
        val moves = new ListBuffer[Move]()
        if (pos.downLeft != None && situation.board.pieces.get(pos.downLeft.get) == None) {
          moves += Move(piece, pos, pos.downLeft.get, situation, boardAfter(situation, pos, pos.downLeft.get), false)
        }
        if (pos.left != None && situation.board.pieces.get(pos.left.get) == None) {
          moves += Move(piece, pos, pos.left.get, situation, boardAfter(situation, pos, pos.left.get), false)
        }
        if (pos.upLeft != None && situation.board.pieces.get(pos.upLeft.get) == None) {
          moves += Move(piece, pos, pos.upLeft.get, situation, boardAfter(situation, pos, pos.upLeft.get), false)
        }
        if (pos.upRight != None && situation.board.pieces.get(pos.upRight.get) == None) {
          moves += Move(piece, pos, pos.upRight.get, situation, boardAfter(situation, pos, pos.upRight.get), false)
        }
        if (pos.right != None && situation.board.pieces.get(pos.right.get) == None) {
          moves += Move(piece, pos, pos.right.get, situation, boardAfter(situation, pos, pos.right.get), false)
        }
        if (pos.downRight != None && situation.board.pieces.get(pos.downRight.get) == None) {
          moves += Move(piece, pos, pos.downRight.get, situation, boardAfter(situation, pos, pos.downRight.get), false)
        }
        val movesList = moves.toList
        moves.clear()
        Some((pos, movesList))
      }
    }
    .flatMap {
      case Some((pos: Pos, moves: List[Move])) => Some(pos -> moves)
      case _ => None
    }
    .toMap
  }

  // @TODO: adapt
  // update the score in case a capture has been made
  // take care of generating new board state with new PieceMap and updated score
  // anything else ?
  def boardAfter(situation: Situation, orig: Pos, dest: Pos): Board = {
    // 1. move pieces and get the score
    /*val (pieces1, capture)   =*/ piecesAfterAction(situation.board.pieces, situation.player, orig, dest)

    // 2. update the score
    

    // val pocketsAfterDrop    = orig match {
    //   case None => situation.board.pocketData.flatMap(_.drop(Piece(situation.player, Role.defaultRole)))
    //   case _    => situation.board.pocketData
    // }
    // val pocketsAfterCapture = capture match {
    //   case Some(piece) =>
    //     pocketsAfterDrop.map(_.store(piece))
    //   case None        => pocketsAfterDrop
    // }
    situation.board.copy(
      pieces = pieces,
        // pocketData = pocketsAfterCapture
    )
  }

  // @TODO: adapt
  // will take care of moving the marbles and determine if a capture has been made
  def piecesAfterAction(pieces: PieceMap, player: Player, @nowarn orig: Pos, @nowarn dest: Pos): (PieceMap, Boolean) = {
    var capture = false

    if (true) { // push
      // compute direction between orig and dest to push the opponent marble



      //     val truc = pieces.filter(_._1 != orig)

      // use the direction until we reach an empty space or the edge 
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
