package strategygames.fairysf

import org.playstrategy.FairyStockfish

import cats.implicits._

import strategygames.{ GameFamily, Player, Pocket, Pockets }
import strategygames.fairysf.format.{ FEN, Uci }
import strategygames.fairysf.variant.Variant

sealed abstract class GameResult extends Product with Serializable

object GameResult {
  final case class Checkmate()  extends GameResult
  final case class Stalemate()  extends GameResult
  final case class Perpetual()  extends GameResult
  final case class Draw()       extends GameResult
  final case class VariantEnd() extends GameResult
  final case class Ongoing()    extends GameResult

  def resultFromInt(value: Int, check: Boolean): GameResult =
    if (value.abs == 32000)
      if (check) GameResult.Checkmate()
      else GameResult.Stalemate()
    else if (value == 0) GameResult.Draw()
    else sys.error(s"Unknown game result: ${value}")

  def optionalResultFromInt(value: Int): GameResult =
    if (value == 32000) GameResult.Perpetual()
    else GameResult.Ongoing()

}

object Api {
  // This will always be called when we import this module. So as long as we directly use
  // this package for calling everything, it should ensure that it's always initialized
  FairyStockfish.init()
  FairyStockfish.loadVariantConfig(ApiVariantConfig.config)

  abstract class Position {
    val variant: Variant

    def makeMoves(movesList: List[String]): Position
    val fen: FEN
    val givesCheck: Boolean
    val isImmediateGameEnd: (Boolean, GameResult)
    val immediateGameEnd: Boolean
    val optionalGameEnd: Boolean
    val insufficientMaterial: (Boolean, Boolean)

    def isDraw(ply: Int): Boolean
    def hasGameCycle(ply: Int): Boolean
    val hasRepeated: Boolean

    val pieceMap: PieceMap
    val piecesInHand: Array[Piece]
    val pocketData: Option[PocketData]

    val optionalGameEndResult: GameResult
    val gameResult: GameResult
    val gameEnd: Boolean
    val legalMoves: Array[String]
  }

  private class FairyPosition(position: FairyStockfish.Position) extends Position {
    // TODO: yes, this is an abuse of scala. We could get an
    //       exception here, but I'm not sure how to work around that
    //       at the moment
    // NOTE: this means we can't use this API to test chess related things
    //       only the variants we support
    val variant = Variant.byFishnetKey(position.variant())

    def makeMoves(movesList: List[String]): Position =
      if (movesList.isEmpty) this
      else new FairyPosition(position.makeMoves(movesList))

    lazy val fen: FEN                                  = FEN(position.getFEN().replace("*", "p"))
    lazy val givesCheck: Boolean                       = position.givesCheck()
    lazy val isImmediateGameEnd: (Boolean, GameResult) = {
      val im = position.isImmediateGameEnd()
      (im.get0(), GameResult.resultFromInt(im.get1(), givesCheck))
    }
    lazy val immediateGameEnd: Boolean                 = isImmediateGameEnd._1
    private lazy val isOptionalGameEnd                 = position.isOptionalGameEnd()
    lazy val optionalGameEnd: Boolean                  = isOptionalGameEnd.get0()
    lazy val insufficientMaterial: (Boolean, Boolean)  = {
      val im = position.hasInsufficientMaterial()
      (im.get0(), im.get1())
    }

    def isDraw(ply: Int): Boolean       = position.isDraw(ply)
    def hasGameCycle(ply: Int): Boolean = position.hasGameCycle(ply)
    lazy val hasRepeated: Boolean       = position.hasRepeated()

    lazy val pieceMap: PieceMap =
      convertPieceMap(position.piecesOnBoard(), position.wallsOnBoard(), variant.gameFamily)

    lazy val piecesInHand: Array[Piece] =
      vectorOfPiecesToPieceArray(position.piecesInHand(), variant.gameFamily)

    lazy val pocketData =
      if (variant.dropsVariant)
        PocketData(
          Pockets(
            Pocket(
              piecesInHand.filter(_.player == P1).toList.map(p => strategygames.Role.FairySFRole(p.role))
            ),
            Pocket(
              piecesInHand.filter(_.player == P2).toList.map(p => strategygames.Role.FairySFRole(p.role))
            )
          ),
          // Can make an empty Set of Pos because we dont have to track promoted pieces
          // FairySF takes care of this for us
          Set[Pos]()
        ).some
      else None

    lazy val optionalGameEndResult: GameResult =
      if (variant.useFairyOptionalGameEnd && isOptionalGameEnd.get0())
        GameResult.optionalResultFromInt(isOptionalGameEnd.get1())
      else GameResult.Ongoing()

    lazy val gameResult: GameResult =
      if (legalMoves.size == 0)
        GameResult.resultFromInt(position.gameResult, givesCheck)
      else optionalGameEndResult

    lazy val gameEnd: Boolean =
      gameResult != GameResult.Ongoing() ||
        insufficientMaterial == ((true, true))

    lazy val legalMoves: Array[String] = position.getLegalMoves()
  }

  def positionFromVariant(variant: Variant): Position =
    new FairyPosition(new FairyStockfish.Position(variant.fishnetKey))

  def positionFromVariantName(variantName: String): Position =
    new FairyPosition(new FairyStockfish.Position(variantName))

  private def removePockets(fen: String): String = {
    val start = fen.indexOf("[", 0)
    val end   = fen.indexOf("]", start)
    if (start > 0 && end > 0)
      fen.substring(0, start) + fen.substring(end + 1, fen.length)
    else fen
  }

  private def fullPockets: String = s"[${"P" * 46}${"p" * 46}]"

  private def addAmazonPockets(fen: String): String = {
    removePockets(fen).split(" ").toList match {
      case first :: rest => (List(first, fullPockets).mkString("") +: rest).mkString(" ")
      case all           => all.mkString(" ")
    }
  }

  private def toFairySFAmazonArrowsFen(fen: String): String =
    fen.replace("p", "*").replace("P", "*")

  private def fromFairySFAmazonArrowsFen(fen: String): String =
    fen.replace("*", "P")

  def toFairySFFen(variantName: String, fen: String): String =
    if (variantName == "amazons") toFairySFAmazonArrowsFen(removePockets(fen))
    else fen

  def fromFairySFFen(variantName: String, fen: String): String =
    if (variantName == "amazons") fromFairySFAmazonArrowsFen(addAmazonPockets(fen))
    else fen

  def positionFromVariantNameAndFEN(variantName: String, fen: String): Position =
    new FairyPosition(new FairyStockfish.Position(variantName, toFairySFFen(variantName, fen)))

  def positionFromVariantAndMoves(variant: Variant, uciMoves: List[String]): Position =
    positionFromVariant(variant).makeMoves(uciMoves)

  val emptyMoves = new FairyStockfish.VectorOfStrings()

  // Some implicits to make the below methods a bit more ergonomic
  implicit def bytePointerToString(bp: org.bytedeco.javacpp.BytePointer): String =
    bp.getString()

  implicit def vectorOfStringFromOptionalMovesList(
      movesList: Option[List[String]] = None
  ): FairyStockfish.VectorOfStrings =
    movesList.map(intoVector).getOrElse(emptyMoves)

  implicit def intoVector(l: List[String]): FairyStockfish.VectorOfStrings = {
    val vec = new FairyStockfish.VectorOfStrings()
    l.foreach(s => {
      vec.push_back(s);
    })
    vec
  }

  implicit def intoArray(vos: FairyStockfish.VectorOfStrings): Array[String] =
    vos.get().map(_.getString())

  private def pieceFromFSPiece(piece: FairyStockfish.Piece, gf: GameFamily): Piece =
    Piece(
      Player.all(piece.color()),
      if (piece.promoted) Role.promotionMap(Role.sfPieceToRole((piece.id(), gf.id)))
      else Role.sfPieceToRole((piece.id(), gf.id))
    )

  def vectorOfPiecesToPieceArray(pieces: FairyStockfish.VectorOfPieces, gf: GameFamily): Array[Piece] =
    pieces.get().map(pieceFromFSPiece(_, gf))

  private def convertPieceMap(
      fsPieceMap: FairyStockfish.PieceMap,
      fsWallMap: FairyStockfish.WallMap,
      gf: GameFamily
  ): PieceMap = {
    var first    = fsPieceMap.begin()
    val end      = fsPieceMap.end()
    val pieceMap = scala.collection.mutable.Map[Pos, Piece]()
    while (!first.equals(end)) {
      pieceMap(
        Pos.fromFairy(first.first()).get
      ) = pieceFromFSPiece(first.second(), gf)
      first = first.increment()
    }
    // Add in the walls if this game family has them.
    wallPiece(gf).map(wall => {
      var firstWall = fsWallMap.begin()
      val endWall   = fsWallMap.end()
      while (!firstWall.equals(endWall)) {
        pieceMap(Pos.fromFairy(firstWall.first()).get) = wall
        firstWall = firstWall.increment()
      }
    })
    pieceMap.toMap
  }

  private def wallPiece(gf: GameFamily): Option[Piece] = gf match {
    case GameFamily.Amazons() => Piece.fromChar(gf, 'P')
    case _                    => None
  }

  def lilaUciToFairyUci(movesList: Option[List[String]]): Option[List[String]] =
    movesList.map(
      _.map(m =>
        m match {
          case Uci.Move.moveP(orig, dest, promotion) =>
            promotion match {
              case "" => m
              case _  => s"${orig}${dest}+"
            }
          case s: String                             => s
        }
      )
    )

  private val fairyPromotion = "^(.*)\\+$".r

  def singleFairyUciToLilaUci(m: String, position: Position): String = {
    m match {
      case fairyPromotion(baseUci) => {
        Uci(position.variant.gameFamily, baseUci) match {
          case Some(baseMove: Uci.Move) => {
            f"${baseUci}${position.pieceMap(baseMove.orig).forsyth}"
          }
          case _                        => m
        }
      }
      case _                       => m
    }
  }

  def fairyUciToLilaUci(variant: Variant, movesList: Option[List[String]]): Option[List[String]] = {
    movesList.map(moveList => {
      var position = positionFromVariant(variant)
      moveList.map(m => {
        val newUci = singleFairyUciToLilaUci(m, position)
        position = position.makeMoves(List(m))
        newUci
      })
    })
  }

  // Actual API wrapper
  def version: String = FairyStockfish.version()

  def info(): Unit = FairyStockfish.info()

  def availableVariants(): Array[String] = FairyStockfish.availableVariants()

  def initialFen(variantName: String): FEN = FEN(
    fromFairySFFen(variantName, FairyStockfish.initialFen(variantName))
  )

  def validateFEN(variantName: String, fen: String): Boolean =
    FairyStockfish.validateFEN(variantName, toFairySFFen(variantName, fen))

  def positionFromMoves(variantName: String, fen: String, movesList: Option[List[String]] = None): Position =
    positionFromVariantNameAndFEN(variantName, fen)
      .makeMoves(lilaUciToFairyUci(movesList).getOrElse(List.empty))

  def pieceMapFromFen(variantName: String, fen: String): PieceMap =
    positionFromMoves(variantName, fen).pieceMap

}

object ApiVariantConfig {

  val config = """
[ps-flipersi]
immobile = p
startFen = 8/8/8/8/8/8/8/8[PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPpppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppp] w 0 1
pieceDrops = true
promotionPieceTypes = -
doubleStep = false
castling = false
stalemateValue = loss
stalematePieceCount = true
materialCounting = unweighted
enclosingDrop = reversi
enclosingDropStart = d4 e4 d5 e5
immobilityIllegal = false
flipEnclosedPieces = reversi
passOnStalemate = false
[ps-flipello:ps-flipersi]
startFen = 8/8/8/3pP3/3Pp3/8/8/8[PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPpppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppp] w 0 1
passOnStalemate = true
[ps-flipello10:ps-flipello]
maxRank = 10
maxFile = 10
startFen = 10/10/10/10/4pP4/4Pp4/10/10/10/10[PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPpppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppp] w 0 1
enclosingDropStart = e5 f5 e6 f6
[ps-antiflipello:ps-flipello]
startFen = 8/8/8/3pP3/3Pp3/8/8/8[PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPpppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppp] w 0 1
extinctionValue = win
[ps-octagonflipello]
maxRank = 10
maxFile = 10
immobile = p
immobilityIllegal = false
startFen = 10/10/10/10/4pP4/4Pp4/10/10/10/10[PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPpppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppp] w 0 1
pieceDrops = true
promotionPieceTypes = -
doubleStep = false
castling = false
stalemateValue = loss
stalematePieceCount = true
materialCounting = unweighted
enclosingDrop = reversi
whiteDropRegion = c1 d1 e1 f1 g1 h1 b2 c2 d2 e2 f2 g2 h2 i2 *3 *4 *5 *6 *7 *8 b9 c9 d9 e9 f9 g9 h9 i9 c10 d10 e10 f10 g10 h10
blackDropRegion = c1 d1 e1 f1 g1 h1 b2 c2 d2 e2 f2 g2 h2 i2 *3 *4 *5 *6 *7 *8 b9 c9 d9 e9 f9 g9 h9 i9 c10 d10 e10 f10 g10 h10
flipEnclosedPieces = reversi
passOnStalemate = true
[ps-minibreakthrough:breakthrough]
maxFile = 5
maxRank = 5
startFen = ppppp/ppppp/5/PPPPP/PPPPP w - - 0 1
flagRegionWhite = *5
flagRegionBlack = *1
[ps-xiangqi:xiangqi]
nFoldRule = 4
[ps-minixiangqi:minixiangqi]
nFoldRule = 4
  """
//Octagon Flipello attempts:
//dropRegionWhite = c1 d1 e1 f1 g1 h1 b2 c2 d2 e2 f2 g2 h2 i2 *3 *4 *5 *6 *7 *8 b9 c9 d9 e9 f9 g9 h9 i9 c10 d10 e10 f10 g10 h10
//dropRegionBlack = c1 d1 e1 f1 g1 h1 b2 c2 d2 e2 f2 g2 h2 i2 *3 *4 *5 *6 *7 *8 b9 c9 d9 e9 f9 g9 h9 i9 c10 d10 e10 f10 g10 h10
//dropRegionWhite = a3 b3 c3 d3 e3 f3 g3 h3 i3 j3 a4 b4 c4 d4 e4 f4 g4 h4 i4 j4 a5 b5 c5 d5 g5 h5 i5 j5 a6 b6 c6 d6 g6 h6 i6 j6 a7 b7 c7 d7 e7 f7 g7 h7 i7 j7 a8 b8 c8 d8 e8 f8 g8 h8 i8 j8 b2 c1 c2 d1 d2 e1 e2 f1 f2 g1 g2 h1 h2 i2 b9 c9 c10 d9 d10 e9 e10 f9 f10 g9 g10 h9 h10 i9
//dropRegionBlack = a3 b3 c3 d3 e3 f3 g3 h3 i3 j3 a4 b4 c4 d4 e4 f4 g4 h4 i4 j4 a5 b5 c5 d5 g5 h5 i5 j5 a6 b6 c6 d6 g6 h6 i6 j6 a7 b7 c7 d7 e7 f7 g7 h7 i7 j7 a8 b8 c8 d8 e8 f8 g8 h8 i8 j8 b2 c1 c2 d1 d2 e1 e2 f1 f2 g1 g2 h1 h2 i2 b9 c9 c10 d9 d10 e9 e10 f9 f10 g9 g10 h9 h10 i9
// whiteDropRegion = a3 b3 c3 d3 e3 f3 g3 h3 i3 j3 a4 b4 c4 d4 e4 f4 g4 h4 i4 j4 a5 b5 c5 d5 g5 h5 i5 j5 a6 b6 c6 d6 g6 h6 i6 j6 a7 b7 c7 d7 e7 f7 g7 h7 i7 j7 a8 b8 c8 d8 e8 f8 g8 h8 i8 j8 b2 c1 c2 d1 d2 e1 e2 f1 f2 g1 g2 h1 h2 i2 b9 c9 c10 d9 d10 e9 e10 f9 f10 g9 g10 h9 h10 i9
// blackDropRegion = a3 b3 c3 d3 e3 f3 g3 h3 i3 j3 a4 b4 c4 d4 e4 f4 g4 h4 i4 j4 a5 b5 c5 d5 g5 h5 i5 j5 a6 b6 c6 d6 g6 h6 i6 j6 a7 b7 c7 d7 e7 f7 g7 h7 i7 j7 a8 b8 c8 d8 e8 f8 g8 h8 i8 j8 b2 c1 c2 d1 d2 e1 e2 f1 f2 g1 g2 h1 h2 i2 b9 c9 c10 d9 d10 e9 e10 f9 f10 g9 g10 h9 h10 i9
}
