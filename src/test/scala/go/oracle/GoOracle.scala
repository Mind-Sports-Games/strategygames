package strategygames.go.oracle

import java.nio.charset.StandardCharsets

import strategygames.Player
import strategygames.go.{ Game, Replay, Situation }
import strategygames.go.format.{ FEN, Forsyth }
import strategygames.go.variant.Variant

final case class GoOraclePly(
    fen: String,
    fenDigest: Int,
    legalDropCount: Int,
    legalDropDigest: Int,
    legalDropKeys: List[String],
    scoreP1: Int,
    scoreP2: Int,
    capturesP1: Int,
    capturesP2: Int,
    end: Boolean,
    winner: Option[String]
)

final case class GoOracleGame(
    name: String,
    variantKey: String,
    initialFen: Option[String],
    recordsFen: Boolean,
    recordsDropKeys: Boolean,
    actionStrs: List[String],
    plies: List[GoOraclePly]
)

object GoOracle {

  private val resourcePath = "/go/oracle.txt"

  private val gameMarker    = "G"
  private val actionsMarker = "A"
  private val plyMarker     = "P"

  private val fieldSeparator  = "|"
  private val fieldSplitter   = "\\|"
  private val valueSeparator  = " "
  private val digestSeparator = ","

  private val yes = "1"
  private val no  = "0"

  def render(games: List[GoOracleGame]): String =
    games.flatMap(renderedGame).mkString("", "\n", "\n")

  def parse(content: String): List[GoOracleGame] =
    gameBlocksOf(content.linesIterator.filter(_.nonEmpty).toList).map(parsedGame)

  def load(): List[GoOracleGame] = parse(resourceContent)

  def replayedPlies(game: GoOracleGame): List[GoOraclePly] = {
    val variant    = Variant(game.variantKey).getOrElse(sys.error(s"unknown go variant: ${game.variantKey}"))
    val initialFen = game.initialFen.map(FEN(_))
    (0 to game.actionStrs.size).toList.map { played =>
      recordedPly(replayed(variant, initialFen, game.actionStrs.take(played)), game)
    }
  }

  def digestOf(value: String): Int = value.hashCode

  def digestOf(legalDropKeys: List[String]): Int = digestOf(legalDropKeys.mkString(digestSeparator))

  private def replayed(variant: Variant, initialFen: Option[FEN], actionStrs: List[String]): Game =
    Replay
      .gameFromUciStrings(
        actionStrs.map(Vector(_)).toVector,
        activePlayerAfter(variant, initialFen, actionStrs.size),
        initialFen,
        variant
      )
      .valueOr(error => sys.error(s"go oracle replay of ${actionStrs.mkString(valueSeparator)}: ${error}"))

  private def activePlayerAfter(variant: Variant, initialFen: Option[FEN], turns: Int): Player =
    Player.fromTurnCount(turns + startPlayerOf(variant, initialFen).fold(0, 1))

  private def startPlayerOf(variant: Variant, initialFen: Option[FEN]): Player =
    initialFen.getOrElse(variant.initialFen).player.getOrElse(Player.P1)

  private def recordedPly(game: Game, recording: GoOracleGame): GoOraclePly = {
    val situation     = game.situation
    val fen           = Forsyth.>>(game).value
    val legalDropKeys = situation.drops.getOrElse(Nil).map(_.key).sorted
    GoOraclePly(
      fen = if (recording.recordsFen) fen else "",
      fenDigest = digestOf(fen),
      legalDropCount = legalDropKeys.size,
      legalDropDigest = digestOf(legalDropKeys),
      legalDropKeys = if (recording.recordsDropKeys) legalDropKeys else Nil,
      scoreP1 = situation.board.areaScore.p1,
      scoreP2 = situation.board.areaScore.p2,
      capturesP1 = situation.board.history.captures.p1,
      capturesP2 = situation.board.history.captures.p2,
      end = situation.end,
      winner = situation.winner.map(_.name)
    )
  }

  private def renderedGame(game: GoOracleGame): List[String] =
    List(
      List(
        gameMarker,
        game.name,
        game.variantKey,
        game.initialFen.getOrElse(""),
        if (game.recordsFen) yes else no,
        if (game.recordsDropKeys) yes else no
      ).mkString(fieldSeparator),
      List(actionsMarker, game.actionStrs.mkString(valueSeparator)).mkString(fieldSeparator)
    ) ++ renderedPlies(game.plies)

  private def renderedPlies(plies: List[GoOraclePly]): List[String] =
    plies
      .foldLeft((List.empty[String], Set.empty[String])) { case ((rendered, previousDrops), ply) =>
        val drops = ply.legalDropKeys.toSet
        val line  = List(
          plyMarker,
          ply.fen,
          ply.fenDigest.toString,
          ply.legalDropCount.toString,
          ply.legalDropDigest.toString,
          ply.scoreP1.toString,
          ply.scoreP2.toString,
          ply.capturesP1.toString,
          ply.capturesP2.toString,
          if (ply.end) yes else no,
          ply.winner.getOrElse(""),
          (drops -- previousDrops).toList.sorted.mkString(valueSeparator),
          (previousDrops -- drops).toList.sorted.mkString(valueSeparator)
        ).mkString(fieldSeparator)
        (line :: rendered, drops)
      }
      ._1
      .reverse

  private def gameBlocksOf(lines: List[String]): List[List[String]] =
    lines
      .foldLeft(List.empty[List[String]]) { (blocks, line) =>
        if (line.startsWith(gameMarker + fieldSeparator)) List(line) :: blocks
        else
          blocks match {
            case block :: earlier => (line :: block) :: earlier
            case Nil              => sys.error(s"go oracle line before any game header: ${line}")
          }
      }
      .map(_.reverse)
      .reverse

  private def parsedGame(block: List[String]): GoOracleGame = block match {
    case header :: actions :: plies =>
      GoOracleGame(
        name = fieldsOf(header)(1),
        variantKey = fieldsOf(header)(2),
        initialFen = Some(fieldsOf(header)(3)).filter(_.nonEmpty),
        recordsFen = fieldsOf(header)(4) == yes,
        recordsDropKeys = fieldsOf(header)(5) == yes,
        actionStrs = parsedValues(fieldsOf(actions)(1)),
        plies = parsedPlies(plies)
      )
    case _                          => sys.error(s"go oracle game without a header and actions: ${block}")
  }

  private def parsedPlies(lines: List[String]): List[GoOraclePly] =
    lines
      .foldLeft((List.empty[GoOraclePly], Set.empty[String])) { case ((parsed, previousDrops), line) =>
        val fields = fieldsOf(line)
        val drops  = (previousDrops -- parsedValues(fields(12))) ++ parsedValues(fields(11))
        val ply    = GoOraclePly(
          fen = fields(1),
          fenDigest = fields(2).toInt,
          legalDropCount = fields(3).toInt,
          legalDropDigest = fields(4).toInt,
          legalDropKeys = drops.toList.sorted,
          scoreP1 = fields(5).toInt,
          scoreP2 = fields(6).toInt,
          capturesP1 = fields(7).toInt,
          capturesP2 = fields(8).toInt,
          end = fields(9) == yes,
          winner = Some(fields(10)).filter(_.nonEmpty)
        )
        (ply :: parsed, drops)
      }
      ._1
      .reverse

  private def fieldsOf(line: String): Vector[String] = line.split(fieldSplitter, -1).toVector

  private def parsedValues(field: String): List[String] =
    if (field.isEmpty) Nil else field.split(valueSeparator).toList

  private def resourceContent: String = {
    val stream = Option(getClass.getResourceAsStream(resourcePath))
      .getOrElse(sys.error(s"missing go oracle fixture ${resourcePath}"))
    try new String(stream.readAllBytes(), StandardCharsets.UTF_8)
    finally stream.close()
  }
}
