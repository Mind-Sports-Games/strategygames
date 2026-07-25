package strategygames.go

import java.nio.charset.StandardCharsets
import java.nio.file.{ Files, Path, Paths }

import scala.util.control.NonFatal

import org.specs2.mutable.Specification

import strategygames.{ Player, Status }
import strategygames.go.format.{ FEN, Forsyth, Uci }
import strategygames.go.variant.{ Go13x13, Go13x13Scala, Go19x19, Go19x19Scala, Go9x9, Go9x9Scala, Variant }

class GoDifferentialTest extends Specification {

  private case class ReplayCase(
      label: String,
      joansalaVariant: Variant,
      initialFen: Option[FEN],
      actions: List[String],
      compareWrapperDrops: Boolean = false,
      superkoActionsByPly: Map[Int, List[Int]] = Map.empty
  ) {
    def scalaVariant: Variant = scalaTwinOf(joansalaVariant)
  }

  private case class PositionFacts(
      wrapperFen: String,
      wrapperDrops: List[String],
      stones: List[(String, Piece)],
      p1Score: Double,
      p2Score: Double,
      ended: Boolean,
      status: Option[Status],
      winner: Option[Player]
  )

  private val scalaTwinOf: Map[Variant, Variant] =
    Map(Go9x9 -> Go9x9Scala, Go13x13 -> Go13x13Scala, Go19x19 -> Go19x19Scala)

  private def factsOf(game: Game, withWrapperDrops: Boolean): PositionFacts = {
    val position = game.situation.board.apiPosition
    val ended    = game.situation.end
    PositionFacts(
      wrapperFen = Forsyth.>>(game).value,
      wrapperDrops =
        if (withWrapperDrops) game.situation.drops.fold(List.empty[String])(_.map(_.key).sorted)
        else Nil,
      stones = position.pieceMap.toList.map { case (pos, piece) => (pos.key, piece) }.sortBy(_._1),
      p1Score = position.p1Score,
      p2Score = position.p2Score,
      ended = ended,
      status = game.situation.status,
      winner = game.situation.winner
    )
  }

  private def play(game: Game, uci: String, context: => String): Game =
    try
      Uci(uci)
        .map(game.apply(_).map { case (next, _) => next })
        .getOrElse(sys.error(s"${context} unreadable uci ${uci}"))
        .valueOr(error => sys.error(s"${context} rejected ${uci}: ${error}"))
    catch {
      case NonFatal(error) => sys.error(s"${context} failed on ${uci}: ${error.getMessage}")
    }

  private def engineActionsWhileOngoing(game: Game): List[Int] =
    if (game.situation.end) Nil else game.situation.board.apiPosition.legalActions.toList.sorted

  private def disagreements(replayCase: ReplayCase): List[String] =
    replayCase.actions.zipWithIndex
      .foldLeft(
        (
          Game(Some(replayCase.joansalaVariant), replayCase.initialFen),
          Game(Some(replayCase.scalaVariant), replayCase.initialFen),
          List.empty[String]
        )
      ) { case ((joansala, scala, found), (uci, index)) =>
        val ply             = index + 1
        val context         = (engine: String) => s"[${replayCase.label} ply ${ply} ${engine}]"
        val nextJoansala    = play(joansala, uci, context(replayCase.joansalaVariant.key))
        val nextScala       = play(scala, uci, context(replayCase.scalaVariant.key))
        val joansalaActions = engineActionsWhileOngoing(nextJoansala)
        val scalaActions    = engineActionsWhileOngoing(nextScala)
        val joansalaOnly    = joansalaActions.diff(scalaActions)
        val scalaOnly       = scalaActions.diff(joansalaActions)
        val joansalaFacts   = factsOf(nextJoansala, replayCase.compareWrapperDrops)
        val scalaFacts      = factsOf(nextScala, replayCase.compareWrapperDrops)
        val difference      =
          if (
            joansalaFacts == scalaFacts && scalaOnly.isEmpty &&
            joansalaOnly == replayCase.superkoActionsByPly.getOrElse(ply, Nil)
          ) None
          else
            Some(
              s"${replayCase.label} ply ${ply} after ${uci}:" +
                s"\n  actions only in ${replayCase.joansalaVariant.key} = ${joansalaOnly}" +
                s"\n  actions only in ${replayCase.scalaVariant.key} = ${scalaOnly}" +
                s"\n  ${replayCase.joansalaVariant.key} = ${joansalaFacts}" +
                s"\n  ${replayCase.scalaVariant.key} = ${scalaFacts}"
            )
        (nextJoansala, nextScala, found ++ difference)
      }
      ._3

  private val corpusDirectory: Path =
    Iterator
      .iterate(Paths.get("").toAbsolutePath)(_.getParent)
      .takeWhile(_ != null)
      .map(_.resolve("bench/src/main/resources/corpus"))
      .find(Files.isDirectory(_))
      .getOrElse(sys.error("cannot locate bench/src/main/resources/corpus from the working directory"))

  private def corpusCase(fileName: String, plyLimit: Int = Int.MaxValue): ReplayCase = {
    val lines      = Files
      .readString(corpusDirectory.resolve(fileName), StandardCharsets.UTF_8)
      .linesIterator
      .toVector
    val header     = lines.take(5).map(line => line.substring(line.indexOf('=') + 1))
    val variant    = Variant(header(2)).getOrElse(sys.error(s"unknown go variant in ${fileName}"))
    val initialFen = Option(header(3)).filter(_.nonEmpty).map(FEN(_))
    val turns      = header(4).toInt
    val actions    = lines
      .slice(5, 5 + turns)
      .flatMap(_.split(",", -1).toVector.filter(_.nonEmpty))
      .take(plyLimit)
      .toList
    ReplayCase(s"${fileName}[${actions.size}]", variant, initialFen, actions)
  }

  private def dropUci(move: Int, variant: Variant): String =
    Api.moveToUci(move, variant).toLowerCase

  private def nextSeed(seed: Long): Long = seed * 6364136223846793005L + 1442695040888963407L

  private def generatedCase(joansalaVariant: Variant, seed: Long, dropCount: Int): ReplayCase = {
    val scalaVariant              = scalaTwinOf(joansalaVariant)
    val (drops, finalPosition, _) =
      (1 to dropCount).foldLeft((List.empty[String], Api.positionFromVariant(scalaVariant), seed)) {
        case ((played, position, state), _) =>
          val available = position.legalDrops
          val advanced  = nextSeed(state)
          val chosen    = dropUci(available((advanced >>> 33).toInt % available.size), scalaVariant)
          (played :+ chosen, position.makeMoves(List(chosen)), advanced)
      }
    val deadStones                = finalPosition.pieceMap.toList
      .filter { case (_, piece) => piece.player == P1 }
      .map { case (pos, _) => pos.key }
      .sorted
      .take(2)
    ReplayCase(
      s"generated-${joansalaVariant.key}-${seed}[${dropCount}]",
      joansalaVariant,
      None,
      drops ++ List("pass", "pass", s"ss:${deadStones.mkString(",")}")
    )
  }

  private def rawFenFields(position: Api.Position): Array[String] = position.fenString.split(' ')

  private val scriptedNineByNine = List(
    "s@g3",
    "s@c7",
    "s@f2",
    "s@e5",
    "s@e1",
    "s@d4",
    "s@h4",
    "s@i1",
    "s@i5",
    "s@c5",
    "s@d5",
    "s@d6"
  )

  private val superkoNineByNine =
    List("c6", "c5", "c7", "d6", "e6", "e5", "e7", "d4", "d8", "e4").map(key => s"s@${key}") ++
      List("pass") ++ List("d7", "d5").map(key => s"s@${key}")

  private val koPointNineteen =
    List(2, 59, 20, 39, 22, 41, 40, 21).map(dropUci(_, Go19x19))

  private val oppositeParityRepeatNineByNine =
    List(40, 49, 48, 58, 57, 76, 67, 68, 59, 66, 50, 81, 67, 49, 81).map(dropUci(_, Go9x9))

  private val oppositeParityRepeatDrop = Api.uciToMove("s@e7", Go9x9)

  private val replayCases: List[ReplayCase] = List(
    corpusCase("go-short.txt"),
    corpusCase("go-medium.txt"),
    corpusCase("go-long.txt"),
    corpusCase("go-go9x9-long.txt"),
    corpusCase("go-go13x13-long.txt"),
    ReplayCase(
      "scripted-9x9-capture",
      Go9x9,
      None,
      scriptedNineByNine ++ List("pass", "pass", "ss:i1"),
      compareWrapperDrops = true
    ),
    ReplayCase("issue489-ko-after-pass", Go19x19, None, koPointNineteen ++ List("pass", "s@c3")),
    ReplayCase(
      "issue490-multi-stone-recapture",
      Go9x9,
      None,
      oppositeParityRepeatNineByNine,
      superkoActionsByPly = Map(15 -> List(oppositeParityRepeatDrop))
    ),
    ReplayCase(
      "superko-9x9",
      Go9x9,
      None,
      superkoNineByNine,
      compareWrapperDrops = true,
      superkoActionsByPly = Map(13 -> List(Api.uciToMove("s@d6", Go9x9)))
    ),
    ReplayCase(
      "four-passes-9x9",
      Go9x9,
      None,
      scriptedNineByNine ++ List("pass", "pass", "pass", "pass"),
      compareWrapperDrops = true
    ),
    generatedCase(Go9x9, 20260724L, 70),
    generatedCase(Go9x9, 987654321L, 70),
    generatedCase(Go9x9, 424242L, 70),
    generatedCase(Go13x13, 20260724L, 70),
    generatedCase(Go13x13, 987654321L, 70),
    generatedCase(Go13x13, 424242L, 70),
    generatedCase(Go19x19, 20260724L, 60),
    generatedCase(Go19x19, 987654321L, 60),
    generatedCase(Go19x19, 424242L, 60)
  )

  replayCases.foreach { replayCase =>
    s"replaying ${replayCase.label} through both engines" should {
      "agree on every position" in {
        disagreements(replayCase) === Nil
      }
    }
  }

  "the empty board" should {
    "score its single stoneless region to white in joansala and to nobody in the scala engine" in {
      val joansala = List(Go9x9, Go13x13, Go19x19).map(Api.positionFromVariant)
      val scala    = List(Go9x9Scala, Go13x13Scala, Go19x19Scala).map(Api.positionFromVariant)
      (joansala.map(p => (p.p1Score, p.p2Score)) === List((0.0, 86.5), (0.0, 176.5), (0.0, 368.5))) and
        (scala.map(p => (p.p1Score, p.p2Score)) === List((0.0, 5.5), (0.0, 7.5), (0.0, 7.5))) and
        (scala.map(_.fen) === List(Go9x9.initialFen, Go13x13.initialFen, Go19x19.initialFen)) and
        (joansala.map(_.fen) !== scala.map(_.fen))
    }
  }

  "the raw engine fen" should {
    val played   = scriptedNineByNine ++ List("pass", "pass")
    val joansala = Api.positionFromVariantAndMoves(Go9x9, played)
    val scala    = Api.positionFromVariantAndMoves(Go9x9Scala, played)

    "hardcode captures in joansala and report them in the scala engine" in {
      (rawFenFields(joansala).slice(5, 7).toList === List("0", "0")) and
        (rawFenFields(scala).slice(5, 7).toList === List("0", "1"))
    }

    "hardcode the pass count in joansala and report it in the scala engine" in {
      (rawFenFields(joansala)(8) === "0") and (rawFenFields(scala)(8) === "2")
    }

    "still produce identical wrapper fens, which overwrite both fields" in {
      val joansalaGame = playAll(Game(Go9x9), played)
      val scalaGame    = playAll(Game(Go9x9Scala), played)
      (Forsyth.>>(joansalaGame) === Forsyth.>>(scalaGame)) and
        (Forsyth.>>(scalaGame).value.split(' ').slice(5, 7).toList === List("0", "1")) and
        (Forsyth.>>(scalaGame).value.split(' ')(8) === "2")
    }

    "omit the ko point in both engines, because validateFEN only accepts a dash there" in {
      val joansalaKo = Api.positionFromVariantAndMoves(Go19x19, koPointNineteen)
      val scalaKo    = Api.positionFromVariantAndMoves(Go19x19Scala, koPointNineteen)
      (joansalaKo.legalDrops.contains(40) === false) and
        (scalaKo.legalDrops.contains(40) === false) and
        (rawFenFields(joansalaKo)(2) === "-") and
        (rawFenFields(scalaKo)(2) === "-") and
        (Api.validateFEN(joansalaKo.fenString) === true) and
        (Api.validateFEN(scalaKo.fenString) === true)
    }
  }

  "a handicap fen" should {
    "lose its score fields to a rescore in both engines alike" in {
      val joansalaFen = Go9x9.fenFromSetupConfig(4, 55)
      val scalaFen    = Go9x9Scala.fenFromSetupConfig(4, 55)
      val joansala    = Api.positionFromVariantNameAndFEN(Go9x9.key, joansalaFen.value)
      val scala       = Api.positionFromVariantNameAndFEN(Go9x9Scala.key, scalaFen.value)
      (joansalaFen === scalaFen) and
        (joansalaFen.value.split(' ').slice(3, 5).toList === List("40", "55")) and
        (joansala.fen === scala.fen) and
        (joansala.fen.value.split(' ').slice(3, 5).toList === List("810", "55"))
    }
  }

  "a superko position" should {
    val joansala = Api.positionFromVariantAndMoves(Go9x9, superkoNineByNine)
    val scala    = Api.positionFromVariantAndMoves(Go9x9Scala, superkoNineByNine)
    val repeat   = Api.uciToMove("s@d6", Go9x9)

    "be reported by joansala only after the move, and forbidden up front by the scala engine" in {
      (joansala.legalDrops.contains(repeat) === true) and
        (scala.legalDrops.contains(repeat) === false) and
        (joansala.makeMoves(List("s@d6")).isRepetition === true) and
        (scala.isRepetition === false)
    }

    "leave the wrapper with the same drops either way" in {
      val joansalaGame = playAll(Game(Go9x9), superkoNineByNine)
      val scalaGame    = playAll(Game(Go9x9Scala), superkoNineByNine)
      (joansalaGame.situation.drops.map(_.map(_.key).sorted)
        === scalaGame.situation.drops.map(_.map(_.key).sorted)) and
        (joansalaGame.situation.drops.map(_.map(_.key).contains("d6")) === Some(false))
    }
  }

  "a board recreated with the opposite player to move" should {
    val joansala = playAll(Game(Go9x9), oppositeParityRepeatNineByNine)
    val scala    = playAll(Game(Go9x9Scala), oppositeParityRepeatNineByNine)

    "stay playable in joansala and be forbidden by the positional superko of the scala engine" in {
      (joansala.situation.board.apiPosition.legalDrops.contains(oppositeParityRepeatDrop) === true) and
        (scala.situation.board.apiPosition.legalDrops.contains(oppositeParityRepeatDrop) === false) and
        (joansala.situation.drops.map(_.map(_.key).contains("e7")) === Some(true)) and
        (scala.situation.drops.map(_.map(_.key).contains("e7")) === Some(false))
    }

    "repeat, in joansala, the board of four plies earlier under the other player" in {
      val repeated = playAll(joansala, List("s@e7"))
      val earlier  = playAll(Game(Go9x9), oppositeParityRepeatNineByNine.take(11))
      (boardFieldOf(repeated) === boardFieldOf(earlier)) and
        (repeated.situation.player === P1) and
        (earlier.situation.player === P2) and
        (repeated.situation.board.apiPosition.isRepetition === false)
    }
  }

  "an illegal action fed past the legality check" should {
    "be replayed blindly by joansala and rejected by the scala engine" in {
      val occupied = List("s@e5", "s@e5")
      (Api.positionFromVariant(Go9x9).makeMovesNoLegalCheck(occupied).pieceMap.size === 1) and
        (Api.positionFromVariant(Go9x9Scala).makeMovesNoLegalCheck(occupied) must throwAn[Exception])
    }
  }

  "a finished game" should {
    val played   = scriptedNineByNine ++ List("pass", "pass", "ss:i1")
    val joansala = playAll(Game(Go9x9), played)
    val scala    = playAll(Game(Go9x9Scala), played)

    "keep listing raw engine actions in joansala and list none in the scala engine" in {
      (joansala.situation.end === true) and
        (scala.situation.end === true) and
        (joansala.situation.board.apiPosition.legalActions.size === 71) and
        (scala.situation.board.apiPosition.legalActions.size === 0)
    }

    "offer no wrapper drops in either engine" in {
      (joansala.situation.drops === None) and (scala.situation.drops === None)
    }
  }

  private def playAll(game: Game, actions: List[String]): Game =
    actions.foldLeft(game)((played, uci) => play(played, uci, s"[${played.board.variant.key}]"))

  private def boardFieldOf(game: Game): String = Forsyth.>>(game).value.split(' ')(0)

}
