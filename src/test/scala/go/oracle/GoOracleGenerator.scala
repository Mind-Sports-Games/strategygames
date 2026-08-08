package strategygames.go.oracle

import java.nio.charset.StandardCharsets
import java.nio.file.{ Files, Paths }

import scala.annotation.tailrec

import strategygames.go.{ Api, Game, Stone }
import strategygames.go.format.FEN
import strategygames.go.variant.{ Go13x13, Go19x19, Go9x9, Variant }

object GoOracleGenerator {

  private val fixturePath = Paths.get("src/test/resources/go/oracle.txt")

  def games(): List[GoOracleGame] =
    upstreamGames ++ randomWalkGames ++ superkoGames ++ curatedGames

  def main(args: Array[String]): Unit = {
    val rendered = GoOracle.render(games())
    Files.createDirectories(fixturePath.getParent)
    Files.write(fixturePath, rendered.getBytes(StandardCharsets.UTF_8))
    println(s"wrote ${fixturePath} (${rendered.length} chars)")
  }

  private def pinnedRule(
      name: String,
      variant: Variant,
      initialFen: Option[FEN],
      actionStrs: List[String]
  ): GoOracleGame =
    recordedGame(name, variant, initialFen, actionStrs, recordsFen = true, recordsDropKeys = false)

  private def pinnedRuleWithDropKeys(
      name: String,
      variant: Variant,
      actionStrs: List[String]
  ): GoOracleGame =
    recordedGame(name, variant, None, actionStrs, recordsFen = true, recordsDropKeys = true)

  private def deterministicWalk(
      name: String,
      variant: Variant,
      initialFen: Option[FEN],
      actionStrs: List[String]
  ): GoOracleGame =
    recordedGame(name, variant, initialFen, actionStrs, recordsFen = false, recordsDropKeys = false)

  private def recordedGame(
      name: String,
      variant: Variant,
      initialFen: Option[FEN],
      actionStrs: List[String],
      recordsFen: Boolean,
      recordsDropKeys: Boolean
  ): GoOracleGame = {
    val unrecorded = GoOracleGame(
      name = name,
      variantKey = variant.key,
      initialFen = initialFen.map(_.value),
      recordsFen = recordsFen,
      recordsDropKeys = recordsDropKeys,
      actionStrs = actionStrs,
      plies = Nil
    )
    unrecorded.copy(plies = GoOracle.replayedPlies(unrecorded))
  }

  private val upstreamResourcePath = "/go/upstream-go-bench.suite"
  private val upstreamFiles        = "abcdefghjklmnopqrst"
  private val upstreamBoardSize    = 19
  private val upstreamMovesMarker  = " moves "

  private def upstreamGames: List[GoOracleGame] =
    upstreamSuite.linesIterator
      .filter(_.contains(upstreamMovesMarker))
      .zipWithIndex
      .map { case (line, suiteLine) =>
        pinnedRule(
          s"upstream-${suiteLine}",
          Go19x19,
          None,
          line.split(upstreamMovesMarker, 2)(1).trim.split("\\s+").toList.map(upstreamDropOf)
        )
      }
      .toList

  private def upstreamDropOf(coordinate: String): String = {
    val file = upstreamFiles.indexOf(coordinate.charAt(0))
    val rank = coordinate.drop(1).toInt
    require(file >= 0 && file < upstreamBoardSize, s"unknown upstream file in ${coordinate}")
    require(rank >= 1 && rank <= upstreamBoardSize, s"unknown upstream rank in ${coordinate}")
    dropOf(upstreamBoardSize * (rank - 1) + file, Go19x19)
  }

  private def dropOf(engineMove: Int, variant: Variant): String =
    Api
      .moveToPos(engineMove, variant)
      .map(pos => s"${Stone.forsyth}@${pos.key}")
      .getOrElse(sys.error(s"engine move ${engineMove} names no square of ${variant.key}"))

  private def upstreamSuite: String = {
    val stream = Option(getClass.getResourceAsStream(upstreamResourcePath))
      .getOrElse(sys.error(s"missing upstream go suite resource ${upstreamResourcePath}"))
    try new String(stream.readAllBytes(), StandardCharsets.UTF_8)
    finally stream.close()
  }

  private val walkSeed          = 20260807L
  private val walkMultiplier    = 1103515245L
  private val walkIncrement     = 12345L
  private val walkGamesPerBoard = 20
  private val walkTermination   = List("pass", "pass", "ss:")

  final private case class BoardWalk(variant: Variant, dropCap: Int)

  private val boardWalks = List(
    BoardWalk(Go9x9, 120),
    BoardWalk(Go13x13, 200),
    BoardWalk(Go19x19, 400)
  )

  private def randomWalkGames: List[GoOracleGame] =
    boardWalks.flatMap { walk =>
      (0 until walkGamesPerBoard)
        .foldLeft((List.empty[GoOracleGame], walkSeed)) { case ((walked, seed), index) =>
          val (drops, nextSeed) = walkedDrops(walk.variant, None, walk.dropCap, seed)
          val name              = f"walk-${walk.variant.key}%s-${index}%02d"
          (deterministicWalk(name, walk.variant, None, drops ++ walkTermination) :: walked, nextSeed)
        }
        ._1
        .reverse
    }

  private def walkedDrops(
      variant: Variant,
      initialFen: Option[FEN],
      dropCap: Int,
      seed: Long
  ): (List[String], Long) = {
    @tailrec
    def walking(game: Game, played: List[String], seed: Long): (List[String], Long) = {
      val candidates = game.situation.dropsAsDrops
      if (played.size >= dropCap || candidates.isEmpty) (played.reverse, seed)
      else {
        val advanced = seed * walkMultiplier + walkIncrement
        val drop     = candidates(math.floorMod(advanced, candidates.size.toLong).toInt)
        walking(game.applyDrop(drop), s"${drop.piece.role.forsyth}@${drop.pos.key}" :: played, advanced)
      }
    }
    walking(Game(Some(variant), initialFen), Nil, seed)
  }

  private val superkoActionStrs = List(
    "s@b2",
    "s@c2",
    "s@a3",
    "s@d3",
    "s@b4",
    "s@c4",
    "s@c3",
    "s@b3",
    "pass",
    "pass",
    "s@g7",
    "s@g3",
    "s@h7",
    "s@h3",
    "s@g8",
    "s@g4",
    "s@h8",
    "s@h4",
    "s@f7",
    "s@f3",
    "pass",
    "pass",
    "ss:"
  )

  private def superkoGames: List[GoOracleGame] = List(
    pinnedRuleWithDropKeys("superko-corpus-9x9", Go9x9, superkoActionStrs)
  )

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

  private val koEngineMoves = List(2, 59, 20, 39, 22, 41, 40, 21)

  private val tripleKo = List(
    "s@b8",
    "s@b7",
    "s@c9",
    "s@c6",
    "s@d8",
    "s@d7",
    "s@f8",
    "s@f7",
    "s@g9",
    "s@g6",
    "s@h8",
    "s@h7",
    "s@b2",
    "s@b3",
    "s@c1",
    "s@c4",
    "s@d2",
    "s@d3",
    "s@f2",
    "s@f3",
    "s@g1",
    "s@g4",
    "s@h2",
    "s@h3",
    "s@c7",
    "s@c2",
    "s@g3",
    "s@g8",
    "s@g7",
    "s@c8",
    "s@c3",
    "s@g2",
    "s@c7",
    "s@c2"
  )

  private def curatedGames: List[GoOracleGame] =
    List(
      pinnedRule(
        "scripted-9x9-settled",
        Go9x9,
        None,
        scriptedNineByNine ++ List("pass", "pass", "ss:i1")
      ),
      pinnedRuleWithDropKeys("ko-point-19x19", Go19x19, koEngineMoves.map(dropOf(_, Go19x19))),
      pinnedRuleWithDropKeys("triple-ko", Go9x9, tripleKo)
    ) ++ handicapGames ++ List(
      pinnedRule("four-pass-no-ss", Go9x9, None, List("pass", "pass", "pass", "pass")),
      pinnedRule(
        "pass-drop-pass-settled",
        Go9x9,
        None,
        List("pass", "pass", "s@e5", "pass", "pass", "ss:")
      ),
      pinnedRule("ss-off-board-key", Go9x9, None, scriptedNineByNine ++ List("pass", "pass", "ss:n4")),
      pinnedRule("ss-names-nothing", Go9x9, None, scriptedNineByNine ++ List("pass", "pass", "ss:"))
    )

  private val handicaps       = (1 to 9).toList
  private val handicapKomi    = 55
  private val handicapDropCap = 10

  private def handicapGames: List[GoOracleGame] =
    handicaps
      .foldLeft((List.empty[GoOracleGame], walkSeed)) { case ((walked, seed), handicap) =>
        val startingFen       = Go9x9.fenFromSetupConfig(handicap, handicapKomi)
        val (drops, nextSeed) = walkedDrops(Go9x9, Some(startingFen), handicapDropCap, seed)
        val name              = s"handicap-9x9-h${handicap}"
        (pinnedRule(name, Go9x9, Some(startingFen), drops) :: walked, nextSeed)
      }
      ._1
      .reverse
}
