package strategygames.bench

import java.nio.charset.StandardCharsets
import java.nio.file.{ Files, Paths }

import scala.annotation.tailrec
import scala.util.{ Failure, Random, Success, Try }

import strategygames.{ Action, Game, GameLogic }
import strategygames.format.{ FEN, UciDump }
import strategygames.variant.Variant

object CorpusGenerator {

  enum StopReason:
    case Ended, CapReached, NoCandidates

  final case class PlayResult(game: Game, stopReason: StopReason)

  val seed: Long = 20240724L

  final case class Family(key: String, lib: GameLogic)

  val families: List[Family] = List(
    Family("chess", GameLogic.Chess()),
    Family("go", GameLogic.Go()),
    Family("backgammon", GameLogic.Backgammon()),
    Family("togyzkumalak", GameLogic.Togyzkumalak()),
    Family("samurai", GameLogic.Samurai()),
    Family("abalone", GameLogic.Abalone())
  )

  final case class Size(key: String, maxPlies: Int)

  val sizes: List[Size] = List(
    Size("short", 12),
    Size("medium", 60),
    Size("long", 400)
  )

  val defaultOutputDir = "bench/src/main/resources/corpus"

  def play(lib: GameLogic, variant: Variant, initialFen: FEN, maxPlies: Int, rng: Random): PlayResult = {
    @tailrec
    def advance(game: Game, candidates: List[Action]): Option[Game] =
      candidates match {
        case Nil => None
        case _   =>
          val index = rng.nextInt(candidates.size)
          Try(game.apply(candidates(index))) match {
            case Success(next) => Some(next)
            case Failure(_)    => advance(game, candidates.patch(index, Nil, 1))
          }
      }

    @tailrec
    def loop(game: Game, plies: Int): PlayResult =
      if (plies >= maxPlies) PlayResult(game, StopReason.CapReached)
      else if (game.situation.end) PlayResult(game, StopReason.Ended)
      else
        advance(game, game.situation.actions.sortBy(_.toUci.uci)) match {
          case None       => PlayResult(game, StopReason.NoCandidates)
          case Some(next) => loop(next, plies + 1)
        }

    loop(Game.apply(lib, Some(variant), Some(initialFen)), 0)
  }

  def fixtureFor(family: Family, size: Size): (CorpusFixture, StopReason) = {
    val variant    = Variant.libStandard(family.lib)
    val initialFen = variant.initialFen
    val rng        = new Random(seed + family.key.hashCode.toLong * 1000L + size.maxPlies.toLong)
    val result     = play(family.lib, variant, initialFen, size.maxPlies, rng)
    (CorpusFixture(family.key, family.lib, variant, Some(initialFen), result.game.actionStrs), result.stopReason)
  }

  def main(args: Array[String]): Unit = {
    val checkMode = args.headOption.contains("check")
    val outDir    = Paths.get(if (args.nonEmpty && !checkMode) args(0) else defaultOutputDir)
    if (!checkMode) Files.createDirectories(outDir)
    val statuses = for {
      family <- families
      size   <- sizes
    } yield {
      val (fixture, stopReason) = fixtureFor(family, size)
      val rendered              = CorpusFixture.render(fixture)
      val file                  = outDir.resolve(CorpusFixture.resourceName(family.key, size.key))
      val status =
        if (!Files.exists(file)) "NEW"
        else if (new String(Files.readAllBytes(file), StandardCharsets.UTF_8) == rendered) "BYTE_IDENTICAL"
        else "CHANGED"
      if (!checkMode) Files.write(file, rendered.getBytes(StandardCharsets.UTF_8))
      val plies = fixture.actionStrs.map(_.size).sum
      val valid = UciDump(fixture.lib, fixture.actionStrs, fixture.initialFen, fixture.variant).isValid
      println(
        s"${family.key}-${size.key}: turns=${fixture.actionStrs.size} plies=$plies stop=$stopReason uciDumpValid=$valid $status -> $file"
      )
      status
    }
    if (checkMode) {
      val drifted = statuses.exists(_ != "BYTE_IDENTICAL")
      println(if (drifted) "DETERMINISM: CHANGES_DETECTED" else "DETERMINISM: ALL_BYTE_IDENTICAL")
      if (drifted) System.exit(1)
    }
  }
}
