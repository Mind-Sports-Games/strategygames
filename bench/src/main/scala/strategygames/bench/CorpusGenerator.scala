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

  final case class Family(key: String, lib: GameLogic, variant: Variant)

  private def standardFamily(key: String, lib: GameLogic): Family = Family(key, lib, Variant.libStandard(lib))

  val families: List[Family] = List(
    standardFamily("chess", GameLogic.Chess()),
    standardFamily("go", GameLogic.Go()),
    standardFamily("backgammon", GameLogic.Backgammon()),
    standardFamily("togyzkumalak", GameLogic.Togyzkumalak()),
    standardFamily("samurai", GameLogic.Samurai()),
    standardFamily("abalone", GameLogic.Abalone()),
    standardFamily("fairysf", GameLogic.FairySF()),
    standardFamily("draughts", GameLogic.Draughts()),
    standardFamily("dameo", GameLogic.Dameo()),
    Family("loa", GameLogic.Chess(), Variant.Chess(strategygames.chess.variant.LinesOfAction))
  )

  final case class Size(key: String, maxPlies: Int)

  val sizes: List[Size] = List(
    Size("short", 12),
    Size("medium", 60),
    Size("long", 400)
  )

  final case class VariantFixture(key: String, lib: GameLogic, variant: Variant, maxPlies: Int)

  val variantFixtures: List[VariantFixture] = List(
    VariantFixture(
      "backgammon-nackgammon",
      GameLogic.Backgammon(),
      Variant.Backgammon(strategygames.backgammon.variant.Nackgammon),
      200
    ),
    VariantFixture(
      "backgammon-hyper",
      GameLogic.Backgammon(),
      Variant.Backgammon(strategygames.backgammon.variant.Hyper),
      200
    ),
    VariantFixture("go-go9x9", GameLogic.Go(), Variant.Go(strategygames.go.variant.Go9x9), 120),
    VariantFixture("go-go13x13", GameLogic.Go(), Variant.Go(strategygames.go.variant.Go13x13), 200),
    VariantFixture(
      "togyzkumalak-bestemshe",
      GameLogic.Togyzkumalak(),
      Variant.Togyzkumalak(strategygames.togyzkumalak.variant.Bestemshe),
      120
    ),
    VariantFixture(
      "abalone-grandabalone",
      GameLogic.Abalone(),
      Variant.Abalone(strategygames.abalone.variant.GrandAbalone),
      120
    ),
    fairysfVariant("amazons", strategygames.fairysf.variant.Amazons),
    fairysfVariant("antiflipello", strategygames.fairysf.variant.AntiFlipello),
    fairysfVariant("breakthroughtroyka", strategygames.fairysf.variant.BreakthroughTroyka),
    fairysfVariant("flipello", strategygames.fairysf.variant.Flipello),
    fairysfVariant("flipello10", strategygames.fairysf.variant.Flipello10),
    fairysfVariant("minibreakthroughtroyka", strategygames.fairysf.variant.MiniBreakthroughTroyka),
    fairysfVariant("minishogi", strategygames.fairysf.variant.MiniShogi),
    fairysfVariant("minixiangqi", strategygames.fairysf.variant.MiniXiangqi),
    fairysfVariant("octagonflipello", strategygames.fairysf.variant.OctagonFlipello),
    fairysfVariant("shogi", strategygames.fairysf.variant.Shogi),
    fairysfVariant("xiangqi", strategygames.fairysf.variant.Xiangqi)
  )

  private def fairysfVariant(name: String, variant: strategygames.fairysf.variant.Variant): VariantFixture =
    VariantFixture(s"fairysf-$name", GameLogic.FairySF(), Variant.FairySF(variant), 120)

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

  def build(key: String, lib: GameLogic, variant: Variant, maxPlies: Int): (CorpusFixture, StopReason) = {
    val initialFen = variant.initialFen
    val rng        = new Random(seed + key.hashCode.toLong * 1000L + maxPlies.toLong)
    val result     = play(lib, variant, initialFen, maxPlies, rng)
    (CorpusFixture(key, lib, variant, Some(initialFen), result.game.actionStrs), result.stopReason)
  }

  def fixtureFor(family: Family, size: Size): (CorpusFixture, StopReason) =
    build(family.key, family.lib, family.variant, size.maxPlies)

  def main(args: Array[String]): Unit = {
    val checkMode = args.headOption.contains("check")
    val outDir    = Paths.get(if (args.nonEmpty && !checkMode) args(0) else defaultOutputDir)
    if (!checkMode) Files.createDirectories(outDir)
    val defaults = for {
      family <- families
      size   <- sizes
    } yield (fixtureFor(family, size), CorpusFixture.resourceName(family.key, size.key))
    val variants = variantFixtures.map(vf =>
      (build(vf.key, vf.lib, vf.variant, vf.maxPlies), CorpusFixture.resourceName(vf.key, "long"))
    )
    val statuses = (defaults ++ variants).map { case ((fixture, stopReason), name) =>
      emit(fixture, stopReason, name, outDir, checkMode)
    }
    if (checkMode) {
      val drifted = statuses.exists(_ != "BYTE_IDENTICAL")
      println(if (drifted) "DETERMINISM: CHANGES_DETECTED" else "DETERMINISM: ALL_BYTE_IDENTICAL")
      if (drifted) System.exit(1)
    }
  }

  private def emit(
      fixture: CorpusFixture,
      stopReason: StopReason,
      name: String,
      outDir: java.nio.file.Path,
      checkMode: Boolean
  ): String = {
    val rendered = CorpusFixture.render(fixture)
    val file     = outDir.resolve(name)
    val status   =
      if (!Files.exists(file)) "NEW"
      else if (new String(Files.readAllBytes(file), StandardCharsets.UTF_8) == rendered) "BYTE_IDENTICAL"
      else "CHANGED"
    if (!checkMode) Files.write(file, rendered.getBytes(StandardCharsets.UTF_8))
    val plies = fixture.actionStrs.map(_.size).sum
    val valid = UciDump(fixture.lib, fixture.actionStrs, fixture.initialFen, fixture.variant).isValid
    println(
      s"${name.stripSuffix(".txt")}: turns=${fixture.actionStrs.size} plies=$plies stop=$stopReason uciDumpValid=$valid $status -> $file"
    )
    status
  }
}
