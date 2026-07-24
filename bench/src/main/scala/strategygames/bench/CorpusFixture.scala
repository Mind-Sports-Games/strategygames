package strategygames.bench

import java.nio.charset.StandardCharsets

import strategygames.{ ActionStrs, GameLogic }
import strategygames.format.FEN
import strategygames.variant.Variant

final case class CorpusFixture(
    family: String,
    lib: GameLogic,
    variant: Variant,
    initialFen: Option[FEN],
    actionStrs: ActionStrs
)

object CorpusFixture {

  private val resourceRoot = "/corpus"

  def resourceName(family: String, size: String): String = s"$family-$size.txt"

  def render(fixture: CorpusFixture): String = {
    val header = List(
      s"family=${fixture.family}",
      s"gameLogic=${fixture.lib.id}",
      s"variant=${fixture.variant.key}",
      s"initialFen=${fixture.initialFen.fold("")(_.value)}",
      s"turns=${fixture.actionStrs.size}"
    )
    val body = fixture.actionStrs.map(_.mkString(","))
    (header ++ body).mkString("", "\n", "\n")
  }

  def parse(content: String): CorpusFixture = {
    val lines      = content.linesIterator.toVector
    val header     = lines.take(5)
    val family     = valueOf(header(0))
    val lib        = GameLogic(valueOf(header(1)).toInt)
    val variantKey = valueOf(header(2))
    val variant    = Variant.apply(lib, variantKey).getOrElse(sys.error(s"unknown variant key: $variantKey"))
    val fenValue   = valueOf(header(3))
    val initialFen = if (fenValue.isEmpty) None else Some(FEN.apply(lib, fenValue))
    val turns      = valueOf(header(4)).toInt
    val body       = lines.slice(5, 5 + turns)
    val actionStrs = body.map(turn => if (turn.isEmpty) Vector.empty[String] else turn.split(",", -1).toVector)
    CorpusFixture(family, lib, variant, initialFen, actionStrs)
  }

  def load(family: String, size: String): CorpusFixture =
    parse(readResource(s"$resourceRoot/${resourceName(family, size)}"))

  private def valueOf(line: String): String = {
    val index = line.indexOf('=')
    if (index < 0) sys.error(s"malformed corpus header line: $line")
    line.substring(index + 1)
  }

  private def readResource(path: String): String = {
    val stream = Option(getClass.getResourceAsStream(path))
      .getOrElse(sys.error(s"missing corpus resource $path"))
    try new String(stream.readAllBytes(), StandardCharsets.UTF_8)
    finally stream.close()
  }
}
