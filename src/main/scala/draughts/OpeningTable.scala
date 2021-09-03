package strategygames.draughts

import format.FEN
import opening.DrawTablesFMJD
import opening.DrawTablesIDF
import variant._

import cats.syntax.option._

case class OpeningTable(key: String, name: String, url: String, categories: List[StartingPosition.Category]) {

  val positions = categories.flatMap(_.positions)

  private lazy val shuffled = new scala.util.Random(475592).shuffle(positions).toIndexedSeq

  def randomOpening: (Int, StartingPosition) = {
    val index = scala.util.Random.nextInt(shuffled.size)
    index -> shuffled(index)
  }

  private val fen2position: Map[FEN, StartingPosition] = positions
    .map { p =>
      p.fen -> p
    }
    .to(Map)

  def openingByFen = fen2position.get _

  def withFen(p: StartingPosition) = s"$key|${p.fen}"
  def withRandomFen                = withFen(StartingPosition.random)
}

object OpeningTable {

  val tableFMJD = OpeningTable(
    key = "fmjd",
    name = "FMJD Drawing Table 64",
    url = "https://results.fmjd.org/viewpage.php?page_id=2",
    categories = DrawTablesFMJD.categoriesFMJD
  )

  val tableFMJDBrazilian = OpeningTable(
    key = "fmjdBrazilian",
    name = "FMJD Drawing Table 64 - Brazilian",
    url = "https://results.fmjd.org/viewpage.php?page_id=2",
    categories = DrawTablesFMJD.categoriesFMJDBrazilian
  )

  val tableIDF = OpeningTable(
    key = "idf",
    name = "IDF Drawing Table 64",
    url = "https://idf64.org/tables-of-draw/",
    categories = DrawTablesIDF.categoriesIDF
  )

  val tableIDFBrazilian = OpeningTable(
    key = "idf",
    name = "IDF Drawing Table 64 - Brazilian",
    url = "https://idf64.org/tables-of-draw/",
    categories = DrawTablesIDF.categoriesIDFBrazilian
  )

  val tableIDFBasic = OpeningTable(
    key = "idfBasic",
    name = "IDF Drawing Table 64 - Basic Positions",
    url = "https://idf64.org/tables-of-draw/",
    categories = DrawTablesIDF.categoriesIDFBasic
  )

  private val allTables = List(
    tableIDF,
    tableIDFBrazilian,
    tableIDFBasic,
    tableFMJD,
    tableFMJDBrazilian,
  )

  private val key2table: Map[String, OpeningTable] = allTables
    .map { p =>
      p.key -> p
    }
    .to(Map)

  def byKey = key2table.get _

  def tablesForVariant(v: Variant) = v match {
    case Russian | Pool => tableIDF.categories.flatMap(_.positions).map(_.fen)
    case Brazilian      => tableIDFBrazilian.categories.flatMap(_.positions).map(_.fen)
    case _              => List()
  }

}
