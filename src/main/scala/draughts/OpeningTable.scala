package strategygames.draughts

import format.FEN
import opening.DrawTablesFMJD
import opening.DrawTablesIDF
import opening.DrawTablesACF
import variant._

import cats.syntax.option._

case class OpeningTable(
    key: String,
    name: String,
    url: String,
    categories: List[StartingPosition.Category]
) {

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

  def displayStr(p: StartingPosition) =
    s"${name}. ${p.code}${p.name.map(n => s": $n").getOrElse("")}"

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

  val tableACF11ManBallot = OpeningTable(
    key = "acf-11_man_ballot",
    name = "ACF 11 man ballot",
    url = "",
    categories = DrawTablesACF.categoriesACF11ManBallot
  )

  val tableACFThreeMoveRestriction = OpeningTable(
    key = "acf-three_move_restriction",
    name = "ACF Three Move Restriction",
    url = "",
    categories = DrawTablesACF.categoriesACF11ManBallot
  )

  private val allTables = List(
    tableIDF,
    tableIDFBrazilian,
    tableIDFBasic,
    tableACF11ManBallot,
    tableACFThreeMoveRestriction,
    tableFMJD,
    tableFMJDBrazilian
  )

  private val key2table: Map[String, OpeningTable] = allTables
    .map { p =>
      p.key -> p
    }
    .to(Map)

  def byKey = key2table.get _

  def tableForVariant(v: Variant): Option[OpeningTable] = v match {
    case Russian | Pool => Some(tableIDF)
    case Brazilian      => Some(tableIDFBrazilian)
    case English        => Some(tableACF11ManBallot)
    case _              => None
  }

  def fensForVariant(v: Variant) =
    tableForVariant(v).map(_.categories).getOrElse(List()).flatMap(_.positions).map(_.fen)

}
