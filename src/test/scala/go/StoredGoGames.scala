package strategygames.go

import java.nio.charset.StandardCharsets
import java.nio.file.{ Files, Path, Paths }

import scala.jdk.CollectionConverters._

import strategygames.go.format.FEN
import strategygames.go.variant.Variant

final case class Ps11Result(plies: Int, end: Boolean, status: String, winner: String, finalFen: FEN)

final case class StoredGoGame(
    id: String,
    initialFen: FEN,
    actions: List[String],
    snapshottedFinalFen: FEN,
    snapshottedResult: String,
    snapshottedDropSetHashes: List[String],
    ps11: Ps11Result
) {

  def variant: Variant = initialFen.variant

  def startPly: Int = Game(Some(variant), Some(initialFen)).plies

  def plies: Int = startPly + actions.size
}

object StoredGoGames {

  private val resourceDirectory = "/go"

  private val fixtureSuffix = ".moves"

  private val fnvOffsetBasis = 0xcbf29ce484222325L

  private val fnvPrime = 0x100000001b3L

  lazy val all: List[StoredGoGame] = fixtureFiles.map(readFixture).sortBy(_.id)

  def named(id: String): StoredGoGame =
    all.find(_.id == id).getOrElse(sys.error(s"no stored go game named ${id}"))

  def dropSetHash(keys: List[String]): String = {
    var hash = fnvOffsetBasis
    keys.sorted.mkString(" ").getBytes(StandardCharsets.UTF_8).foreach { byte =>
      hash = (hash ^ (byte & 0xff)) * fnvPrime
    }
    f"${hash}%016x"
  }

  private def readFixture(file: Path): StoredGoGame = {
    val id = gameIdOf(file)
    linesOf(file) match {
      case initialFen :: actions :: finalFen :: result :: dropSetHashes :: ps11 :: Nil =>
        StoredGoGame(
          id,
          FEN(initialFen.trim),
          wordsOf(actions),
          FEN(finalFen.trim),
          result.trim,
          wordsOf(dropSetHashes),
          ps11ResultOf(id, ps11)
        )
      case otherwise                                                                   =>
        sys.error(
          s"${id} holds ${otherwise.size} lines where a fixture holds an initial fen, its actions, " +
            "its final fen, its result, a drop set hash per position and what ps11 replayed it to"
        )
    }
  }

  private def ps11ResultOf(id: String, line: String): Ps11Result =
    line.trim.split(" fen=", 2) match {
      case Array(summary, fen) =>
        val stated = summary
          .split(" ")
          .map(_.split("=", 2))
          .collect { case Array(name, value) => (name, value) }
          .toMap
        def field(name: String): String =
          stated.getOrElse(name, sys.error(s"${id} states no ${name} for ps11"))
        Ps11Result(
          field("plies").toInt,
          field("end").toBoolean,
          field("status"),
          field("winner"),
          FEN(fen.trim)
        )
      case _                   => sys.error(s"${id} states no fen for ps11")
    }

  private def gameIdOf(file: Path): String =
    file.getFileName.toString.dropRight(fixtureSuffix.length)

  private def linesOf(file: Path): List[String] =
    Files.readAllLines(file, StandardCharsets.UTF_8).asScala.toList

  private def wordsOf(line: String): List[String] = line.trim.split("\\s+").toList

  private def fixtureFiles: List[Path] = {
    val directory = Option(getClass.getResource(resourceDirectory))
      .getOrElse(sys.error(s"missing stored go game resources ${resourceDirectory}"))
    val listed    = Files.list(Paths.get(directory.toURI))
    try listed.iterator.asScala.filter(_.getFileName.toString.endsWith(fixtureSuffix)).toList
    finally listed.close()
  }
}
