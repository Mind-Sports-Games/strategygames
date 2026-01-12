package strategygames
package format.sgf
import strategygames.ClockConfig
import scalalib.extensions.*

import org.joda.time.DateTimeZone
import org.joda.time.format.DateTimeFormat

case class Tag(name: TagType, value: String) {
  override def toString = s"""${name}[${value}]"""
}

sealed trait TagType {
  lazy val name      = toString
  lazy val lowercase = name.toLowerCase
  val isUnknown      = false
}

case class Tags(value: List[Tag]) extends AnyVal {

  def apply(name: String): Option[String] = {
    val tagType = Tag tagType name
    value.find(_.name == tagType).map(_.value)
  }

  def apply(which: Tag.type => TagType): Option[String] =
    value find (_.name == which(Tag)) map (_.value)

  def ++(tags: Tags) = tags.value.foldLeft(this)(_ + _)
  def +(tag: Tag)    = Tags(value.filterNot(_.name == tag.name) :+ tag)

  def sorted = copy(
    value = value.sortBy { tag =>
      Tags.tagIndex.getOrElse(tag.name, 999)
    }
  )

  override def toString = sorted.value mkString "\n"
}

object Tags {
  val empty = Tags(Nil)

  val importantTagRoster          = List(
    Tag.FF,
    Tag.GM,
    Tag.CA,
    Tag.AP,
    Tag.DT,
    Tag.EV,
    Tag.TM,
    Tag.PW,
    Tag.PB,
    Tag.RE
  )
  val tagIndex: Map[TagType, Int] = importantTagRoster.zipWithIndex.toMap
}

object Tag {

  case object FF extends TagType // File Format
  case object CA extends TagType // Character
  case object GN extends TagType // Game Name
  case object AP extends TagType // Application
  case object DT extends TagType { // Date time
    val format = DateTimeFormat forPattern "yyyy.MM.dd" withZone DateTimeZone.UTC
  }
  case object PC extends TagType // Place (site)
  case object EV extends TagType // Event
  case object PB extends TagType // Player Black
  case object PW extends TagType // Player White
  case object RE extends TagType // Result
  case object BR extends TagType // Black rating
  case object WR extends TagType // White rating
  case object TM extends TagType // Time Control
  case object BT extends TagType // Black Team
  case object WT extends TagType // White Team
  case object IP extends TagType // Initial positon (fen)

  // variant specific
  case object GM extends TagType // Game mode
  case object SU extends TagType // Setup info
  case object SZ extends TagType // Board size
  case object KM extends TagType // Komi (go)
  case object HA extends TagType // handicap (go)
  case object RU extends TagType // rules (go, backgammon)
  case object TB extends TagType // Total Black (score)
  case object TW extends TagType // Total White (score)
  case object CV extends TagType // dice value (backgammon)
  case object CO extends TagType // dice corner (backgammon)
  case object MI extends TagType // match info (backgammon)

  case class Unknown(n: String) extends TagType {
    override def toString  = n
    override val isUnknown = true
  }

  val tagTypes                                  = List(
    FF,
    GN,
    CA,
    AP,
    DT,
    PC,
    EV,
    PB,
    PW,
    RE,
    BR,
    WR,
    TM,
    BT,
    WT,
    IP,
    GM,
    SU,
    SZ,
    KM,
    HA,
    RU,
    TB,
    TW,
    CV,
    CO,
    MI
  )
  val tagTypesByLowercase: Map[String, TagType] =
    tagTypes.map { t => t.lowercase -> t }.to(Map)

  def apply(name: String, value: Any): Tag = new Tag(
    name = tagType(name),
    value = value.toString
  )

  def apply(name: Tag.type => TagType, value: Any): Tag = new Tag(
    name = name(this),
    value = value.toString
  )

  def tagType(name: String) =
    (tagTypesByLowercase get name.toLowerCase) | Unknown(name)

  def matchInfo(length: Int, gameNb: Int, p1Score: Int, p2Score: Int) =
    Tag(MI, s"length:${length}][game:${gameNb}][ws:${p1Score}][bs:${p2Score}")

  def timeControl(clock: Option[ClockConfig]) = Tag(TM, clock.fold("-") { _.toString })
}
