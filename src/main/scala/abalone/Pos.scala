package strategygames.abalone

import scala.annotation.nowarn
import scala.util.matching.Regex

case class Pos(var x: Int, var y: Int) extends AnyRef {
  def +(a: Pos): Pos = Pos.copy(this).add(a)

  def add(a: Pos): Pos = add(a.x, a.y)

  def add(x: Int, y: Int): Pos = {
    this.x += x
    this.y += y
    this
  }

  def -(a: Pos): Pos = Pos.copy(this).sub(a)

  def sub(a: Pos): Pos = sub(a.x, a.y)

  def sub(a: Int, b: Int): Pos = add(-a, -b)

  def *(a: Int): Pos = Pos.copy(this).mult(a)

  def *(a: Int, b: Int): Pos = Pos.copy(this).mult(a, b)

  def mult(a: Int): Pos = mult(a, a)

  def mult(a: Int, b: Int): Pos = {
    x *= a
    y *= b
    this
  }

  def /(a: Int): Pos = Pos.copy(this).div(a)

  def /(a: Int, b: Int): Pos = Pos.copy(this).div(a, b)

  def div(a: Int): Pos = div(a, a)

  def div(a: Int, b: Int): Pos = {
    x /= a
    y /= b
    this
  }

  //
  //
  def key: String = (y match {
    case _ if (y >= 0) => ('a' + y).toChar.toString
    case -1 => "0"
    case _ => "-" + ('a' - y - 2).toChar.toString
  }) + (x + 1).toString

  def index: Int = Piotr.posToIndex(this)

  def piotr: Char = Piotr.posToPiotr(this)

  def piotrStr: String = piotr.toString

  def equals(x: Int, y: Int): Boolean = this.x == x && this.y == y

  override def toString = "(" + x + ", " + y + ")"

  //
  //
  def vectTo3: (Double, Double) = Pos.vectTo3(x, y)

  def scal(a: Pos): Int = scal(a.x, a.y)

  def scal(a: Int, b: Int): Int = x * a + y * b

  def scal3(a: Pos): Double = scal3(a.x, a.y)

  def scal3(a: Int, b: Int): Double = (x - y / 2d) * (a - b / 2d) + y * b * 3 / 4d

  def cross(a: Pos): Int = cross(a.x, a.y)

  def cross(a: Int, b: Int): Int = x * b - y * a

  def cross3(a: Pos): Double = cross3(a.x, a.y)

  def cross3(a: Int, b: Int): Double = Pos.cross(vectTo3, Pos.vectTo3(a, b))
}

object Pos {
  private val sr3 = math.sqrt(3)

  def copy(a: Pos): Pos = new Pos(a.x, a.y)

  def fromPoint(x: (Double, Double)): Pos = fromPoint(x._1, x._2)

  def fromPoint(x: Double, y: Double): Pos = new Pos(math.round(x).toInt, math.round(y).toInt)

  private def vectTo3(x: Int, y: Int): (Double, Double) = (x - y / 2d, y * sr3 / 2)

  @nowarn private def vectTo3(x: (Double, Double)): (Double, Double) = vectTo3(x._1, x._2)

  @nowarn private def vectTo3(x: Double, y: Double): (Double, Double) = (x - y / 2, y * sr3 / 2)

  private def cross(x: (Double, Double), a: (Double, Double)): Double = cross(x._1, x._2, a._1, a._2)

  private def cross(x: Double, y: Double, a: Double, b: Double): Double = x * b - y * a

  def getRotated(a: Pos, deg: Double): (Double, Double) = getRotated(a.x, a.y, deg)

  def getRotated(x: (Double, Double), deg: Double): (Double, Double) = getRotated(x._1, x._2, deg)

  def getRotated(x: Double, y: Double, deg: Double): (Double, Double) = {
    val cos = math.cos(math.toRadians(deg))
    val sin = math.sin(math.toRadians(deg))
    (cos * x - sin * y, sin * x + cos * y)
  }

  private val _rex: String = "(0|-?[1-9][0-9]*)"

  private val _rey: String = "(0|-?[a-z])"

  private val _re: String = _rey + _rex

  private val rex: Regex = _rex.r

  private val rey: Regex = _rey.r

  val re: Regex = _re.r

  def fromKey(key: String): Option[Pos] = {
    val _y = rey.findFirstMatchIn(key)

    if (_y.isDefined) {
      val _x = rex.findFirstMatchIn(key.substring(_y.get.end))

      if (_x.isDefined) {
        val x = _x.get.matched.toInt - 1

        var sy = _y.get.matched
        val neg = if (sy.startsWith("-")) {
          sy = sy.substring(1)
          true
        } else false

        if (sy.length == 1) {
          var y = if ("0".equals(sy)) -1 else sy.charAt(0) - 'a'
          if (neg) y = -y - 1

          return Option(new Pos(x, y))
        }
      }
    }

    Option.empty
  }

  def fromIndex(i: Int): Pos = Piotr.indexToPos(i)

  def piotr(c: Char): Option[Pos] = Piotr.piotrToPos.get(c)
}

object Piotr {
  /** https://en.wikipedia.org/wiki/List_of_Unicode_characters#Latin_script, from the latin-1 script. */
  //@formatter:off
  val piotrs: Array[Char] = Array(
    'a',
    'b',
    'c',
    'd',
    'e',
    'f',
    'g',
    'h',
    'i',
    'j',
    'k',
    'l',
    'm',
    'n',
    'o',
    'p',
    'q',
    'r',
    's',
    't',
    'u',
    'v',
    'w',
    'x',
    'y',
    'z',
    'A',
    'B',
    'C',
    'D',
    'E',
    'F',
    'G',
    'H',
    'I',
    'J',
    'K',
    'L',
    'M',
    'N',
    'O',
    'P',
    'Q',
    'R',
    'S',
    'T',
    'U',
    'V',
    'W',
    'X',
    'Y',
    'Z',
    '0',
    '1',
    '2',
    '3',
    '4',
    '5',
    '6',
    '7',
    '8',
    '9',
    '!',
    '?',
    '\"',
    '#',
    '$',
    '%',
    '&',
    '\'',
    '(',
    ')',
    '*',
    '+',
    '§', // NOTE: comma is not a valid piotr due to the way lila stores analysis.
    '-',
    '.',
    '/',
    ':',
    '¨', // Note: ';' cannot be used for the same reason as above
    '<',
    '=',
    '>',
    '@',
    '[',
    '\\',
    ']',
    '^',
    '_',
    '`',
    '{',
    '|',
    '}',
    '~',
    '¡',
    '¢',
    '£',
    '¤',
    '¥',
    '¦',
    // NOTE: "§" and "¨" are used above
    '\u00A9',
    '\u00AA',
    '\u00AB',
    '\u00AC',
    '\u00AD',
    '\u00AE',
    '\u00AF',
    '\u00B0',
    '\u00B1',
    '\u00B2',
    '\u00B3',
    '\u00B4',
    '\u00B5',
    '\u00B6',
    '\u00B7',
    '\u00B8',
    '\u00B9',
    '\u00BB',
    '\u00BC',
    '\u00BD',
    '\u00BE',
    '\u00BF',
    '\u00C0',
    '\u00C1',
    '\u00C2',
    '\u00C3',
    '\u00C4',
    '\u00C5',
    '\u00C6',
    '\u00C7',
    '\u00C8',
    '\u00C9',
    '\u00CA',
    '\u00CB',
    '\u00CC',
    '\u00CD',
    '\u00CE',
    '\u00CF',
    '\u00D0',
    '\u00D1',
    '\u00D2',
    '\u00D3',
    '\u00D4',
    '\u00D5',
    '\u00D6',
    '\u00D7',
    '\u00D8',
    '\u00D9',
    '\u00DA',
    '\u00DB',
    '\u00DC',
    '\u00DD',
    '\u00DE',
    '\u00DF',
    '\u00E0',
    '\u00E1',
    '\u00E2',
    '\u00E3',
    '\u00E4',
    '\u00E5',
    '\u00E6',
    '\u00E7',
    '\u00E8',
    '\u00E9',
    '\u00EA',
    '\u00EB',
    '\u00EC',
    '\u00ED',
    '\u00EE',
    '\u00EF',
    '\u00F0',
    '\u00F1',
    '\u00F2',
    '\u00F3',
    '\u00F4',
    '\u00F5',
    '\u00F6',
    '\u00F7',
    '\u00F8',
    '\u00F9',
    '\u00FA',
    '\u00FB',
    '\u00FC',
    '\u00FD',
    '\u00FE',
    '\u00FF',
    '\u0100',
    '\u0101',
    '\u0102',
    '\u0103'
  ) ++ (260 to 328).map(_.toChar) ++ (330 to 431).map(_.toChar) // NOTE: 329 is deprecated
  // @formatter:on

  def posToIndex: Map[Pos, Int] = Range(0, piotrs.size).map(i => (indexToPos(i), i)).toMap

  def indexToPiotr: Map[Int, Char] = Range(0, piotrs.size).map(i => (i, piotrs(i))).toMap

  def posToPiotr: Map[Pos, Char] = posToIndex.keys.map(a => (a, indexToPiotr(posToIndex(a)))).toMap

  def piotrToIndex: Map[Char, Int] = indexToPiotr.keys.map(i => (indexToPiotr(i), i)).toMap

  def piotrToPos: Map[Char, Pos] = posToPiotr.keys.map(a => (posToPiotr(a), a)).toMap

  def indexToPos(i: Int): Pos = { // Notice the 'return's are necessary here
    var j = i
    if (j < 64) return new Pos(j % 8, j / 8) // A1-H1, ..., A8-H8

    j -= 64
    if (j < 20) return new Pos(j % 10, 8 + j / 10) // A9-J9, A10-J10

    j -= 20
    if (j < 16) return new Pos(8 + j % 2, j / 2) // I1, J1, ... I8, J8

    j -= 16
    if (j < 90) return new Pos(10 + j % 9, j / 9) // K1-S1, ..., K10-S10

    j -= 90
    if (j < 190) return new Pos(j % 19, 10 + j / 19) // A11-S11, ..., A19-S19

    // k² <= i < (k + 1)² => (0, k)...(k, k)...(k, 0) (note: from here, 19² <= i)
    val k = math.floor(math.sqrt(i)).toInt
    j = i - k * k
    if (j < k) return new Pos(j, k)
    new Pos(k, 2 * k - j)
  }
}