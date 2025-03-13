package abalone.util.geometry

class Cell(var x: Int, var y: Int) extends AnyRef {
  def copy(a: Cell): Cell = new Cell(a.x, a.y)

  def +(a: Cell): Cell = copy(this).add(a)

  def add(a: Cell): Cell = add(a.x, a.y)

  def add(x: Int, y: Int): Cell = {
    this.x += x
    this.y += y
    this
  }

  def -(a: Cell): Cell = copy(this).sub(a)

  def sub(a: Cell): Cell = sub(a.x, a.y)

  def sub(a: Int, b: Int): Cell = add(-a, -b)

  def *(a: Int): Cell = copy(this).mult(a)

  def *(a: Int, b: Int): Cell = copy(this).mult(a, b)

  def mult(a: Int): Cell = mult(a, a)

  def mult(a: Int, b: Int): Cell = {
    x *= a
    y *= b
    this
  }

  def /(a: Int): Cell = copy(this).div(a)

  def /(a: Int, b: Int): Cell = copy(this).div(a, b)

  def div(a: Int): Cell = div(a, a)

  def div(a: Int, b: Int): Cell = {
    x /= a
    y /= b
    this
  }

  //
  //
  def piotr: Char = Piotr.cellToPiotr(this)

  def piotrStr = piotr.toString

  //
  //
  def scal(a: Cell): Int = scal(a.x, a.y)

  def scal(a: Int, b: Int): Int = x * a + y * b

  def scal3(a: Cell): Double = scal3(a.x, a.y)

  def scal3(a: Int, b: Int): Double = (x - y / 2d) * (a - b / 2d) + y * b * 3 / 4d

  def cross(a: Cell): Int = cross(a.x, a.y)

  def cross(a: Int, b: Int): Int = x * b - y * a

  def cross3(a: Cell): Double = cross3(a.x, a.y)

  def cross3(a: Int, b: Int): Double = Cell.cross(x - y / 2d, y * math.sqrt(3) / 2, a - b / 2d, b * math.sqrt(3) / 2)
}

object Cell {
  def cross(x: Double, y: Double, a: Double, b: Double): Double = x * b - y * a
}

object Piotr {
  /** https://en.wikipedia.org/wiki/List_of_Unicode_characters#Latin_script, from the latin-1 script. */
  private val piotrs: Array[Char] = Array(
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

  val cellToIndex: Map[Cell, Int] = Range(0, piotrs.size).map(i => (indexToCell(i), i)).toMap
  val indexToPiotr: Map[Int, Char] = Range(0, piotrs.size).map(i => (i, piotrs(i))).toMap // <=> lookup
  val cellToPiotr: Map[Cell, Char] = cellToIndex.keys.map(a => (a, indexToPiotr(cellToIndex(a)))).toMap

  val piotrToIndex: Map[Char, Int] = indexToPiotr.keys.map(i => (indexToPiotr(i), i)).toMap
  val piotrToCell: Map[Char, Cell] = cellToPiotr.keys.map(a => (cellToPiotr(a), a)).toMap // <=> allPiotrs

  private def indexToCell(i: Int): Cell = {
    var j = i
    if (j < 64) new Cell(j % 8, j / 8) // A1-H1, ..., A8-H8

    j -= 64
    if (j < 20) new Cell(j % 10, j / 10) // A9-J9, A10-J10

    j -= 20
    if (j < 16) new Cell(8 + j % 2, j / 2) // I1, J1, ... I8, J8

    j -= 16
    if (j < 90) new Cell(10 + j % 9, j / 9) // K1-S1, ..., K10-S10

    j -= 90
    if (j < 171) new Cell(j % 19, 11 + j / 9) // A11-S11, ..., A19-S19

    // k² <= i < (k + 1)² => (0, k)...(k, k)...(k, 0) (note: from here, 19² <= i)
    val k = math.floor(math.sqrt(i)).toInt
    j = i - k * k
    if (j < k) new Cell(j, k)
    new Cell(k, 2 * k - j)
  }
}