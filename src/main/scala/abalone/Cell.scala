package abalone

class Cell(var x: Int, var y: Int) extends AnyRef {
  def copy(a: Cell): Cell = new Cell(a.x, a.y)

  def +(a: Cell): Unit = add(a)

  def add(a: Cell): Unit = add(a.x, a.y)

  def add(x: Int, y: Int): Unit = {
    this.x += x
    this.y += y
  }

  def -(a: Cell): Unit = sub(a)

  def sub(a: Cell): Unit = sub(a.x, a.y)

  def sub(a: Int, b: Int): Unit = add(-a, -b)

  def *(a: Int): Unit = mult(a)

  def *(a: Int, b: Int): Unit = mult(a, b)

  def mult(a: Int): Unit = mult(a, a)

  def mult(a: Int, b: Int): Unit = {
    x *= a
    y *= b
  }

  def /(a: Int): Unit = div(a)

  def /(a: Int, b: Int): Unit = div(a, b)

  def div(a: Int): Unit = div(a, a)

  def div(a: Int, b: Int): Unit = {
    x /= a
    y /= b
  }

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