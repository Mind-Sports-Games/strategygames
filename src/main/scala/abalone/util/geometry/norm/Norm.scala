package abalone.util.geometry.norm

import abalone.util.geometry.Cell

abstract class Norm(val radius: Int) {
  final def apply(a: Cell): Int = this (a.x, a.y)

  def apply(x: Int, y: Int): Int

  final def dist(a: Cell, b: Cell): Int = dist(a, b.x, b.y)

  final def dist(a: Cell, x: Int, y: Int): Int = dist(a.x, a.y, x, y)

  final def dist(x: Int, y: Int, a: Cell): Int = dist(x, y, a.x, a.y)

  final def dist(x: Int, y: Int, z: Int, t: Int): Int = this (z - x, t - y)

  final def vectors: Set[Cell] = (0 to radius)
    .flatMap(y => (0 to radius)
      .filter(x => this (x, y) == 1)
      .map(x => new Cell(x, y))
    )
    .toSet
}