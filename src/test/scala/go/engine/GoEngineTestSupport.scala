package strategygames.go.engine

import strategygames.go.{ File, Pos }

object GoEngineTestSupport {

  def engineMove(size: Int, key: String): Int = {
    val pos = Pos.fromKey(key).get
    size * pos.rank.index + pos.file.index
  }

  def playAll(size: Int, keys: List[String]): GoState =
    keys.foldLeft(GoState.initial(size)) { (state, key) =>
      if (key == "pass") state(state.passMove) else state(engineMove(size, key))
    }

  def boardKeys(size: Int): Seq[String] =
    for {
      rank <- 1 to size
      file <- File.all.take(size)
    } yield s"${file}${rank}"
}
