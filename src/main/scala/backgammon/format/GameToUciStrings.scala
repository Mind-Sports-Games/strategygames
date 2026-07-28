package strategygames.backgammon.format

import cats.data.Validated

import strategygames.{ ActionStrs, Player }
import strategygames.backgammon.Pos
import strategygames.backgammon.variant.Variant

object GameToUciStrings {

  private type PointMap = Map[Pos, (Player, Int)]

  def apply(
      actionStrs: ActionStrs,
      initialFen: Option[FEN],
      variant: Variant
  ): Validated[String, String] = {
    val startFen    = initialFen.getOrElse(variant.initialFen)
    val startPlayer = startFen.player.getOrElse(Player.P1)
    val startPieces = startFen.pieces.map { case (pos, (piece, count)) => pos -> (piece.player, count) }
    Validated.valid(render(actionStrs, startPieces, startPlayer))
  }

  private def render(actionStrs: ActionStrs, startPieces: PointMap, startPlayer: Player): String =
    actionStrs
      .foldLeft((startPieces, startPlayer, Vector.empty[String])) { case ((pieces, player, acc), turn) =>
        val (piecesAfter, rendered) = turn.foldLeft((pieces, Vector.empty[String])) {
          case ((current, tokens), action) =>
            val (next, token) = applyAction(current, player, action)
            (next, tokens :+ token)
        }
        (piecesAfter, !player, acc :+ rendered.mkString(","))
      }
      ._3
      .mkString(" ")

  private def applyAction(pieces: PointMap, player: Player, action: String): (PointMap, String) =
    if (action.lift(1).contains('@')) drop(pieces, player, action)
    else if (action.contains('^')) (lift(pieces, action), action)
    else if (action.contains('/')) (pieces, action)
    else if (action == "endturn" || action == "roll" || action == "undo") (pieces, action)
    else if (action.startsWith("cube")) (pieces, action)
    else move(pieces, action)

  private def move(pieces: PointMap, action: String): (PointMap, String) = {
    val bare = stripCapture(action)
    (Pos.fromKey(bare.take(2)), Pos.fromKey(bare.slice(2, 4))) match {
      case (Some(orig), Some(dest)) =>
        pieces.get(orig).map(_._1) match {
          case Some(mover) =>
            val captured = pieces.get(dest).exists(_._1 != mover)
            (place(removeOne(pieces, orig), dest, mover), if (captured) s"${bare}x" else bare)
          case None        => (pieces, bare)
        }
      case _                        => (pieces, bare)
    }
  }

  private def drop(pieces: PointMap, player: Player, action: String): (PointMap, String) = {
    val bare = stripCapture(action)
    Pos.fromKey(keyAfter(bare, '@')) match {
      case Some(dest) =>
        val captured = pieces.get(dest).exists(_._1 != player)
        (place(pieces, dest, player), if (captured) s"${bare}x" else bare)
      case None       => (pieces, bare)
    }
  }

  private def lift(pieces: PointMap, action: String): PointMap =
    Pos.fromKey(keyAfter(action, '^')).map(removeOne(pieces, _)).getOrElse(pieces)

  private def place(pieces: PointMap, pos: Pos, player: Player): PointMap =
    pieces.get(pos) match {
      case Some((owner, count)) if owner == player => pieces.updated(pos, (player, count + 1))
      case _                                       => pieces.updated(pos, (player, 1))
    }

  private def removeOne(pieces: PointMap, pos: Pos): PointMap =
    pieces.get(pos) match {
      case Some((owner, count)) if count > 1 => pieces.updated(pos, (owner, count - 1))
      case Some(_)                           => pieces - pos
      case None                              => pieces
    }

  private def stripCapture(action: String): String =
    if (action.endsWith("x")) action.dropRight(1) else action

  private def keyAfter(action: String, sep: Char): String =
    action.substring(action.indexOf(sep) + 1)
}
