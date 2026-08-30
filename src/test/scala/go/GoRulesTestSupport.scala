package strategygames.go

import strategygames.go.format.{ FEN, Forsyth }
import strategygames.go.variant.Variant

trait GoRulesTestSupport {

  protected val pocket = "[SSSSSSSSSSssssssssss]"

  protected def playing(variant: Variant, actions: List[String]): Game =
    playingOn(Game(variant), actions)

  protected def playingFrom(fen: FEN, actions: List[String]): Game =
    playingOn(Game(Some(fen.variant), Some(fen)), actions)

  protected def playingOn(game: Game, actions: List[String]): Game =
    actions.foldLeft(game)(afterAction)

  protected def situationFrom(fen: FEN): Situation =
    Forsyth.<<(fen).getOrElse(sys.error(s"unreadable go fen: ${fen.value}"))

  protected def dropKeysOf(situation: Situation): List[String] =
    situation.board.variant.validDrops(situation).map(_.pos.key)

  protected def fenOf(game: Game): FEN = Forsyth.>>(game)

  protected def fenOf(situation: Situation): FEN = Forsyth.>>(situation)

  protected def koPointOf(fen: FEN): String = fen.value.split(' ')(2)

  protected def stoneAt(game: Game, key: String): Option[Piece] = game.board.pieces.get(pointAt(key))

  protected def pointAt(key: String): Pos =
    Pos.fromKey(key).getOrElse(sys.error(s"no such go point: ${key}"))

  private def afterAction(game: Game, action: String): Game = action match {
    case "pass"                                     => game.apply(game.board.variant.validPass(game.situation))
    case settlement if settlement.startsWith("ss:") =>
      game.apply(
        game.situation
          .selectSquares(settledPointsOf(settlement))
          .getOrElse(sys.error(s"cannot settle with ${settlement}"))
      )
    case key                                        =>
      game.apply(
        game.board.variant
          .validDrops(game.situation)
          .find(_.pos.key == key)
          .getOrElse(sys.error(s"${key} is not a legal drop"))
      )
  }

  private def settledPointsOf(settlement: String): List[Pos] =
    settlement.drop(3).split(",").toList.filter(_.nonEmpty).map(pointAt)
}
