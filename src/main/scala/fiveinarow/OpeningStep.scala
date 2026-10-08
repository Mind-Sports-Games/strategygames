package strategygames.fiveinarow

sealed abstract class OpeningStep(val fen: Char)

object OpeningStep {

  case object Opening     extends OpeningStep('o')
  case object Choice      extends OpeningStep('c')
  case object Swap2Drops  extends OpeningStep('s')
  case object FinalChoice extends OpeningStep('f')
  case object Play        extends OpeningStep('-')

  val all: List[OpeningStep] = List(Opening, Choice, Swap2Drops, FinalChoice, Play)

  def fromFen(c: Char): Option[OpeningStep] = all.find(_.fen == c)

}
