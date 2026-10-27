package strategygames.fiveinarow

sealed abstract class OpeningStep(val fen: Char)

object OpeningStep {

  case object Opening     extends OpeningStep('o')
  case object Choice      extends OpeningStep('c')
  case object Swap2Drops  extends OpeningStep('s')
  case object FinalChoice extends OpeningStep('f')
  case object Placing     extends OpeningStep('p')
  case object SwapOffered extends OpeningStep('q')
  case object Offering    extends OpeningStep('x')
  case object Offered     extends OpeningStep('k')
  case object Play        extends OpeningStep('-')

  val all: List[OpeningStep] =
    List(Opening, Choice, Swap2Drops, FinalChoice, Placing, SwapOffered, Offering, Offered, Play)

  def fromFen(c: Char): Option[OpeningStep] = all.find(_.fen == c)

}
