package strategygames.go
package variant

trait Go9x9Setup extends Variant {

  override def initialFen =
    format.FEN("9/9/9/9/9/9/9/9/9[SSSSSSSSSSssssssssss] b - 0 55 0 0 55 0 1")

  override def komi: Double = 5.5

  override def boardFenFromHandicap(handicap: Int): String = {
    handicap match {
      case 1           => "9/9/2S6/9/9/9/9/9/9"
      case 2           => "9/9/6S2/9/9/9/2S6/9/9"
      case 3           => "9/9/6S2/9/9/9/2S3S2/9/9"
      case 4           => "9/9/2S3S2/9/9/9/2S3S2/9/9"
      case 5           => "9/9/2S3S2/9/4S4/9/2S3S2/9/9"
      case 6           => "9/9/2S3S2/9/2S3S2/9/2S3S2/9/9"
      case 7           => "9/9/2S3S2/9/2S1S1S2/9/2S3S2/9/9"
      case 8           => "9/9/2S1S1S2/9/2S3S2/9/2S1S1S2/9/9"
      case 9           => "9/9/2S1S1S2/9/2S1S1S2/9/2S1S1S2/9/9"
      case 10          => "9/1S7/2S1S1S2/9/2S1S1S2/9/2S1S1S2/9/9"
      case 11          => "9/1S5S1/2S1S1S2/9/2S1S1S2/9/2S1S1S2/9/9"
      case 12          => "9/1S5S1/2S1S1S2/9/2S1S1S2/9/2S1S1S2/7S1/9"
      case 13          => "9/1S5S1/2S1S1S2/9/2S1S1S2/9/2S1S1S2/1S5S1/9"
      case 14          => "9/1S5S1/2S1S1S2/3S5/2S1S1S2/9/2S1S1S2/1S5S1/9"
      case 15          => "9/1S5S1/2S1S1S2/3S5/2S1S1S2/5S3/2S1S1S2/1S5S1/9"
      case 16          => "9/1S5S1/2S1S1S2/3S1S3/2S1S1S2/5S3/2S1S1S2/1S5S1/9"
      case 17          => "9/1S5S1/2S1S1S2/3S1S3/2S1S1S2/3S1S3/2S1S1S2/1S5S1/9"
      case 18          => "9/1S1S3S1/2S1S1S2/3S1S3/2S1S1S2/3S1S3/2S1S1S2/1S5S1/9"
      case 19          => "9/1S1S1S1S1/2S1S1S2/3S1S3/2S1S1S2/3S1S3/2S1S1S2/1S5S1/9"
      case 20          => "9/1S1S1S1S1/2S1S1S2/3S1S1S1/2S1S1S2/3S1S3/2S1S1S2/1S5S1/9"
      case 21          => "9/1S1S1S1S1/2S1S1S2/3S1S1S1/2S1S1S2/3S1S1S1/2S1S1S2/1S5S1/9"
      case 22          => "9/1S1S1S1S1/2S1S1S2/3S1S1S1/2S1S1S2/3S1S1S1/2S1S1S2/1S3S1S1/9"
      case 23          => "9/1S1S1S1S1/2S1S1S2/3S1S1S1/2S1S1S2/3S1S1S1/2S1S1S2/1S1S1S1S1/9"
      case 24          => "9/1S1S1S1S1/2S1S1S2/3S1S1S1/2S1S1S2/1S1S1S1S1/2S1S1S2/1S1S1S1S1/9"
      case 25          => "9/1S1S1S1S1/2S1S1S2/1S1S1S1S1/2S1S1S2/1S1S1S1S1/2S1S1S2/1S1S1S1S1/9"
      case x if x > 25 => "9/1S1S1S1S1/2S1S1S2/1S1S1S1S1/2S1S1S2/1S1S1S1S1/2S1S1S2/1S1S1S1S1/9"
      case _           => "9/9/9/9/9/9/9/9/9"
    }
  }

}
