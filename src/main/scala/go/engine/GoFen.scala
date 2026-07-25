package strategygames.go.engine

sealed abstract class GoFenError extends Product with Serializable

object GoFenError {
  final case class UnexpectedFieldCount(found: Int, fen: String)             extends GoFenError
  final case class UnsupportedBoardSize(rows: Int, fen: String)              extends GoFenError
  final case class MalformedBoardRow(row: String, expected: Int, found: Int) extends GoFenError
  final case class UnknownStoneSymbol(symbol: Char, row: String)             extends GoFenError
  final case class UnknownTurnSymbol(field: String)                          extends GoFenError
  final case class MalformedKoPoint(field: String)                           extends GoFenError
  final case class NonNumericField(name: String, field: String)              extends GoFenError
}

object GoFen {

  private val Pocket                    = "[SSSSSSSSSSssssssssss]"
  private val KoPointOmittedByValidator = "-"
  private val FieldsWithPassCount       = 10
  private val FieldsWithoutPassCount    = 9

  def render(game: GoGame): String = {
    val state = game.state
    s"${renderBoard(state)}$Pocket ${state.turn} $KoPointOmittedByValidator " +
      s"${game.p1FenScore} ${game.p2FenScore} ${state.capturesByBlack} ${state.capturesByWhite} " +
      s"${game.komiTenths} ${game.fenPassCount} ${game.fullMoveNumber}"
  }

  def parse(fen: String): Either[GoFenError, GoGame] = {
    val fields = fen.trim.split(' ')
    if (fields.length != FieldsWithPassCount && fields.length != FieldsWithoutPassCount)
      Left(GoFenError.UnexpectedFieldCount(fields.length, fen))
    else
      for {
        sizeAndOwners   <- parseBoard(fields(0), fen)
        (size, owners)   = sizeAndOwners
        playerTurn      <- parseTurn(fields(1))
        simpleKoMove    <- parseKoPoint(fields(2), size)
        _               <- parseNumber("p1Score", fields(3))
        _               <- parseNumber("p2Score", fields(4))
        capturesByBlack <- parseNumber("p1Captures", fields(5))
        capturesByWhite <- parseNumber("p2Captures", fields(6))
        komiTenths      <- parseNumber("komi", fields(7))
        passCount       <- if (fields.length == FieldsWithPassCount) parseNumber("passCount", fields(8))
                           else Right(0)
        fullMoveNumber  <- parseNumber("fullMove", fields(fields.length - 1))
      } yield {
        val deadStonesSelected = passCount >= GoGame.DeadStonesSelectedPassCount
        val consecutivePasses  =
          Math.max(0, Math.min(passCount, GoGame.HighestOngoingPassCount))
        GoGame(
          GoState.fromStoneOwners(
            size,
            owners(_),
            playerTurn,
            capturesByBlack,
            capturesByWhite,
            simpleKoMove,
            consecutivePasses
          ),
          komiTenths / 10.0,
          plyCountOf(fullMoveNumber, playerTurn),
          deadStonesSelected
        )
      }
  }

  private def plyCountOf(fullMoveNumber: Int, playerTurn: Int): Int =
    Math.max(0, fullMoveNumber * 2 - (if (playerTurn == GoState.BlackPlayer) 2 else 1))

  private def renderBoard(state: GoState): String = {
    val size  = state.size
    val board = new java.lang.StringBuilder(size * size + size)
    var rank  = size
    while (rank >= 1) {
      var file     = 0
      var emptyRun = 0
      while (file < size) {
        state.stoneOwnerAt(size * (rank - 1) + file) match {
          case GoState.BlackPlayer =>
            if (emptyRun > 0) { board.append(emptyRun); emptyRun = 0 }
            board.append('S')
          case GoState.WhitePlayer =>
            if (emptyRun > 0) { board.append(emptyRun); emptyRun = 0 }
            board.append('s')
          case _                   => emptyRun += 1
        }
        file += 1
      }
      if (emptyRun > 0) board.append(emptyRun)
      if (rank > 1) board.append('/')
      rank -= 1
    }
    board.toString
  }

  private def parseBoard(field: String, fen: String): Either[GoFenError, (Int, Array[Int])] = {
    val rows = withoutPocket(field).split('/')
    val size = rows.length
    if (!GoState.supportedSizes.contains(size)) Left(GoFenError.UnsupportedBoardSize(size, fen))
    else {
      val owners  = new Array[Int](size * size)
      val failure = rows.zipWithIndex.foldLeft(Option.empty[GoFenError]) { case (found, (row, rowIndex)) =>
        found.orElse(readRow(row, size, size - rowIndex, owners))
      }
      failure.toLeft((size, owners))
    }
  }

  private def withoutPocket(field: String): String = {
    val pocketStart = field.indexOf('[')
    if (pocketStart >= 0) field.substring(0, pocketStart) else field
  }

  private def readRow(row: String, size: Int, rank: Int, owners: Array[Int]): Option[GoFenError] = {
    var file                        = 0
    var emptyRun                    = 0
    var index                       = 0
    var failure: Option[GoFenError] = None
    while (index < row.length && failure.isEmpty) {
      val symbol = row.charAt(index)
      if (symbol >= '0' && symbol <= '9') emptyRun = emptyRun * 10 + (symbol - '0')
      else {
        file += emptyRun
        emptyRun = 0
        if (file >= size) failure = Some(GoFenError.MalformedBoardRow(row, size, file + 1))
        else if (symbol == 'S') {
          owners(size * (rank - 1) + file) = GoState.BlackPlayer
          file += 1
        } else if (symbol == 's') {
          owners(size * (rank - 1) + file) = GoState.WhitePlayer
          file += 1
        } else failure = Some(GoFenError.UnknownStoneSymbol(symbol, row))
      }
      index += 1
    }
    file += emptyRun
    failure.orElse(Option.when(file != size)(GoFenError.MalformedBoardRow(row, size, file)))
  }

  private def parseTurn(field: String): Either[GoFenError, Int] = field match {
    case "b" => Right(GoState.BlackPlayer)
    case "w" => Right(GoState.WhitePlayer)
    case _   => Left(GoFenError.UnknownTurnSymbol(field))
  }

  private def parseKoPoint(field: String, size: Int): Either[GoFenError, Option[Int]] =
    if (field == KoPointOmittedByValidator) Right(None)
    else {
      val file = field.headOption.map(_ - 'a').getOrElse(-1)
      val rank = field.drop(1).toIntOption.getOrElse(0)
      if (file < 0 || file >= size || rank < 1 || rank > size) Left(GoFenError.MalformedKoPoint(field))
      else Right(Some(size * (rank - 1) + file))
    }

  private def parseNumber(name: String, field: String): Either[GoFenError, Int] =
    field.toIntOption.toRight(GoFenError.NonNumericField(name, field))
}
