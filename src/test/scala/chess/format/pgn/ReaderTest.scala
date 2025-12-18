package strategygames.chess
package format.pgn

class ReaderTest extends ChessTest {

  import Fixtures._
  import Reader.Result._

  "only raw moves" should {
    "many games" in {
      forall(raws) { (c: String) =>
        Reader.full(c).toOption must beSome.like { case Complete(replay) =>
          replay.actions must haveSize(c.split(' ').length)
        }
      }
    }
    "example from prod 1" in {
      Reader.full(fromProd1).isValid === true
    }
    "example from prod 2" in {
      Reader.full(fromProd2).isValid === true
    }
    "rook promotion" in {
      Reader.full(promoteRook).isValid === true
    }
    "castle check O-O-O+" in {
      Reader.full(castleCheck1).isValid === true
    }
    "castle checkmate O-O#" in {
      Reader.full(castleCheck2).isValid === true
    }
    "and delimiters" in {
      Reader.full(withDelimiters).toOption must beSome.like { case Complete(replay) =>
        replay.actions must haveSize(33)
      }
    }
    "and delimiters on new lines" in {
      Reader.full(withDelimitersOnNewLines).toOption must beSome.like { case Complete(replay) =>
        replay.actions must haveSize(33)
      }
    }
  }
  "tags and moves" should {
    "chess960" in {
      Reader.full(complete960).isValid === true
    }
    "with empty lines" in {
      Reader.full("\n" + complete960 + "\n").isValid === true
    }
    "example from wikipedia" in {
      Reader.full(fromWikipedia).isValid === true
    }
    "with inline comments" in {
      Reader.full(inlineComments).isValid === true
    }
    "example from chessgames.com" in {
      Reader.full(fromChessgames).isValid === true
    }
    "example from chessgames.com with escape chars" in {
      Reader.full(fromChessgamesWithEscapeChar).isValid === true
    }
    "immortal with NAG" in {
      Reader.full(withNag).isValid === true
    }
    "example from TCEC" in {
      Reader.full(fromTcec).isValid === true
    }
    "from https://chessprogramming.wikispaces.com/Kasparov+versus+Deep+Blue+1996" in {
      Reader.full(fromChessProgrammingWiki).isValid === true
    }
    "comments and variations" in {
      Reader.full(commentsAndVariations).isValid === true
    }
    "comments and variations by smartchess" in {
      Reader.full(bySmartChess).isValid === true
    }
    "invalid variant" in {
      Reader.full(invalidVariant) .toOption must beSome.like { case Complete(replay) =>
        replay.setup.board.variant === variant.Standard
      }
    }
    "promoting to a rook" in {
      Reader.full(fromLichessBadPromotion) .toOption must beSome.like { case Complete(replay) =>
        replay.chronoPlies lift 10 must beSome.like { case move: strategygames.chess.Move =>
          move.promotion === Option(Rook)
        }
      }
    }
    "chessbase arrows" in {
      Reader.full(chessbaseArrows).isValid === true
    }
    "atomic regression" in {
      Reader.full(atomicRegression).isValid === true
    }
    "atomic promotion" in {
      Reader.full(atomicPromotion).isValid === true
    }
    "lichobile export" in {
      Reader.full(lichobile).isValid === true
    }
    "crazyhouse 1" in {
      Reader.full(crazyhouse1) .toOption must beSome.like { case Complete(replay) =>
        replay.chronoPlies lift 11 must beSome.like {
          _.toUci.uci === "P@c6"
        }
      }
    }
    "crazyhouse 2" in {
      Reader.full(crazyhouse2) .toOption must beSome.like { case Complete(replay) =>
        replay.chronoPlies.size === 111
      }
    }
    "crazyhouse without variant tag" in {
      Reader.full(crazyhouseNoVariantTag) .toOption must beSome.like { case Incomplete(replay, _) =>
        replay.chronoPlies.size === 8
      }
    }
    "crazyhouse from chess.com" in {
      Reader.full(chessComCrazyhouse).isValid === true
    }
  }
  "from prod" in {
    "from position close chess" in {
      Reader.full(fromPosProdCloseChess) .toOption must beSome.like { case Complete(replay) =>
        replay.chronoPlies.size === 152
      }
    }
    "from position empty FEN" in {
      Reader.full(fromPositionEmptyFen) .toOption must beSome.like { case Complete(replay) =>
        replay.chronoPlies.size === 164
      }
    }
    "preserves initial ply" in {
      Reader.full(caissa) .toOption must beSome.like { case Complete(replay) =>
        replay.setup.plies === 43
        replay.setup.startedAtTurn === 43
        replay.setup.startedAtPly === 43
        replay.state.startedAtTurn === 43
        replay.state.startedAtPly === 43
      }
    }
  }
  "partial from broadcast" in {
    Reader.full(festivalFigueira) .toOption must beSome.like { case Incomplete(replay, _) =>
      replay.chronoPlies.size === 113
    }
  }
  "invisible char" in {
    Reader.full(invisibleChar) .toOption must beSome.like { case Complete(replay) =>
      replay.chronoPlies.size === 19
    }
  }
}
