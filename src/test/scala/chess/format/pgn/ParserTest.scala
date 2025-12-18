package strategygames.chess
package format.pgn

import strategygames.format.pgn._

import cats.syntax.option._

import strategygames.chess.variant.Standard
import strategygames.chess.format.pgn.{ Parser => ChessParser }

class ParserTest extends ChessTest {

  import Fixtures._

  val parser                 = ChessParser.full _
  def parseMove(str: String) = ChessParser.ActionParser(str, Standard)

  "promotion check" should {
    "as a queen" in {
      parser("b8=Q ") .toOption must beSome.like { case a =>
        a.sans.value.headOption must beSome.like { case san: Std =>
          san.promotion === Option(Queen)
        }
      }
    }
    "as a rook" in {
      parser("b8=R ") .toOption must beSome.like { case a =>
        a.sans.value.headOption must beSome.like { case san: Std =>
          san.promotion === Option(Rook)
        }
      }
    }
  }

  "result" in {
    "no tag but inline result" in {
      parser(noTagButResult) .toOption must beSome.like { case parsed =>
        parsed.tags("Result") === Option("1-0")
      }
    }
    "in tags" in {
      parser(p1ResignsInTags) .toOption must beSome.like { case parsed =>
        parsed.tags("Result") === Option("0-1")
      }
    }
    "in moves" in {
      parser(p1ResignsInMoves) .toOption must beSome.like { case parsed =>
        parsed.tags("Result") === Option("0-1")
      }
    }
    "in tags and moves" in {
      parser(p1ResignsInTagsAndMoves) .toOption must beSome.like { case parsed =>
        parsed.tags("Result") === Option("0-1")
      }
    }
  }

  "glyphs" in {
    parseMove("e4") .toOption must beSome.like { case a =>
      a === Std(Pos.E4, Pawn)
    }
    parseMove("e4!") .toOption must beSome.like { case a: Std =>
      a.dest === Pos.E4
      a.role === Pawn
      a.metas.glyphs === Glyphs(Glyph.MoveAssessment.good.some, None, Nil)
    }
    parseMove("Ne7g6+?!") .toOption must beSome.like { case a: Std =>
      a.dest === Pos.G6
      a.role === Knight
      a.metas.glyphs === Glyphs(Glyph.MoveAssessment.dubious.some, None, Nil)
    }
    parser("Ne7g6+!").isValid === true
    parseMove("P@e4?!") .toOption must beSome.like { case a: Drop =>
      a.pos === Pos.E4
      a.role === Pawn
      a.metas.glyphs === Glyphs(Glyph.MoveAssessment.dubious.some, None, Nil)
    }
  }

  "nags" in {
    parser(withNag).isValid === true

    parser("Ne7g6+! $13") .toOption must beSome.like { case ParsedPgn(_, _, Sans(List(san))) =>
      san.metas.glyphs.move === Option(Glyph.MoveAssessment.good)
      san.metas.glyphs.position === Option(Glyph.PositionAssessment.unclear)
    }
  }

  "non-nags" in {
    parser(withGlyphAnnotations).isValid === true

    parser("Bxd3?? ∞") .toOption must beSome.like { case ParsedPgn(_, _, Sans(List(san))) =>
      san.metas.glyphs.move === Option(Glyph.MoveAssessment.blunder)
      san.metas.glyphs.position === Option(Glyph.PositionAssessment.unclear)
    }
  }

  "comments" in {
    parser("Ne7g6+! {such a neat comment}") .toOption must beSome.like { case ParsedPgn(_, _, Sans(List(san))) =>
      san.metas.comments === List("such a neat comment")
    }
  }

  "variations" in {
    parser("Ne7g6+! {such a neat comment} (e4 Ng6)") .toOption must beSome.like {
      case ParsedPgn(_, _, Sans(List(san))) =>
        san.metas.variations.headOption must beSome.like { case variation =>
          variation.value must haveSize(2)
        }
    }
  }

  "first move variation" in {
    parser("1. e4 (1. d4)") .toOption must beSome.like { case ParsedPgn(_, _, Sans(List(san))) =>
      san.metas.variations.headOption must beSome.like { case variation =>
        variation.value must haveSize(1)
      }
    }
  }

  raws foreach { sans =>
    val size = sans.split(' ').length
    "sans only size: " + size in {
      parser(sans) .toOption must beSome.like { case a =>
        a.sans.value.size === size
      }
    }
  }

  (shortCastles ++ longCastles ++ annotatedCastles) foreach { sans =>
    val size = sans.split(' ').length
    "sans only size: " + size in {
      parser(sans) .toOption must beSome.like { case a =>
        a.sans.value.size === size
      }
    }
  }

  "disambiguated" in {
    parser(disambiguated) .toOption must beSome.like { case a =>
      a.sans.value.size === 3
    }
  }

  List(fromProd1, fromProd2, castleCheck1, castleCheck2) foreach { sans =>
    val size = sans.split(' ').length
    "sans only from prod size: " + size in {
      parser(sans) .toOption must beSome.like { case a =>
        a.sans.value.size === size
      }
    }
  }

  "variations" in {
    parser(variations) .toOption must beSome.like { case a =>
      a.sans.value.size === 20
    }
  }

  "inline tags" in {
    parser(inlineTags) .toOption must beSome.like { case a =>
      a.tags.value must contain { (tag: Tag) =>
        tag.name == Tag.P1 && tag.value == "Blazquez, Denis"
      }
    }
  }

  "tag with nested quotes" in {
    parser("""[P2 "Schwarzenegger, Arnold \"The Terminator\""]""") .toOption must beSome.like { case a =>
      a.tags.value must contain { (tag: Tag) =>
        tag.name == Tag.P2 && tag.value == """Schwarzenegger, Arnold "The Terminator""""
      }
    }
  }

  "tag with inner brackets" in {
    parser("""[P2 "[=0040.34h5a4]"]""") .toOption must beSome.like { case a =>
      a.tags.value must contain { (tag: Tag) =>
        tag.name == Tag.P2 && tag.value == "[=0040.34h5a4]"
      }
    }
  }

  "game from wikipedia" in {
    parser(fromWikipedia) .toOption must beSome.like { case a =>
      a.sans.value.size === 85
    }
  }

  "game from crafty" in {
    parser(fromCrafty) .toOption must beSome.like { case a =>
      a.sans.value.size === 68
    }
  }

  "inline comments" in {
    parser(inlineComments) .toOption must beSome.like { case a =>
      a.sans.value.size === 85
    }
  }

  "comments and variations" in {
    parser(commentsAndVariations) .toOption must beSome.like { case a =>
      a.sans.value.size === 103
    }
  }

  "comments and lines by smartchess" in {
    parser(bySmartChess) .toOption must beSome.like { case a =>
      a.sans.value.size === 65
    }
  }

  "complete 960" in {
    parser(complete960) .toOption must beSome.like { case a =>
      a.sans.value.size === 42
    }
  }

  "TCEC" in {
    parser(fromTcec) .toOption must beSome.like { case a =>
      a.sans.value.size === 142
    }
  }

  "TCEC with engine output" in {
    parser(fromTcecWithEngineOutput) .toOption must beSome.like { case a =>
      a.sans.value.size === 165
    }
  }

  "chesskids iphone" in {
    parser(chesskids) .toOption must beSome.like { case a =>
      a.sans.value.size === 135
    }
  }

  "handwritten" in {
    parser(handwritten) .toOption must beSome.like { case a =>
      a.sans.value.size === 139
    }
  }

  "chess by post" in {
    parser(chessByPost) .toOption must beSome.like { case a =>
      a.sans.value.size === 100
    }
  }

  "Android device" in {
    parser(android) .toOption must beSome.like { case a =>
      a.sans.value.size === 69
    }
  }

  "weird dashes" in {
    parser(weirdDashes) .toOption must beSome.like { case a =>
      a.sans.value.size === 74
    }
  }

  "lichobile" in {
    parser(lichobile) .toOption must beSome.like { case a =>
      a.sans.value.size === 68
    }
  }

  "overflow" in {
    parser(overflow) .toOption must beSome.like { case a =>
      a.sans.value.size === 67
    }
  }
  "overflow 2" in {
    parser(stackOverflow) .toOption must beSome.like { case a =>
      a.sans.value.size === 8
    }
  }
  "overflow 3" in {
    parser(overflow3) .toOption must beSome.like { case a =>
      a.sans.value.size === 343
    }
  }
  "overflow 3: tags" in {
    ChessParser.TagParser.fromFullPgn(overflow3) .toOption must beSome.like { case tags =>
      tags.value.size === 9
    }
  }
  "chessbase arrows" in {
    parser(chessbaseArrows) .toOption must beSome.like { case a =>
      a.initialPosition.comments === List(
        "[%csl Gb4,Yd5,Rf6][%cal Ge2e4,Ye2d4,Re2g4]"
      )
    }
  }
  "chessbase weird" in {
    parser(chessbaseWeird) .toOption must beSome.like { case a =>
      a.sans.value.size === 115
    }
  }
  "crazyhouse from prod" in {
    parser(crazyhouseFromProd) .toOption must beSome.like { case a =>
      a.sans.value.size === 49
    }
  }
  "crazyhouse from chess.com" in {
    parser(chessComCrazyhouse) .toOption must beSome.like { case a =>
      a.sans.value.size === 42
    }
  }
  "en passant e.p. notation" in {
    parser(enpassantEP) .toOption must beSome.like { case a =>
      a.sans.value.size === 36
    }
    parser(enpassantEP2) .toOption must beSome.like { case a =>
      a.sans.value.size === 36
    }
  }

  "year" in {
    "full date" in {
      parser(recentChessCom) .toOption must beSome.like { case parsed =>
        parsed.tags.year === Option(2016)
      }
    }
    "only year" in {
      parser(explorerPartialDate) .toOption must beSome.like { case parsed =>
        parsed.tags.year === Option(1978)
      }
    }
  }

  "weird variant names" in {
    parser(stLouisFischerandom) .toOption must beSome.like { case parsed =>
      parsed.tags.variant === Option(
        strategygames.variant.Variant.wrap(strategygames.chess.variant.Chess960)
      )
    }
  }

  "example from chessgames.com with weird comments" in {
    parser(chessgamesWeirdComments).isValid === true
  }
}
