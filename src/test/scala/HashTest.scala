package strategygames

import org.specs2.mutable.Specification
import org.specs2.specification.core.Fragment

import strategygames.variant.Variant

class HashTest extends Specification {

  private def ownLong(situation: Situation, start: Int): Long = situation match {
    case Situation.Chess(s)        => chess.Hash.get(s, new chess.Hash.ZobristConstants(start))
    case Situation.Draughts(s)     => draughts.Hash.get(s, new draughts.Hash.ZobristConstants(start))
    case Situation.FairySF(s)      => fairysf.Hash.get(s, new fairysf.Hash.ZobristConstants(start))
    case Situation.Samurai(s)      => samurai.Hash.get(s, new samurai.Hash.ZobristConstants(start))
    case Situation.Togyzkumalak(s) => togyzkumalak.Hash.get(s, new togyzkumalak.Hash.ZobristConstants(start))
    case Situation.Go(s)           => go.Hash.get(s, new go.Hash.ZobristConstants(start))
    case Situation.Backgammon(s)   => backgammon.Hash.get(s, new backgammon.Hash.ZobristConstants(start))
    case Situation.Abalone(s)      => abalone.Hash.get(s, new abalone.Hash.ZobristConstants(start))
    case Situation.Dameo(s)        => dameo.Hash.get(s, new dameo.Hash.ZobristConstants(start))
    case Situation.Entropy(s)      => entropy.Hash.get(s, new entropy.Hash.ZobristConstants(start))
    case Situation.Yonkoma(s)      => yonkoma.Hash.get(s, new yonkoma.Hash.ZobristConstants(start))
  }

  private def ownBytes(situation: Situation, size: Int): Seq[Byte] = situation match {
    case Situation.Chess(s)        => new chess.Hash(size)(s).toSeq
    case Situation.Draughts(s)     => new draughts.Hash(size)(s).toSeq
    case Situation.FairySF(s)      => new fairysf.Hash(size)(s).toSeq
    case Situation.Samurai(s)      => new samurai.Hash(size)(s).toSeq
    case Situation.Togyzkumalak(s) => new togyzkumalak.Hash(size)(s).toSeq
    case Situation.Go(s)           => new go.Hash(size)(s).toSeq
    case Situation.Backgammon(s)   => new backgammon.Hash(size)(s).toSeq
    case Situation.Abalone(s)      => new abalone.Hash(size)(s).toSeq
    case Situation.Dameo(s)        => new dameo.Hash(size)(s).toSeq
    case Situation.Entropy(s)      => new entropy.Hash(size)(s).toSeq
    case Situation.Yonkoma(s)      => new yonkoma.Hash(size)(s).toSeq
  }

  private def longBytes(l: Long): Seq[Byte] = (0 until 8).map(i => (l >>> ((7 - i) * 8)).toByte)

  private def positions(lib: GameLogic, variant: Variant): List[Situation] =
    Iterator
      .iterate(Option(Situation(lib, variant)))(
        _.flatMap(s => s.actions.lift(s.actions.size / 2).map(_.situationAfter))
      )
      .take(7)
      .flatten
      .toList

  "the wrapper's hashes" should {

    Fragment.foreach(GameLogic.all.flatMap(lib => Variant.all(lib).map(lib -> _))) { (lib, variant) =>
      s"match the ${lib.name} package's own hashes for ${variant.key}" in {
        val situations = positions(lib, variant)
        situations.size must be_>(1)
        foreach(situations) { situation =>
          val polyglot = ownLong(situation, 0)
          val random   = ownLong(situation, 16)
          Hash.long(lib, situation) === polyglot
          Hash(lib, situation).toSeq === longBytes(polyglot).take(Hash.size)
          Hash(lib, situation).toSeq === ownBytes(situation, Hash.size)
          new Hash(8)(lib, situation).toSeq === longBytes(polyglot)
          new Hash(16)(lib, situation).toSeq === longBytes(polyglot) ++ longBytes(random)
          new Hash(16)(lib, situation).toSeq === ownBytes(situation, 16)
          new Hash(12)(lib, situation).toSeq === ownBytes(situation, 12)
        }
      }
    }
  }
}
