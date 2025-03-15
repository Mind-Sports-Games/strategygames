package strategygames.abalone

final class Hash(size: Int) {
  @deprecated("Alex", since = "1.5.5") def apply(situation: Situation): PositionHash = {
    val l = Hash.get(situation, Hash.polyglotTable)
    if (size <= 8) {
      Array.tabulate(size)(i => (l >>> ((7 - i) * 8)).toByte)
    } else {
      val m = Hash.get(situation, Hash.randomTable)
      Array.tabulate(size)(i =>
        if (i < 8) (l >>> ((7 - i) * 8)).toByte
        else (m >>> ((15 - i) * 8)).toByte
      )
    }
  }

  def apply(situation: SSituation): PositionHash = {
    val l = Hash.get(situation, Hash.polyglotTable)
    if (size <= 8) {
      Array.tabulate(size)(i => (l >>> ((7 - i) * 8)).toByte)
    } else {
      val m = Hash.get(situation, Hash.randomTable)
      Array.tabulate(size)(i =>
        if (i < 8) (l >>> ((7 - i) * 8)).toByte
        else (m >>> ((15 - i) * 8)).toByte
      )
    }
  }
}

object Hash {

  val size = 3

  class ZobristConstants(start: Int) {
    def hexToLong(s: String): Long =
      (java.lang.Long.parseLong(s.substring(start, start + 8), 16) << 32) |
        java.lang.Long.parseLong(s.substring(start + 8, start + 16), 16)

    val p1TurnMask = hexToLong(ZobristTables.p1TurnMask)
    val actorMasks = ZobristTables.actorMasks.map(hexToLong)
  }

  object ZobristConstants {}

  // The following masks are compatible with the Polyglot
  // opening book format.
  private val polyglotTable = new ZobristConstants(0)
  private lazy val randomTable = new ZobristConstants(16)

  private def actorIndex(actor: Actor) = 80 * actor.piece.player.fold(1, 0) + actor.pos.hashCode

  def get(situation: Situation, table: ZobristConstants): Long = {
    val board = situation.board
    val hturn = situation.player.fold(table.p1TurnMask, 0L)

    val hactors = board.actors.values.view
      .map {
        table.actorMasks compose actorIndex _
      }
      .fold(hturn)(_ ^ _)

    hactors
  }

  def get(situation: SSituation, table: ZobristConstants): Long = {
    //    val board = situation.board
    //    val hturn = situation.player.fold(table.p1TurnMask, 0L)
    //
    //    val hactors = board.actors.values.view
    //      .map {
    //        table.actorMasks compose actorIndex _
    //      }
    //      .fold(hturn)(_ ^ _)
    //
    //    hactors
    0L //TODO
  }

  private val h = new Hash(size)

  def apply(situation: Situation): PositionHash = h.apply(situation)

  def apply(situation: SSituation): PositionHash = h.apply(situation)

  def debug(hashes: PositionHash) = hashes.map(_.toInt).sum.toString
}

// Although an Abalone board has 61 squares, our Pos index goes up to 81 so we need 2*81 masks.
private object ZobristTables {
  val actorMasks = Array(
    "9d39247e33776d4152b375aa7c0d7bac",
    "2af7398005aaa5c7208d169a534f2cf5",
    "44db0150246235478981513722b47f24",
    "9c15f73e62a76ae209b8f20f910a8ff7",
    "75834465489c0c890b8ea70255209cc0",
    "3290ac3a203001bfa688a9791f027500",
    "0fbbad1f6104227919b88b8ffaed8f55",
    "e83a908ff2fb60ca88bf7822d00d5526",
    "0d7e765d58755c10db7bf62ab390b71b",
    "1a083822ceafe02d33a6ac1d85c9f22f",
    "9605d5f0e25ec3b055ab2a27271d42ac",
    "d021ff5cd13a2ed540a21ff9c803fca4",
    "40bdf15d4a672e323c169aeb80a1d5d2",
    "011355146fd5639587684e27293ecf96",
    "5db4832046f3d9e5f8b91b39d4c6997c",
    "239f8b2d7ff719cc1d5f744f312fd467",
    "05d1a1ae85b49aa1eda18c452d5de5b4",
    "679f848f6e8fc9717497db888eccda0f",
    "7449bbff801fed0b94c1bb7016749887",
    "7d11cdb1c3b7adf035b23d663606fde2",
    "82c7709e781eb7cc17b4ae80b8184845",
    "f3218f1c9510786c8bd98922a3089d8e",
    "331478f3af51bbe6fec77fb07cea5e84",
    "4bb38de5e7219443e153a54d23c93a8a",
    "aa649c6ebcfd50fca196fa76c24405eb",
    "8dbd98a352afd40b6f333e11d079240a",
    "87d2074b81d7921723b8d480df5bb521",
    "19f3c751d3e92ae1634adaec002b3000",
    "b4ab30f062b19abf3d0e41d65872d549",
    "7b0500ac42047ac45aba83908462b892",
    "c9452ca81a09d85d26457864aff288af",
    "24aa6c514da2750043f10561015da64e",
    "4c9f34427501b447545cc6285df42807",
    "14a68fd73c910841a7140dc7b82e96ef",
    "a71b9b83461cbd93b1dcadc8fe30a8d4",
    "03488b95b0f1850f72ebd048ba373ac4",
    "637b2b34ff93c0402eb0ddf1351a1adb",
    "09d1bc9a3dd90a949cdc8c44a201836d",
    "3575668334a1dd3b0afc1fb45a728973",
    "735e2b97a4c45a2358c8fa415b96ec95",
    "18727070f1bd400b497a9b9a7f9f8872",
    "1fcbacd259bf02e7bff840799ee05fdf",
    "d310a7c2ce9b6555e4ec1554316c2704",
    "bf983fe0fe5d82443c9f0c8b89f31f3e",
    "9f74d14f7454a8244a601b99475baf4e",
    "51ebdc4ab9ba30356c65e1386536c3a9",
    "5c82c505db9ab0fab60a571d59e8a485",
    "fcf7fe8a3430b241e23c5d7045696d85",
    "3253a729b9ba3ddec9d4d61b569ec607",
    "8c74c368081b3075ce9ed71a6d18deb2",
    "b9bc6c87167c33e72dbc16559bdba870",
    "7ef48f2b83024e2050cda5339d836c83",
    "11d505d4c351bd7f98091ef4f2ab1ed3",
    "6568fca92c76a243f5803ac17fc45ecf",
    "4de0b0f40f32a7b809730ef15a78c687",
    "96d693460cc37e5df8bb209d715ab566",
    "42e240cb63689f2f0c5b201d6cb89a50",
    "6d2bdcdae291966152571fbfabb4a367",
    "42880b0236e4d9511b1db82269890861",
    "5f0f4a5898171bb69423f70ed512f1ea",
    "39f890f579f92f8879e448c72183e2a5",
    "93c5b5f47356388b3c88a0cf5b852900",
    "63dc359d8d231b789e12f819acaa6653",
    "ec16ca8aea98ad76c6f09266299a5902",
    "5355f900c2a82dc73e8cad2210fce3f3",
    "07fb9f855a997142a3868eff53346da1",
    "5093417aa8a7ed5e61de5496186b0d70",
    "7bcbc38da25a7f3ce17a09f0e53bc940",
    "19fc8a768cf4b6d4e0ffe83afe44ec11",
    "637a7780decfc0d9f35a5e3184c1c980",
    "8249a47aee0e41f783390d9b2e7563a6",
    "79ad695501e7d1e8950f14737ed6be5b",
    "14acbaf4777d57766df42fcfa743809d",
    "f145b6beccdea1950f2b1872ba3fef30",
    "dabf2ac8201752fc04171b94f58c5d2e",
    "24c3c94df9c8d3f678b05fea0dc77c38",
    "bb6e2924f03912eaa76ddf41aa675504",
    "0ce26c0b95c980d9e634bc8f87d0fe75",
    "a49cd132bfbf7cc42dbf77a8851237de",
    "e99d662af424393910f9b7d996836741",
    "27e6ad7891165c3f26f6547bb0471fb0",
    "8535f040b9744ff1f727c06db8bb34e0",
    "54b3f4fa5f40d87328984171f866b615",
    "72b12c32127fed2b349d245078c312ef",
    "ee954d3c7b411f4799f8f1ab94a13206",
    "9a85ac909a24eaa1967d7e5e99566e67",
    "70ac4cd9f04f21f5470fda103f9476cc",
    "f9b89d3e99a075c237dad4fcdedc6db8",
    "87b3e2b2b5c907b199f91b1cd65c50f0",
    "a366e5b8c54f48b86d89c29cb7034aef",
    "ae4a9346cc3f7cf2824c9daa114a11c7",
    "1920c04d47267bbdf3b1ee14505939c6",
    "87bf02c6b49e2ae92fa0d39cbee05ced",
    "092237ac237f3859c0c43ea8a642a49c",
    "ff07f64ef8ed14d04824a871bca34e17",
    "8de8dca9f03cc54e394b500f07f96989",
    "9c1633264db49c897c6efb4dc9bea9d9",
    "b3f22c3d0b0b38edca09213bfeb36c6a",
    "390e5fb44d01144b0069832f9f2bd0b5",
    "5bfea5b4712768e9f092a01d0d4420da",
    "1e1032911fa789846952e2015db39c5a",
    "9a74acb964e78cb3f3993e2a4cdf615e",
    "4f80f7a035dafb047d3ddc6bef2ed6f2",
    "6304d09a0b3738c4a7040db38e233bac",
    "2171e64683023a08a8b59fe4836ffc08",
    "5b9b63eb9ceff80cc55dbb54360414a9",
    "506aacf4898893423e24465359dc03c0",
    "1881afc9a3a701d6d27a416ed84cc3b7",
    "65030804407506447a77677e0de620c4",
    "dfd395339cdbf4a730cacd32e0313d3b",
    "ef927dbcf00c20f2dc6952fd3e61c11a",
    "7b32f7d1e03680ec08ab1642b5129e01",
    "b9fd7620e7316243aa8e0962b8eebcdc",
    "05a7e8a57db91b776dda36bacc3b2e2c",
    "b5889c6e15630a75ba82f4e2cb60b43e",
    "4a750a09ce9573f7509da4ba5295c4a5",
    "cf464cec899a2f8a36a18fa38c3d74c6",
    "f538639ce705b824b9e5652481a6df69",
    "3c79a0ff5580ef7f5f4946b7dd41d1c7",
    "ede6c87f8477609dfd7a4fb7bfe1d23d",
    "799e81f05bc93f3132e0b2b68ea83031",
    "86536b8cf3428a8cf25fcb24f0c19623",
    "97d7374c60087b73a317676dc1eb8797",
    "a246637cff328532f754b557c09ae146",
    "043fcae60cc0eba05bbd920fffe5fa7a",
    "920e449535dd359e5b3c978e296d2280",
    "70eb093b15b290ccca9c1ea34fc1484f",
    "73a1921916591cbd470c408e3b3d2dc5",
    "56436c9fe1a1aa8d6a0772093f97e152",
    "efac4b70633b8f81fa76d36719e7e5e3",
    "bb215798d45df7af2e799233a544062a",
    "45f20042f24f1768e003451144a03be8",
    "930f80f4e8eb7462974d8f4ee692ed35",
    "ff6712ffcfd75ea1d30afadfc4dc52f5",
    "ae623fd67468aa70278dde02bf30c1da",
    "dd2c5bc84bc8d8fc8b7e3a2bf5a061a3",
    "7eed120d54cf2dd9d1443752e511a579",
    "22fe545401165f1cd1b02672a0ec44cf",
    "c91800e98fb99929ba3005c8512514f1",
    "808bd68e6ac103652e3b86211f6b4295",
    "dec468145b7605f6057431bfeaa9d6f5",
    "1bede3a3aef53302a348ddd66378afaf",
    "43539603d6c556024a1817775b086ce1",
    "aa969b5c691ccb7a184b1c983a6a1a77",
    "a87832d392efee56cb70392e7b7db185",
    "65942c7b3c7e11ae3b1b1166a648330e",
    "ded2d633cad004f6e5201b155f51cc30",
    "21f08570f420e5654d053ee865f21b96",
    "b415938d7da94e3cd8d8062e343d9c66",
    "91b859e59ecb635013507b31a966da7d",
    "10cff333e0ed804ad637536d2e7a58b0",
    "28aed140be0bb7ddcbcae035eab824a0",
    "c5cc1d89724fa4568c77fe1e1b06691b",
    "5648f680f11a2741fd7841ed1ab4b961",
    "2d255069f0b7dab3caa88da017669d53",
    "9bc5a38ef729abd4abda5baf650b3675",
    "ef2f054308f6a2bcfe14d0c6b8f11e97",
    "af2042f5cc5c2858c13423f81c4b9adf",
    "480412bab7f5be2a34507a3503935243",
    "aef3af4a563dfe43ec504bd0c7ae79a1",
    "19afe59ae451497f0bc761ea4004d2ae",
    "52593803dff1e8403a0748078a78fd4d"
  )

  val p1TurnMask = "f8d626aaaf2785093815e537b6222c85"

}
