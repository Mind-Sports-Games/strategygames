package strategygames.backgammon

import strategygames.Player

final class Hash(size: Int) {

  def apply(situation: Situation): PositionHash = {
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
    val p1TurnMask                 = hexToLong(ZobristTables.p1TurnMask)
    val actorMasks                 = ZobristTables.actorMasks.map(hexToLong)
    val pocketMasks                = ZobristTables.pocketMasks.map(hexToLong)
  }

  object ZobristConstants {}

  // The following masks are compatible with the Polyglot
  // opening book format.
  private val polyglotTable    = new ZobristConstants(0)
  private lazy val randomTable = new ZobristConstants(16)

  // for backgammon hashInt is just 1 for stone
  private def pieceIndex(piece: Piece, count: Int) =
    (piece.role.hashInt * count) * 2 + piece.player.fold(1, 0)

  private def actorIndex(actorCount: (Actor, Int)) =
    Pos.all.size * pieceIndex(actorCount._1.piece, actorCount._2) + actorCount._1.pos.hashCode

  def get(situation: Situation, table: ZobristConstants): Long = {

    def pocketMask(playershift: Int, count: Int) = {
      if (0 < count && count <= 15)
        Option(table.pocketMasks(count + playershift))
      else None
    }

    val board = situation.board
    val hturn = situation.player.fold(table.p1TurnMask, 0L)

    val hactors = board.actors.values.view
      .map {
        table.actorMasks compose actorIndex _
      }
      .fold(hturn)(_ ^ _)

    board.pocketData.fold(hactors) { data =>
      Player.all
        .flatMap { player =>
          val playershift = player.fold(14, -1)
          data.pockets(player).roles.groupBy(identity).flatMap { case (_, list) =>
            pocketMask(playershift, list.size)
          }
        }
        .fold(hactors)(_ ^ _)
    }

  }

  private val h = new Hash(size)

  def apply(situation: Situation): PositionHash = h.apply(situation)

  def debug(hashes: PositionHash) = hashes.map(_.toInt).sum.toString

}

private object ZobristTables {
  // to work out the size of this calculate what the max value actorIndex can produce
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
    "52593803dff1e8403a0748078a78fd4d",
    "f4f076e65f2ce6f0c5f36bde8caa93fe",
    "11379625747d5af35b2299dc44080278",
    "bce5d2248682c1153f99cbeb6ec653fa",
    "9da4243de836994f48ebcfc004b524ca",
    "066f70b33fe09017d2278829cd344d05",
    "4dc4de189b671a1c5fe637e58fc1c0f3",
    "51039ab7712457c30a4b136f25a65a32",
    "c07a3f80c31fb4b44119314b520d04d9",
    "b46ee9c5e64a6e7c5354a8b08947cc8e",
    "b3819a42abe61c876001d6a94517300b",
    "21a007933a522a2014597a074f133855",
    "2df16f761598aa4fdc9a6baf92ffde03",
    "763c4a1371b368fdc5cbc5270de986b0",
    "f793c46702e086a095c72d49bd7560be",
    "d7288e012aeb8d3112b437e4c286737a",
    "de336a2a4bc1c44baa7c6f89f1442c5d",
    "0bf692b38d079f231a3ebbf317bfc4d8",
    "2c604a7a177326b34ad3c9fa863a5aa3",
    "4850e73e03eb6064c7c94147de663b5b",
    "cfc447f1e53c8e1b840e7fe4b35d4a4b",
    "b05ca3f564268d998921109126e23341",
    "9ae182c8bc9474e8a18a4f12c127de17",
    "a4fc4bd4fc5558ca471973e4dc6efb4b",
    "e755178d58fc4e76c723867b98d07330",
    "69b97db1a4c03dfef5fcc6de350950d1",
    "f9b5b7c4acc67c96b90913e02bde576a",
    "fc6a82d64b8655fb5554c92f272b73c5",
    "9c684cb6c4d244173738a3f0fdf5d9c6",
    "8ec97d2917456ed03771f25e5e278ee3",
    "6703df9d2924e97ea9d58a812b10906e",
    "c547f57e42a7444e2814f2a19d8670eb",
    "78e37644e7cad29e5ced6d617e9d6b4d",
    "fe9a44e9362f05fa69be27bdc682e06a",
    "08bd35cc38336615945e3cd54c7a41f4",
    "9315e5eb3a129acefac825c29c4e52fc",
    "94061b871e04df7595c380633671f3c0",
    "df1d9f9d784ba010b1f0f11b309f849f",
    "3bba57b68871b59d36b7ac17862bc4ac",
    "d2b7adeeded1f73f8b89835b5e731ac5",
    "f7a255d83bc373f8122138b676fc6561",
    "d7f4f2448c0ceb81ce3107b858b368ea",
    "d95be88cd210ffa7aa14dd3733de4203",
    "336f52f8ff4728e7ec4ea6805a8dad1e",
    "a74049dac312ac71ce5cd5938049dcf0",
    "a2f61bb6e437fdb521156227c06a4b0b",
    "4f2a5cb07f6a35b3da13d541802c4d5e",
    "87d380bda5bf7859fbf03d5c9f783bb2",
    "16b9f7e06c453a21e512fa9fd5c68b8a",
    "7ba2484c8a0fd54e30bd781b22277dcd",
    "f3a678cad9a2e38c56625e22aef316ec",
    "39b0bf7dde437ba2ed75251a71024db6",
    "fcaf55c1bf8a4424b468dc45b39cde2f",
    "18fcf680573fa59468b33836bea9a0a0",
    "4c0563b89f495ac33187565f03cc0d85",
    "40e087931a00930d9bbc591ddc43447f",
    "8cffa9412eb642c1c53a29458191d2db",
    "68ca39053261169f6bc263803d691ec8",
    "7a1ee967d27579e204cca68628858bac",
    "9d1d60e5076f5b6fa20a13cffa4679d1",
    "3810e399b6f65ba285725a1e096e1abf",
    "32095b6d4ab5f9b1bc986393043f78d5",
    "35cab62109dd038a0fa47125507ccb12",
    "a90b24499fcfafb1c2c27b60c8b2ce36",
    "77a225a07cc2c6bd217520a809c97da6",
    "513e5e634c70e331552ad48c96617c16",
    "4361c0ca3f692f12758c0637401144ae",
    "d941aca44b20a45bf1ae50d591aeb10f",
    "528f7c8602c5807b0c127280b89240a3",
    "52ab92beb9613989a9a8cd5ddd7737b0",
    "9d1dfa2efc557f738506683f3e28c050",
    "722ff175f572c3488105b0573483941f",
    "1d1260a51107fe97d00bcf6974e8788c",
    "7a249a57ec0c9ba23311a2a4e61fc638",
    "04208fe9e8f7f2d65b31cba035ff4f50",
    "5a110c6058b920a09ef049141a01e743",
    "0cd9a497658a56983355b7a63e03cf20",
    "56fd23c8f9715a4c78bf716c2f94ffcf",
    "284c847b9d887aae232304d6a359676e",
    "04feabfbbdb619cbffeebdd04f15816e",
    "742e1e651c60ba83594fdc90c434a4fd",
    "9a9632e65904ad3cba5cc088b72c0942",
    "881b82a13b51b9e2036efdc30e389de2",
    "506e6744cd9749245038b23c9af174d2",
    "b0183db56ffc6a799b5e64f304474d48",
    "0ed9b915c66ed37e280a4b8c73c2e8d8",
    "5e11e86d5873d484fda076be88bcc507",
    "f678647e3519ac6eafc896ae852c60c2",
    "1b85d488d0f20cc5be903340939e63fd",
    "dab9fe6525d890217a97bd60aba4c349",
    "0d151d86adb73615f62e51f178597cf9",
    "a865a54edcc0f0198f9ab42711b663dc",
    "93c42566aef98ffbc8d003d119dcac63",
    "99e7afeabe000731ef2101c32adaed38",
    "48cbff086ddf285adde81906502ad1b0",
    "7f9b6af1ebf78baf149756bc21368632",
    "58627e1a149bba2125c80f323a516eaa",
    "2cd16e2abd791e333ea039f7ff28ae8e",
    "d363eff5f09779960caf481f40063dd8",
    "0ce2a38c344a6eedbce23e106b1eefd7",
    "1a804aadb9cfa74110853ea82a5ccb34",
    "907f30421d78c5dee7c76ac3dbbf8c8c",
    "501f65edb3034d071624c0ce1532313d",
    "37624ae5a48fa6e95f3895b25d7b4744",
    "957baf61700cff4efbe363cbb55a913e",
    "3a6c27934e31188a35850e8f63400ddd",
    "d49503536abca3453d300047b5ddde66",
    "088e049589c432e01c1c7ca8b3386353",
    "f943aee7febf21b8986ec52ac2c88cec",
    "6c3b8e3e336139d3c93b616a554d23c8",
    "364f6ffa464ee52e211d7b5759da7504",
    "d60f6dcedc314222f2663fc59b541585",
    "56963b0dca418fc0f57fefeadb21b029",
    "16f50edf91e513af30fd60d9ee260966",
    "ef1955914b609f933c29da000d5b9a08",
    "565601c0364e3228d0d6203fa69da0ba",
    "ecb53939887e81758167e4bd87c6f05e",
    "bac7a9a18531294b5c063405c62f8154",
    "b344c470397bba52b86fe57d53081fe6",
    "65d34954daf3cebdeb60ad080cd573fe",
    "b4b81b3fa97511e2bfbbb41602635b78",
    "b422061193d6f6a74e39d536b723213c",
    "071582401c38434d56d7e0468df15a47",
    "7a13f18bbedc4ff59e601537348ed732",
    "bc4097b116c524d2ee2e827d9faa74c1",
    "59b97885e2f2ea28e43302e0d517a316",
    "99170a5dc3115544f662e9ba781a1fae",
    "6f423357e7c6a9f9e83da2efce442856",
    "325928ee6e6f8794143ae097749a513a",
    "d0e4366228b03343a203386e6a86f7c7",
    "565c31f7de89ea2773905f8c5056ecee",
    "30f56114841194140da07ce44c0142e4",
    "d873db391292ed4f8b9e97003ef01d2e",
    "7bd94e1d8e17debcd8c666a665840842",
    "c7d9f16864a76e948bb1069bba169263",
    "947ae053ee56e63c6bdc866d7daa19dc",
    "c8c93882f9475f5fe1115bb3f8ad0cfe",
    "3a9bf55ba91f81ca0859ae34a51ed77c",
    "d9a11fbb3d9808e4f1d73663c53a0156",
    "0fd22063edc29fca669283df212c93db",
    "b3f256d8aca0b0b97489abc08bd4db15",
    "b03031a8b4516e84f9d2b26d0375aab0",
    "35dd37d5871448afde4856e7777e27d1",
    "e9f6082b05542e4e2caeaf61386fa1f2",
    "ebfafa33d7254b597c6d4b00383f052a",
    "9255abb50d532280b1943df6ea3687ff",
    "b9ab4ce57f2d34f37e4d1baca94da20d",
    "693501d62829755138d1a6b6448fdc40",
    "c62c58f97dd949bf8aed53051756d212",
    "cd454f8f19c5126a1805fa7482c60f4e",
    "bbe83f4ecc2bdecb24c9891c2f4db0b1",
    "dc842b7e2819e230bb703190b30eb664",
    "ba89142e007503b8b52f857f41e68fce",
    "a3bc941d0a5061cbeb5a7a714d4ec1a1",
    "e9f6760e32cd8021c41334a36d4211ea",
    "09c7e552bc76492fe60188ecb537010d",
    "852f54934da55cc9ff67dd932e5755cc",
    "8107fccf064fcf5614c92be552467cfb",
    "098954d51fff6580c94fb8eae42b3453",
    "23b70edb1955c4bf0bd007042735acc6",
    "c330de426430f69db9dd82debb9abd3d",
    "4715ed43e8a45c0a3312b675a5bc5dfb",
    "a8d7e4dab780a08dd2e9447e6c6d3509",
    "0572b974f03ce0bbd4d771a89c91beb6",
    "b57d2e985e1419c731d0724183248884",
    "e8d9ecbe2cf3d73fe7229acdb568bc00",
    "2fe4b17170e59750b4ef94e8251ecb66",
    "11317ba87905e790ac817cad3cfb00ed",
    "7fbf21ec8a1f45ec3fe869890b69552c",
    "1725cabfcb045b0091aec362daa37fcd",
    "964e915cd5e2b207fb25f88ca887ca77",
    "3e2b8bcbf016d66d053bff886db0847c",
    "be7444e39328a0ac991fd5641b666e80",
    "f85b2b4fbcde44b724fc37ad820ed73a",
    "49353fea39ba63b12b2bcac1fa28086e",
    "1dd01aafcd53486a4c1a3a2190e08f26",
    "1fca8a92fd719f858e718591cf07851e",
    "fc7c95d827357afab14fadbd3baa703f",
    "18a6a990c8b35ebd8f1eb8cc7eedd98e",
    "cccb7005c6b9c28d89ed662f01bcb2fd",
    "3bdbb92c43b17f26a263fa3b9a325a8f",
    "aa70b5b4f89695a2bae9f4e14d09637c",
    "e94c39a54a98307f6be076b52945a007",
    "b7a0b174cff6f36ed264830e7dc7f906",
    "d4dba84729af48adc26059b78ed6854f",
    "2e18bc1ad9704a68148dca9a9b0c8474",
    "2de0966daf2f8b1c9749c69073bafeb8",
    "b9c11d5b1e43a07ebba6f4662b0cfd3c",
    "64972d68dee33360620103f01e5b63f8",
    "94628d38d0c205844c7820f950a4c583",
    "dbc0d2b6ab90a559e1262fa8ff1d3269",
    "d2733c4335c6a72f8f5121c2873029ef",
    "7e75d99d94a70f4d4fb3edb54d507b36",
    "6ced1983376fa72bf8594c470632ebb6",
    "97fcaacbf030bc24b6e876e78ecf5164",
    "7b77497b32503b12afeb0a5d807150f5",
    "8547eddfb81ccb94f651bea4c88fcaae",
    "79999cdff70902cbbfbce123f03177da",
    "cffe1939438e9b24b6aa0fd22855e81c",
    "829626e3892d95d7a240adf54d70b24e",
    "92fae24291f2b3f1732ea6db834bf5a4",
    "63e22c147b9c3403b47231e07ae8b35f",
    "c678b6d860284a1c80554c039ab7af15",
    "5873888850659ae7db2e83297a30b541",
    "0981dcd296a8736dd58b2a396b5a1669",
    "9f65789a6509a4402151156aaffdf4b7",
    "9ff38fed72e9052f65479a629704845e",
    "e479ee5b9930578ca9bed81039cf1c6d",
    "e7f28ecd2d49eecdd1a3f98b97eea710",
    "56c074a581ea17feff78a5d72aa18fe3",
    "5544f7d774b14aef96d1a9830ba7ffd3",
    "7b3f0195fc6f290fe19f501dcdf116db",
    "12153635b2c0cf578e68f253a278535d",
    "7f5126dbba5e0ca7dc75c481b2cdc0fa",
    "7a76956c3eafb4137162fa408d9042fd",
    "3d5774a11d31ab399fadef0bce1a7da1",
    "8a1b083821f40cb45421bbff426d4e84",
    "7b4a38e32537df62e7944beeb699c0e7",
    "950113646d1d6e032ed8b2db03799071",
    "4da8979a0041e8a9fc4e188df10d5454",
    "3bc36e078f7515d79de7df26c1457c8f",
    "5d0a12f27ad310d18fd73517b47f22c8",
    "7f9d1a2e1ebe1327a3005e22b1bd3e53",
    "da3a361b1c5157b12bb2c23c899cbc9e",
    "dcdd7d20903d0c25bfb0766d26526dbf",
    "36833336d068f707166dbdc38309e26b",
    "ce68341f798933896796f7e11a4c3cba",
    "ab9090168dd05f34562c093e14a87ff2",
    "43954b3252dc25e53627e637e2413092",
    "b438c2b67f98e5e9240414702e63067a",
    "10dcd78e3851a492a6f63494f774323c",
    "dbc27ab5447822bfc19ea801fb344afb",
    "9b3cdb65f82ca3829d18e8c21a6c8c61",
    "b67b7896167b4c8487f9503f9fded941",
    "bfced1b0048eac503e5098f627c4ed6c",
    "a9119b60369ffebd6eb063443f58c2ae",
    "1fff7ac80904bf453427200616f65462",
    "ac12fb171817eee7e3b74fe55c76691f",
    "af08da9177dda93dd6316d5008f932ec",
    "1b0cab936e65c74435a0d4ca7416842f",
    "b559eb1d04e5e932de4683286c56072d",
    "c37b45b3f8d6f2ba158754657dc0f21d",
    "c3a9dc228caac9e96d808b472208eab2",
    "f3b8b6675a6507ffcb208f7c937f44e6",
    "9fc477de4ed681dadff1dc38532c3a2e",
    "67378d8eccef96cb64ee6958558a5d83",
    "6dd856d94d259236f103b408d1245a6d",
    "a319ce15b0b4db31a1f19c518c3e0b41",
    "073973751f12dd5e48f0c2ff42819bfd",
    "8a8e849eb32781a51472ba925a4c6123",
    "e1925c71285279f5e3b750660989609a",
    "74c04bf1790c0efe6dd760ab49ed7373",
    "4dda48153c94938a67926c593a78bcaa",
    "9d266d6a1cc0542c5978ff009a18007c",
    "7440fb816508c4fe5568e6bf328b448e",
    "13328503df48229f7cc90fbed1165f2b",
    "d6bf7baee43cac40156f28d728f97cba",
    "4838d65f6ef6748f2edf603ec74b4900",
    "1e152328f3318dead48f299033fa9c9a",
    "8f8419a348f296bf543751766a18d326",
    "72c8834a5957b51104f25bdd180f31cb",
    "d7a023a73260b45cc66a3569f903b0dc",
    "94ebc8abcfb56daece61b42c7eead35c",
    "9fc10d0f989993e0a705d68144caaf00",
    "de68a2355b93cae63371ede2968498fb",
    "a44cfe79ae538bbe1c0c9a220f8fbf8a",
    "9d1d84fcce3714256631fc26158faebd",
    "51d2b1ab2ddfb636ef0ec337ff2aef59",
    "2fd7e4b9e72cd38cc979ef8243e71d7a",
    "65ca5b96b7552210d6c1a70601c91112",
    "dd69a0d8ab3b546df3a1da1866141057",
    "604d51b25fbf70e2d3e2b4c698f2a99e",
    "73aa8a564fb7ac9e1b4f5d5760ac5121",
    "1a8c1e992b94114827d0b28f28e7d0ef",
    "aac40a2703d9bea0f3a2d309d1e72bc0",
    "764dbeae7fa4f3a61294c73b3f914dda",
    "1e99b96e70a9be8b56dab15dc8b3fc48",
    "2c5e9deb57ef47435b77d3202095d45c",
    "3a938fee32d29981e0d317e26a17ae47",
    "26e6db8ffdf5adfe5b1d671e17069897",
    "469356c504ec9f9d937dc438ef99030b",
    "c8763c5b08d1908cce09ea57087ebea9",
    "3f6c6af859d80055d0d8fac3d4cfa048",
    "7f7cc39420a3a54527a5886862d56b94",
    "9bfb227ebdf4c5ce591673661c00d80b",
    "89039d79d6fc5c5cd1cc44ae71a9791d",
    "8fe88b57305e2ab6df5a715ada209d36",
    "a09e8c8c35ab96de491171944e677dae",
    "fa7e393983325753d0cf92367e04163c",
    "d6b6d0ecc617c69967b6a5b051ce8d5c",
    "dfea21ea9e7557e388d41953044d621e",
    "b67c1fa481680af8b93bcd9dd369fe49",
    "ca1e3785a9e724e579999db57b235430",
    "1cfc8bed0d68163920bad52dd13a90d4",
    "d18d8549d140caea4cccc5ae16a29dd2",
    "4ed0fe7e9dc91335a0615c69803b77c7",
    "e4dbf0634473f5d22e2cb0938ba801a0",
    "1761f93a44d5aefe7ba91914cab50528",
    "53898e4c3910da552738873108dcba8a",
    "734de8181f6ec39a4502a97899131230",
    "2680b122baa28d9738108697800e8bb5",
    "298af231c85bafaba46ede145d66a90b",
    "7983eed3740847d5e99d73feedfd98b3",
    "66c1a2a1a60cd8893267a96bed604a38",
    "9e17e49642a3e4c1ad66f2c81cc3fc42",
    "edb454e7badc0805fa2de5ba2d8c3693",
    "50b704cab602c329d4ca1c86d116bcbd",
    "4cc317fb9cddd023525f7774ee1dde6b",
    "66b4835d9eafea22a8e346682e2883b9",
    "219b97e26ffc81bdae03d84b90df4cb2",
    "261e4e4c0a333a9dd12735c3d08e24d9",
    "1fe2cca76517db90b737b467cee71d3a",
    "d7504dfa8816edbb36970fe334b2a37e",
    "b9571fa04dc089c8d5c73db7872be26f",
    "1ddc0325259b27de8aeb0ed56a4177fa",
    "cf3f4688801eb9aa0199f29dbf7a1802",
    "f4f5d05c10cab2431caba957a1ff78f0",
    "38b6525c21a42b0e2abfd2ecbf62492e",
    "36f60e2ba4fa6800add370c3cd316a3e",
    "eb3593803173e0ce08a307d218ffbcbf",
    "9c4cd6257c5a36037bf02813994b261f",
    "af0c317d32adaa8a894290366274ef43",
    "258e5a80c7204c4bc0821c96582294b4",
    "8b889d624d44885dbd2ce07a63da7db1",
    "f4d14597e660f85503ba38df61dba3a6",
    "d4347f66ec8941c34bc8ce1e706bb08d",
    "e699ed85b0dfb40d7dca263ea9024a3c",
    "2472f6207c2d04844e876b140c9cda33",
    "c2a1e7b5b459aeb55bdabc35a2fa1b4e",
    "ab4f6451cc1d45ec383a46ece18c27c4",
    "63767572ae3d6174532ee826c31a9b81",
    "a59e0bd101731a2884f3d8faecfd7924",
    "116d0016cb948f09ba905cc371f066d9",
    "2cf9c8ca052f6e9f3da882318279b416",
    "0b090a7560a968e3b3566f6dccae32ed",
    "abeeddb2dde06ff16eb13ec5ca4cb179",
    "58efc10b06a2068dab38266303f672c0",
    "c6e57a78fbd986e0c5e23f8698084689",
    "2eab8ca63ce802d7502fe8a1ce0a1bc6",
    "14a195640116f3364664fe5154093ec2",
    "7c0828dd624ec3903e39d0fefb4ffaf8",
    "d74bbe77e6116ac74162ffb2b58aed88",
    "804456af10f5fb535e1e505c7c916883",
    "ebe9ea2adf4321c72185b1f34d275173",
    "03219a39ee587a30db898d0487271365",
    "49787fef17af9924ddb7e20f4f0b0a5f",
    "a1e9300cd8520548e39ecdbb80830b57",
    "5b45e522e4b1b4efd1e12d09eb3d6c76",
    "b49c3b3995091a36f33de05912951acf",
    "d4490ad526f144317ddca4e7cf6a2022",
    "12a8f216af9418c237dae73934d2b45c",
    "001f837cc7350524ddf7d6c08847b906",
    "1877b51e57a764d576a4f4c4bd5b4dc4",
    "a2853b80f17f58eede916ef3cda04b7a",
    "993e1de72d36d310f79a5f06f0548dcf",
    "b3598080ce64a6567980b63a972639bc",
    "252f59cf0d9f04bbc327c42efcd3dcb0",
    "d23c8e176d1136008ae99f5a13771265",
    "1bda0492e7e4586eaeb96c60887bf568",
    "21e0bd5026c619bf0262cf9cfb1be6f7",
    "3b097adaf088f94e96f28db3af9f8eaf",
    "8d14dedb30be846ebd74fc2e2cd130ed",
    "f95cffa23af5f6f4bfb40b3bf2130455",
    "3871700761b3f7430d27390428f909cc",
    "ca672b91e9e4fa16e64118d41047bff4",
    "64c8e531bff53b559f175874ee74dfc0",
    "241260ed4ad1e87d3c41278759da9b49",
    "106c09b972d2e822ce243a587f0b6bbd",
    "7fba195410e5ca3019790b6cab0ef9bc",
    "7884d9bc6cb569d81b7bb956b20e0ff1",
    "0647dfedcd894a29fcd55d2a3d92556c",
    "63573ff03e2247744f6a491571d2a627",
    "4fc8e9560f91b1232a4bd439b9085684",
    "1db956e450275779b8eb5b22d497aa9d",
    "b8d91274b9e9d4fbf6cb137e88679a76",
    "a2ebee47e2fbfce122bc990578e7dd2c",
    "d9f1f30ccd97fb0901fc46a5af41ba72",
    "efed53d75fd64e6b9ee540b9ce7e922a",
    "2e6d02c36017f67f5a823d7da29de33e",
    "a9aa4d20db084e9bdcb54247ed750238",
    "b64be8d8b25396c1bbe346044327e21a",
    "70cb6af7c2d5bcf03905a0daabfe04e3",
    "98f076a4f7a2322e1670290ab9118147",
    "bf84470805e69b5f68163cb777eab10d",
    "94c3251f06f90cf3ed240589e171a9a1",
    "3e003e616a6591e92281ad67174bb66b",
    "b925a6cd0421aff3579ed522cb20efa0",
    "61bdd1307c66e3009c319f6b6fd2554a",
    "bf8d5108e27e0d480dd4faaafa80e520",
    "240ab57a8b888b2074bbae7f87e860f9",
    "fc87614baf287e07dff318b4d732a759",
    "ef02cdd06ffdb432c371ee9d7d2af132",
    "a1082c0466df6c0a76cc4be2658dda80",
    "8215e577001332c8fd9e506d75af7df5",
    "d39bb9c3a48db6cfc5a0d6e02e703f74",
    "2738259634305c14631701e42e35f5cf",
    "61cf4f94c97df93d3055402095f743c0",
    "1b6baca2ae4e125b37e89e5deff34268",
    "758f450c88572e0bcc01cd9d386a49cb",
    "959f587d507a835904a328251bd5828f",
    "b063e962e045f54dc8dc0ff4362a8c8f",
    "60e8ed72c0dff5d1ee814f9b3bce7af1",
    "7b64978555326f9f57ae92f6b7f094ef",
    "fd080d236da814ba0dbc0ba7bb1c2445",
    "8c90fd9b083f4558ec57a417b3ae8ad1",
    "106f72fe81e2c59008d8ec9d3a3a3a85",
    "7976033a39f7d95218a2e3daa4c6bbe3",
    "a4ec0132764ca04b68f8161c35388cf9",
    "733ea705fae4fa77e6e7cc9733d7aa2f",
    "b4d8f77bc3e56167206c16a493a194f6",
    "9e21f4f903b33fd9221153dec7e37554",
    "9d765e419fb69f6dd110651aa3d83db7",
    "d30c088ba61ea5ef841c207674a2a59d",
    "5d94337fbfaf7f5bc09b4da25303cd0e",
    "1a4e4822eb4d7a59e48fec76c3d485e3",
    "6ffe73e81b637fb34c37f28895f6b9e0",
    "ddf957bc36d8b9ca97fe8d6e8425ea84",
    "64d0e29eea8838b3e13176251539f9db",
    "08dd9bdfd96b9f634e887953d43713f5",
    "087e79e5a57d1d1332dc49e0f8b96e5c",
    "e328e230e3e2b3fb1240f0315707df84",
    "1c2559e30f0946bee22f60c878f03163",
    "720bf5f26f4d2eaa733ede4131a1662d",
    "b0774d261cc609db5c82e1f58657d112",
    "443f64ec5a3711956b39b17300529a95",
    "4112cf68649a260e9a366e73434c37a3",
    "d813f2fab7f5c5ca3af04822ac63ca77",
    "660d3257380841ee6c20030dcbb3e153",
    "59ac2c7873f910a3812bc00a41d9de43",
    "e846963877671a179d698757f729c30b",
    "93b633abfa3469f8764407e6839c724e",
    "c0c0f5a60ef4cdcf347b8bcf7aa8a816",
    "caf21ecd4377b28ca15cbc1e73577012",
    "57277707199b81758387a4b08cc28a2e",
    "506c11b9d90e8b1dce827a97fe830269",
    "d83cc2687a19255ffc3677ad21589c7a",
    "4a29c6465a314cd1c902d33e0a464ec1",
    "ed2df21216235097fb39294699a1e2aa",
    "b5635c95ff7296e2d7bf168e258f8f01",
    "22af003ab672e8112e602229d2b37a22",
    "52e762596bf68235821bc37e2c1f8912",
    "9aeba33ac6ecc6b0c57a1ba0f260acc5",
    "944f6de09134dfb6ce78cb667346e38c",
    "6c47bec883a7de39bb39eae7a290fcb0",
    "6ad047c430a121041bbc42a45ca20618",
    "a5b1cfdba0ab4067ef37e286f590dcfb",
    "7c45d833aff07862d9708ed7ec641deb",
    "5092ef950a16da0b12c5e2f484724605",
    "9338e69c052b8e7bf3e85d7b8a77b033",
    "455a4b4cfe30e3f58ee17c0021438904",
    "6b02e63195ad0cf8462f7fba7c2868f2",
    "6b17b224bad6bf27fb7a14d6137fa5b1",
    "d1e0ccd25bb9c16999ceec99465fe08c",
    "de0c89a556b9ae70a4be0f0017879351",
    "50065e535a213cf648c9c74efaeff5a7",
    "9c1169fa2777b874682f784ba23dc02e",
    "78edefd694af1eed226e96a540113353",
    "6dc93d9526a50e68aa04b725850e2e32",
    "ee97f453f06791edbc3338f53303ff19",
    "32ab0edb696703d394d80fc8e2559d54",
    "3a6853c7e70757a7b470b50e6769b2e4",
    "31865ced6120f37d9d5caf79d12f737c",
    "67fef95d926078900335ce6790cce15a",
    "1f2b1d1f15f6dc9ca9d88c05289cbd6d",
    "b69e38a8965c6b65f3d1d29b153dbd5e",
    "aa9119ff184cccf44104764512849057",
    "f43c732873f24c131d6d02af20051f53",
    "fb4a3d794a9a80d2c8e487152b256129",
    "3550c2321fd6109caf60d64fbda8f2ce",
    "371f77e76bb8417ebf6d091a05417402",
    "6bfa9aae5ec05779da184d8ff54f3fc1",
    "cd04f3ff001a4778b83657ca6633e2ac",
    "e3273522064480ca75d3e0b8feedbdb7",
    "9f91508bffcfc14a26a58648fd56deab",
    "049a7f41061a9e605da2dbf10323e25c",
    "fcb6be43a9f2fe9b1e3ed1a390724f66",
    "08de8a1c7797da9bc76159619f3af003",
    "8f9887e6078735a1b4af22b5f5b26389",
    "b5b4071dbfc73a66466792d7a2e6c6fa",
    "230e343dfba08d3366feaacd911b81a2",
    "43ed7f5a0fae657d94c1e6508f5f1f47",
    "3a88a0fbbcb05c634206c6c80ef8fd9f",
    "21874b8b4d2dbc4f3c1dea6d0afcae52",
    "1bdea12e35f6a8c9e79cf8044680e415",
    "53c065c6c8e6352848676fcc85844eb5",
    "e34a1d250e7a8d6b62f20ecb301144ad",
    "d6b04d3b7651dd7e2778709cb5ff3fc7",
    "5e90277e7cb39e2dd87c68010650c250",
    "2c046f22062dc67dbe61c9fc8adc7260",
    "b10bb459132d0a26eaec3f6695236d35",
    "3fa9ddfb67e2f199d47e91679b5bbefc",
    "0e09b88e1914f7af195693617db7534c",
    "10e8b35af3eeab372725134b52fd2c81",
    "9eedeca8e272b93322770ffa079a1704",
    "d4c718bc4ae8ae5f012c2a69dda5ad22",
    "81536d601170fc2009338c04f427a66f",
    "91b534f885818a060d85fd7519aa9dec",
    "ec8177f83f9009780246a0f9fd642861",
    "190e714fada5156eac615f38f5a451ea",
    "b592bf39b036496307a44f6430336f1c",
    "89c350c893ae7dc12184303422b53201",
    "ac042e70f8b383f26e47c77585ab8164",
    "b49b52e587a1ee6055d75131c650586c",
    "fb152fe3ff26da89420514ac928637fa",
    "3e666e6f69ae2c15bb00290a9289ce13",
    "3b544ebe544c19f9321f8022ccc2553a",
    "e805a1e290cf24567effcb24d14c9d18",
    "24b33c9d7ed25117dc418c09511a5174",
    "e74733427b72f0c11130c8b2334d05c7",
    "0a804d18b709747557f10554d8e9323b",
    "57e3306d881edb4f90a5ce5a89ea0b56",
    "4ae7d6a36eb5dbcbc27327f936e68d1b",
    "2d8d5432157064c8417b730cb2a966b0",
    "d1e649de1e7f268b1d301ea15b8ea672",
    "8a328a1cedfe552cf14dee3399ddf91c",
    "07a3aec79624c7dabd989097807f7fbf",
    "84547ddc3e203c94c3a0533a6d96954b",
    "990a98fd5071d2631992575160c43696",
    "1a4ff12616eefc89d49493b523ad7777",
    "f6f7fd1431714200b77c0f9cf9e436f2",
    "30c05b1ba332f41ce62702fef78982ef",
    "8d2636b81555a786236a476ed9466eb7",
    "46c9feb55d120902e304cfd61b4f416c",
    "ccec0a73b49c99212cb2ea092e5f1215",
    "4e9d2827355fc492e2004d8b7660e169",
    "19ebb029435dcb0f7fcabe79c4442ade",
    "4659d2b743848a2c3ec3d0686df24ad5",
    "963ef2c96b33be318f3d9e86c7b31fff",
    "74f85198b05a2e7d6ba130e1edb0873d",
    "5a0f544dd2b1fb1840fe52eaaac04a87",
    "03727073c2e134b1789eb8c74c29b5bd",
    "c7f6aa2de59aea614630d70199d8fe85",
    "352787baa0d7c22fc4f6e06e6eadb36d",
    "9853eab63b5e0b352d9cd0536bddc355",
    "abbdcdd7ed5c08609522ac8d318de072",
    "cf05daf5ac8d77b0dac15872053fb2a8",
    "49cad48cebf4a71e4b63b05120422ba5",
    "7a4c10ec2158c4a6c4f797ac06e0c775",
    "d9e92aa246bf719e4b9efbbed8c2bd98",
    "13ae978d09fe555767b2d7640bd6d12a",
    "730499af921549ff69216ca21f6e1bf4",
    "4e4b705b92903ba449ab0589118fe345",
    "ff577222c14f0a3a90b672aaf0764986",
    "55b6344cf97aafaec24aa6db9b0e9300",
    "b862225b055b69608478cc06efce550e",
    "cac09afbddd2cdb482e3ec2ccd28350f",
    "daf8e9829fe96b5fbc059bb74b993690",
    "b5fdfc5d3132c498e31111d14445238b",
    "310cb380db6f7503a7ed1df77e79a736",
    "e87fbb46217a360ea137dc0582888c63",
    "2102ae466ebb11487630e70040281934",
    "f8549e1a3aa5e00d2c17c3b74634a6d6",
    "07a69afdcc42261a05eff03a838a4fb4",
    "c4c118bfe78feaae3c28d21d40d4f80e",
    "f9f4892ed96bd438beb9cec18b163f7c",
    "1af3dbe25d8f45dadc75195297484115",
    "f5b4b0b0d2deeeb4548955be3bde572b",
    "962aceefa82e1c84c8281bde4d280b81",
    "046e3ecaaf453ce9914da129a132922b",
    "f05d129681949a4cff11d08f1cee77a4",
    "964781ce734b3c848070295bd01f6bfd",
    "9c2ed44081ce5fbd006a9346f317a9c9",
    "522e23f3925e319e37e62ccdf9739fc3",
    "177e00f9fc32f7915cbf8b753a7e703c",
    "2bc60a63a6f3b3f2fe0f162fcebe01c8",
    "222bbfae61725606094f481a19d464ff",
    "486289ddcc3d67809737e370b3c679cf",
    "7dc7785b8efdfc8060d67575f3b9b1c2",
    "8af38731c02ba9803948fbaf41196093",
    "1fab64ea29a2ddf70c204d1cfdbf6e2a",
    "e4d9429322cd065ab7d2d42a9be29c2a",
    "9da058c67844f20c49fae729fc2974b3",
    "24c0e332b70019b06fb26356dad98ed6",
    "233003b5a6cfe6ad4847b6cc14eeffd4",
    "d586bd01c5c217f63b5285a9f0152c99",
    "5e5637885f29bc2bff1fe7f4b91cff4c",
    "7eba726d8c94094b16e20774363e99d0",
    "0a56a5f0bfe39272437d1aa9cb4159e0",
    "d79476a84ee20d067bdcc9d5c1c4da0b",
    "9e4c1269baa4bf378fd087733782eecd",
    "17efee45b0dee640bf94dee3ad478cda",
    "1d95b0a5fcf90bc683c94bbe4c623bf5",
    "93cbe0b699c2585d08fda03aa45ee9ba",
    "65fa4f227a2b6d79c188f92d4d403856",
    "d5f9e858292504d5fd6611dfb12345fc",
    "c2b5a03f71471a6fef003ffd18aecc12",
    "59300222b4561e00b7b474cbf2934019",
    "ce2f8642ca0712dcbc2a55e58b30deee",
    "7ca9723fbb2e89883e77de18337cda42",
    "2785338347f2ba082f81e058ffb75885",
    "c61bb3a141e50e8c8f0c300cf585707e",
    "150f361dab9dec26cf4f4c536e0e2af2",
    "9f6a419d382595f424c81ee5fd39a8e4",
    "64a53dc924fe7ac92841577e66ad726e",
    "142de49fff7a7c3d68090c81a1357214",
    "0c335248857fa9e7a6aa70d44a613a24",
    "0a9c32d5eae45305db805d26087f4db9",
    "e6c42178c4bbb92e6dd954b45a122182",
    "71f1ce2490d20b07c34729fd9f1948a3",
    "f1bcc3d275afe51af0682ca0764cc153",
    "e728e8c83c334074bf29824279ca73e1",
    "96fbf83a12884624f0c70fb4e725caff",
    "81a1549fd6573da51e18cad809e9eedc",
    "5fa7867caf35e149a893a8fa258f383e",
    "56986e2ef3ed091be78c30cc1179b849",
    "917f1dd5f8886c612e4432e6ce4996d9",
    "d20d8c88c8ffe65f576ec7a84e0b932d"
  )

  val p1TurnMask = "f8d626aaaf2785093815e537b6222c85"

  val pocketMasks = Array(
    "b262e9f9d61233206e21a47d5b561a1d",
    "91533947cdaa8bec4263a757e414fe44",
    "a13b56b45723a3d493c43f67cf55b53f",
    "9a35cce29ca3ac75a89732f339d35eec",
    "2716940e1d4f28d75df4ac25c29fbebf",
    "7447209cfb79306662059230cedcd78f",
    "5cf91d8ae6402e1a9b0f7261932d2c8e",
    "4625588d38487ac5fc0f99f00e0cc0e7",
    "e42ec6191353e3bdcba8a5a02c351aa5",
    "478e6cc8f6b2dada77a6c01bd0174d69",
    "1726fc948b994b87c200e5264100e463",
    "fb9d2e5a66b467413f340a89f525effe",
    "7f668e401ffe9e6ff748b2be597b2f7a",
    "ee4d6fe11c46a236f7b2eddf7b8838c2",
    "006cb70064259959bdec7e9f4317a678",
    "33535a7c4def1b245e022239fdf20b36",
    "479e792f8171fc29fdd92fa05b03b629",
    "656a6e71de97097580cdab95a89927dc",
    "cada3e48618a1c2b92cb516b8eba4a30",
    "b37ad7262db9c99eac950f8bce3af2d9",
    "85ae25402a311d5d9a43060d3aaae01a",
    "3de4e82d52dbb44cf12b2f6012f9333c",
    "b1c8499674464c218ae9143ece61584a",
    "f1c1853cc6827b84676f70930e72993c",
    "51f97ed3ba004fb025e0f5f014476a1f",
    "00da9ede878e3e981e33827f042de978",
    "3cd0fd658e1cdb12dd158e4a7838524d",
    "ac2940b688a1d0f92b75316dfa1b15e2",
    "e51acb5b336db0df0283b57f325ea495",
    "cf7517fbdcb16174146e60f56ab91765"
  )

}
