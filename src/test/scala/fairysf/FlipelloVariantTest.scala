// package strategygames.fairysf

// import variant.Flipello
// import strategygames.fairysf.format.FEN

// class FlipelloVariantTest extends FairySFTest {

//   "Flipello" should {

//     "P2 win from position" in {
//       val position = FEN("8/8/8/3pP3/3Pp3/8/8/8[PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPpppppppppppppppppppppppppppppp] w 0 1")
//       val game     = fenToGame(position, Flipello)
//       game must beValid.like {
//         case game => {
//           game.situation.winner == None must beTrue
//         }
//       }
//     }

// }
// }
