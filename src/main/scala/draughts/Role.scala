package strategygames.draughts

sealed trait Role {
  val forsyth: Char
  lazy val pdn: Char    = forsyth
  lazy val name: String = toString.toLowerCase
  val binaryInt: Int
  val hashInt: Int
}

sealed trait PromotableRole extends Role

case object King extends PromotableRole {
  val forsyth = 'K'
  val binaryInt = 1
  val hashInt = 2
}

case object Man extends Role {
  val forsyth = ' '
  val binaryInt = 2
  val hashInt = 1
}

case object GhostMan extends Role {
  val forsyth = 'G'
  val binaryInt = 4
  val hashInt = 3
}

case object GhostKing extends Role {
  val forsyth = 'P'
  val binaryInt = 3
  val hashInt = 2
}

object Role {

  val all: List[Role]                     = List(King, Man)
  val allPromotable: List[PromotableRole] = List(King)

  val allByForsyth: Map[Char, Role]                     = all.map(r => (r.forsyth, r)).toMap
  val allByPdn: Map[Char, Role]                         = all.map(r => (r.pdn, r)).toMap
  val allByName: Map[String, Role]                      = all.map(r => (r.name, r)) toMap
  val allByBinaryInt: Map[Int, Role]                    = all.map(r => (r.binaryInt, r)) toMap
  val allByHashInt: Map[Int, Role]                      = all.map(r => (r.hashInt, r)) toMap
  val allPromotableByName: Map[String, PromotableRole]  = allPromotable.map(r => (r.toString, r)) toMap
  val allPromotableByForsyth: Map[Char, PromotableRole] = allPromotable.map(r => (r.forsyth, r)) toMap
  val allPromotableByPdn: Map[Char, PromotableRole]     = allPromotable.map(r => (r.pdn, r)) toMap

  def forsyth(c: Char): Option[Role] = allByForsyth get c

  def binaryInt(i: Int): Option[Role] = allByBinaryInt get i

  def hashInt(i: Int): Option[Role] = allByHashInt get i

  def promotable(c: Char): Option[PromotableRole] =
    allPromotableByForsyth get c

  def promotable(name: String): Option[PromotableRole] =
    allPromotableByName get name.capitalize

  def promotable(name: Option[String]): Option[PromotableRole] =
    name flatMap promotable

  def pdnMoveToRole(c: Char): Role =
    //We dont want ghosts to be returned here
    c match {
      case 'K' | 'O' => King
      case _ => Man
    }

  def javaSymbolToRole(s: String): Role =
    s match {
      case "" => Man
      case _ => King
    }

  def valueOf(r: Role): Option[Int] = r match {
    case Man  => Some(1)
    case King => Some(2)
    case _    => Some(0)
  }

}
