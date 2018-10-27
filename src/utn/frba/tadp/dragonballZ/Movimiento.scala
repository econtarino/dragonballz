package utn.frba.tadp.dragonballZ

sealed trait Movimiento
case object DejarseFajar extends Movimiento
case object CargarKi extends Movimiento
case class  UsarItem(item:Item) extends Movimiento
case object ComerseAlOponente extends Movimiento
case object ConvertirseEnMono extends Movimiento
case object ConvertirseEnSuperSayayin extends Movimiento
case class Fusion(guerrero1: Guerrero[_],guerrero2: Guerrero[_]) extends Movimiento
case object Magia extends Movimiento
case object Ataques extends Movimiento