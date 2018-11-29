package utn.frba.tadp.dragonballZ

object DragonballZ {

  type resultadoAtaque = (Guerrero,Guerrero);
  class NotMovimientosFoundException extends Exception
  case class Guerrero(val items: List[Item],val estado: Estado,movimientos:List[Movimiento],val ki:Int){
    def realizarMovimientoContra(defensor: Guerrero,movimiento:Movimiento): (Guerrero,Guerrero) ={
      movimiento.realizarMovimiento(this,defensor)
    }

    def movimentoMasEfectivoContra(defensor:Guerrero, criterio:Guerrero=>Guerrero=>Int): Option[Movimiento] ={
      this.movimientos.map(movimiento => {(movimiento,criterio(this.realizarMovimientoContra(defensor,movimiento)))}).maxBy(tupla=>tupla._2)._1
    }

    def pelearRound(movimiento: Movimiento,defensor:Guerrero): (Guerrero,Guerrero) ={
      realizarMovimientoContra(defensor,movimiento)
      defensor.realizarMovimientoContra(this,defensor.movimentoMasEfectivoContra(this,???).getOrElse(throw new NotMovimientosFoundException()))
    }

  }
  class Item
  class Estado
  class Dormido extends Estado
  class Despierto extends Estado
  class SuperSayajin(val nivel:Int) extends Estado{

  }
  sealed trait  TipoGuerrero{

  }
  case class Humano(override val items: List[Item],override val estado: Estado,override val movimientos:List[Movimiento],override val ki:Int) extends Guerrero(items,estado,movimientos,ki)
  case class Saiyajins(override val items: List[Item],override val estado :Estado,override val movimientos:List[Movimiento],override val ki:Int,cola :Boolean) extends Guerrero(items,estado,movimientos,ki)
  case class Androides(override val items: List[Item],override val estado :Estado,override val movimientos:List[Movimiento]) extends Guerrero(items,estado,movimientos,0)
  case class Namekuseins(override val items: List[Item],override val estado :Estado,override val movimientos:List[Movimiento],override val ki:Int) extends Guerrero(items,estado,movimientos,ki)
  case class Monstruos(override val items: List[Item],override val estado :Estado,override val movimientos:List[Movimiento],override  val ki:Int) extends Guerrero(items,estado,movimientos,ki)



  trait Movimiento{
    def realizarMovimiento(atacante:Guerrero,defensor:Guerrero):(Guerrero,Guerrero)
  }
  case object dejarseFajar extends Movimiento {
    override def realizarMovimiento(atacante: Guerrero, defensor: Guerrero): (Guerrero,Guerrero) = {
      return (atacante.copy(),defensor.copy())
    }
  }
  case object cargarKi extends Movimiento {
    override def realizarMovimiento(atacante: Guerrero, defensor: Guerrero): (Guerrero,Guerrero) = atacante.estado match {
        case superSayajin: SuperSayajin => (atacante.copy(ki = atacante.ki+(150*superSayajin.nivel)),defensor.copy())
        case whoa  => (atacante.copy(ki=atacante.ki+100),defensor.copy())
    }
  }

  case class Arma() extends Item
  class Fuego extends Arma
  class Filosa extends Arma
  class SemillaErmitanio extends Item

  case class usarItem(val item:Item) extends Movimiento {
    override def realizarMovimiento(atacante: Guerrero, defensor: Guerrero): (Guerrero,Guerrero) = ???
  }
}
