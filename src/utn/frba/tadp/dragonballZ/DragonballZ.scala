package utn.frba.tadp.dragonballZ

object DragonballZ {

  type resultadoAtaque = (Guerrero,Guerrero);
  class NotMovimientosFoundException extends Exception
  class Guerrero(val items: List[Item],val estado: Estado,movimientos:List[Movimiento],ki: Int){
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
  sealed trait  TipoGuerrero{

  }
  case class Humano(override val items: List[Item],override val estado: Estado, val movimientos:List[Movimiento],ki:Int) extends Guerrero(items,estado,movimientos,ki)
  case class Saiyajins(override val items: List[Item],override val estado :Estado,val movimientos:List[Movimiento],ki:Int,cola :Boolean) extends Guerrero(items,estado,movimientos,ki)
  case class Androides(override val items: List[Item],override val estado :Estado,val movimientos:List[Movimiento],ki:Int) extends Guerrero(items,estado,movimientos,ki)
  case class Namekuseins(override val items: List[Item],override val estado :Estado,val movimientos:List[Movimiento],ki:Int) extends Guerrero(items,estado,movimientos,ki)
  case class Monstruos(override val items: List[Item],override val estado :Estado,val movimientos:List[Movimiento],ki:Int) extends Guerrero(items,estado,movimientos,ki)



  trait Movimiento{
    def realizarMovimiento(atacante:Guerrero,defensor:Guerrero):(Guerrero,Guerrero)
  }
  case object dejarseFajar extends Movimiento {
    override def realizarMovimiento(atacante: Guerrero, defensor: Guerrero): (Guerrero,Guerrero) = ???
  }
  case object cargarKi extends Movimiento {
    override def realizarMovimiento(atacante: Guerrero, defensor: Guerrero): (Guerrero,Guerrero) = ???
  }

  case class Arma() extends Item
  class Fuego extends Arma
  class Filosa extends Arma
  class SemillaErmitanio extends Item

  case class usarItem[A<:Item](val item:A) extends Movimiento {
    override def realizarMovimiento(atacante: Guerrero, defensor: Guerrero): (Guerrero,Guerrero) = ???
  }
}
