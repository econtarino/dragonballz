package utn.frba.tadp.dragonballZ
object guerrero {
  val estado ="Inconciente"
}
sealed case class Guerrero[A <: Guerrero[A]](val ki: Int, val movimientos: List[Movimiento], val items : List[Item], val estado: String = "bien", val isMonster:Boolean=false) {

  type Movimientos = List[Movimiento]

  def atacar(defensor:Guerrero[_],movimiento : Movimiento): (Guerrero[_],Guerrero[_]) ={
    movimiento match {
      case DejarseFajar => {
        val atacante = this.copy()
        val defensorAtacado = defensor.copy()
        (atacante, defensor)
      }
      case CargarKi => {
          val atacante = copy(ki= this.ki + 100)
          val defensorAtacado = defensor.copy()
          new Tuple2(atacante,defensorAtacado)
      }
      case UsarItem(item) => {
        val itemAUtilizar = items.find(itemInventario=>itemInventario.equals(item))
        if (itemAUtilizar.isEmpty)
          (this.copy(),defensor)
        else{
          item.utilizarItem(this,defensor)
        }
      }
      case ComerseAlOponente if (this.isMonster)=>{
        val defensorAtacado = defensor.copy(ki=0)
        val atacante = this.copy(movimientos=this.movimientos:::defensor.movimientos)
        (atacante,defensorAtacado)
      }
      case ConvertirseEnMono => this match {
        case Saiyajins(nombre, ki, tieneCola)=>
          val atacante = this.copy(ki=ki*3,movimientos=movimientos)
          (atacante,defensor)
      }
      case ConvertirseEnSuperSayayin => this match {
        case Saiyajins(nombre, ki, tieneCola)=>
          val atacante = this.copy(ki=ki*3,movimientos=movimientos)
          (atacante,defensor)
      }
      case Fusion(guerrero1,guerrero2) => (this.copy(ki=guerrero1.ki+guerrero2.ki),defensor)
      case Magia => ???
      case Ataques => ???
    }
  }
  def movimentoMasEfectivoContra(defensor:Guerrero[_], criterio:Guerrero[_] => Guerrero[_] => Int)  = {
      val movimiento = this.movimientos.map(movimiento => {
        val dosGuerreros = this.atacar(defensor,movimiento)
        (movimiento,criterio(dosGuerreros._1,dosGuerreros._2))
      }
    ).maxBy(tupla => tupla._2)._1
    new Some(movimiento)
  }

}
case class Humano(nombre:String) extends Guerrero[Humano](_,_,_) {
}

case class Saiyajins(nombre:String,override val ki:Int,val tieneCola:Boolean=false) extends Guerrero[Saiyajins](ki,_,_){
  def saltar: Unit = {

  }
}
case class Androides(nombre:String) extends Guerrero[Androides](_,_,_) {
}
case class Namekuseins(nombre:String) extends Guerrero[Namekuseins](_,_,_) {
}
