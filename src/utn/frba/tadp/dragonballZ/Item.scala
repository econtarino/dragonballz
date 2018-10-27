package utn.frba.tadp.dragonballZ

trait Item {
    def utilizarItem(atacante:Guerrero[_],defensor:Guerrero[_]):(Guerrero[_],Guerrero[_]) = utilizarItem(atacante,defensor)
}
abstract class Arma extends Item {
  def utilizarItem(atacante: Guerrero[_], defensor: Guerrero[_]) : (Guerrero[_],Guerrero[_])
}
class SemillaErmitanio extends Item {
  override def utilizarItem(atacante: Guerrero[_], defensor: Guerrero[_]):(Guerrero[_],Guerrero[_])

class Roma extends Arma{
  override def utilizarItem(defensor: Guerrero[_],atacante:Guerrero[_]): (Guerrero[_],Guerrero[_]) ={
    defensor match {
      case Androides(_) => (atacante,defensor.copy())
      case default => (atacante,defensor.copy(estado = guerrero.estado)
    }
  }
}
class Filosa extends Arma{
  def utilizarArma(defensor:Guerrero[_],atacante:Guerrero[_]):(Guerrero[_],Guerrero[_]) ={
    defensor match {
      case Saiyajins(_,ki,tieneCola) => {
        val nuevoSayayin = new Guerrero[Saiyajins](100,Nil,_,_,_)
      }
      case default => (atacante,defensor)
    }
  }
}
class Fuego(var municion:Int=0) extends Arma{

  def isMunicionNotEmpty:Boolean = municion>0

  override def utilizarItem(defensor:Guerrero[_],atacante:Guerrero[_]) : (Guerrero[_],Guerrero[_]) ={
    defensor match {
      case Humano(_) => (atacante,defensor.copy(ki=defensor.ki-20))
      case Namekuseins(_) if(guerrero.estado.equals(defensor.estado)) => {
        if (isMunicionNotEmpty){
          this.municion=municion-1
          (atacante,defensor.copy(ki = defensor.ki-10))
        }else{
          (atacante,defensor)
        }
      }
    }
  }
}

class FotoDeLuna extends Item {
  override def utilizarItem(atacante: Guerrero[_], defensor: Guerrero[_]): (Guerrero[_], Guerrero[_]) = ???
}
