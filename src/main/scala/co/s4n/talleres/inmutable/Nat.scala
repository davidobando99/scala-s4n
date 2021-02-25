package co.s4n.talleres.inmutable

sealed trait Nat
case object Cero extends Nat
case class Suc(nat: Nat) extends Nat

object Nat{

  //Exercise10
  def fromNatToInt(nat:Nat):Int = nat match{
    case Cero => 0
    case Suc(n) => 1 + fromNatToInt(n)
  }

  //Exercise11
  def fromIntToNat(n:Int):Nat = n match{
    case 0 => Cero
    case x => Suc(fromIntToNat(x-1))
  }

}
