package co.s4n.talleres.inmutable

import scala.annotation.tailrec

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

  def addNat(nat1:Nat , nat2:Nat): Nat = (nat1,nat2) match{
    case (Cero,Cero) => Cero
    case (Cero,x) => x
    case (Suc(n), x) => Suc(addNat(n,x))
  }

  def prodNat(nat1 : Nat, nat2 : Nat) : Nat  = {
    @tailrec
    def prodNatTail(nat1: Nat, nat2: Nat, accum: Nat): Nat = (nat1, nat2) match {
      case (Cero, x) => accum
      case (Suc(n), x) => prodNatTail(n, x, addNat(x, accum))
    }
    prodNatTail(nat1, nat2, Cero)
  }



}
