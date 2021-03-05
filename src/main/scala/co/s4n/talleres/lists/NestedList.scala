package co.s4n.talleres.lists

sealed trait NestedList[+A]
case class Elem[A](a:A) extends NestedList[A]
case class Const[A](lst :List[NestedList[A]]) extends NestedList[A]

object NestedList{

  def flatten[A](lst: List[NestedList[A]]):List[A] = ???

}