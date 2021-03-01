package co.s4n.talleres.inmutable

sealed trait Tree[+A]
case class Leaf[A](value:A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](tree:Tree[A]):Int = tree match{
    case Leaf(_) => 1
    case Branch(left,right) => 1+ size(right)+ size(left)

  }

  def depth[A](tree:Tree[A]):Int = tree match{
    case Leaf(_) => 1
    case Branch(left,right) => 1+ size(right) max size(left)

  }




}
