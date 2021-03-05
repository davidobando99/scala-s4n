package co.s4n.talleres.lists

import scala.collection.immutable.List

object Lista {

  def subs[A](lst: List[A]):List[List[A]] = lst.length match {
        case 1 => Nil::List(lst)
        case 2 => Nil::lst.combinations (1).toList ::: lst.combinations (lst.length).toList
        case _ => Nil::lst.combinations (1).toList ::: lst.permutations.toList.map (_.tail) ::: lst.combinations (lst.length).toList
  }

  def permute[A](xs:List[A]):List[List[A]] = xs match {
    case Nil => List(List())
    case head::tail => {
      val len = xs.length
      val tps = (0 to len-1).map(xs.splitAt(_)).toList.filter(tp => !tp._1.contains(tp._2.head))
      tps.map(tp => permute(tp._1:::tp._2.tail).map(tp._2.head :: _)).flatten
    }
  }

  def main(args:Array[String]) ={
      val x = permute(List(1,2,3))
      println(x)
  }



}
