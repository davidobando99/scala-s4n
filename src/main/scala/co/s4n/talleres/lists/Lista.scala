package co.s4n.talleres.lists
import scala.annotation.tailrec
import scala.collection.immutable.List

object Lista {

  def subs[A](lst: List[A]):List[List[A]] = lst.length match {
        case 1 => Nil::List(lst)
        case 2 => Nil::lst.combinations (1).toList ::: lst.combinations (lst.length).toList
        case _ => Nil::lst.combinations (1).toList ::: lst.permutations.toList.map (_.tail) ::: lst.combinations (lst.length).toList
  }

  def subs2[A](lst: List[A]):List[List[A]] = lst match {
    case Nil => List(Nil)
    case head :: tail => subs2(tail) ::: subs2(tail).map(head :: _)

  }

  def barajar[A](a:A,lst: List[A]):List[List[A]] = lst match {
    case Nil => List(List(a))
    case x :: xs => (a ::(x :: xs)):: (barajar(a,xs).map(x::_))
  }

  def perms[A](lst:List[A]):List[List[A]] = lst match {
    case Nil => List(Nil)
    case head :: tail => (perms(tail)).flatMap(barajar(head,_))
  }


  def permute[A](xs:List[A]):List[List[A]] = xs match {
    case Nil => List(List())
    case head::tail => {
      val len = xs.length
      val tps = (0 to len-1).map(xs.splitAt(_)).toList.filter(tp => !tp._1.contains(tp._2.head))
      tps.map(tp => permute(tp._1:::tp._2.tail).map(tp._2.head :: _)).flatten
    }
  }

  def predAtPos[A](lst:List[A])( preds:List[(Int, A => Boolean)]):List[List[Boolean]] = {
    @tailrec
    def predAtPosAux[A](lst:List[A],preds:List[(Int,A => Boolean)], acum: List[List[Boolean]]):List[List[Boolean]] = preds match {
      case Nil => acum
      case head :: tail if(head._1 <= lst.length-1) => predAtPosAux(lst,tail,if (head._2(lst(head._1))) acum ::: List(List(true)) else acum ::: List(List(false)))
      case head :: tail  => predAtPosAux(lst,tail, acum ::: List(Nil))
    }
    predAtPosAux(lst,preds,List(Nil)).tail
  }

  def main(args:Array[String]) ={
      val x = predAtPos(List("A","B","C"))(List((2,_.equals("A")),(0,_.equals("A")), (6,_.equals("B"))))
      println(x)
      val y = predAtPos(List(1,2,3,4,5,6))(List((4,_==5),(0,_%2 == 1), (5,_%2 == 1)))
      println(y)
      val z = predAtPos(List(1,2,3,4,5,6))(List((1,_<=2),(6,_%2 == 0),(0,_>0)))
      println(z)
  }



}
