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

  /**
   * This functions gets the last element of the List
   * @param lst
   * @tparam A
   * @return
   */
  def myLast[A](lst:List[A]):A = lst match {
    case head :: Nil => head
    case head :: tail => myLast(tail)
  }

  def myButLast[A](lst:List[A]):A = lst match {
    case head :: _ :: Nil => head
    case _ :: tail => myButLast(tail)
  }

  def my2Last[A](lst:List[A]):List[A] = lst match {
    case head :: n :: Nil => List(head,n)
    case _ :: tail => my2Last(tail)
  }

  def elementAt[A](lst:List[A], pos:Int):A = (lst,pos) match {
    case (head::_,1) => head
    case (_::tail,n) => elementAt(tail,n-1)
  }

  def myLength[A](lst:List[A]):Int = lst match {
    case _::Nil => 1
    case _::tail => 1+ myLength(tail)
  }
  def myLengthFoldR[A](lst:List[A]):Int = lst.foldRight(0)((_,tail)=>1+tail)

  def myLengthFoldL[A](lst:List[A]):Int = lst.foldLeft(0)((y,_)=>y+1)


  def myReverse[A](lst: List[A]): List[A] = {
    @tailrec
    def myReverseAux[A](lst: List[A], acum: List[A]): List[A] = lst match {
      case Nil => acum
      case head :: tail => myReverseAux(tail, head :: acum)
    }
    myReverseAux(lst, Nil)
  }

  def isPalindrome[A](lst: List[A]): Boolean= lst == myReverse(lst)


  def compress[A](lst: List[A]): List[A] = lst match {
    case head::Nil => List(head)
    case head::h2::tail if (head==h2) => compress(h2::tail)
    case head::tail => head::compress(tail)
  }

  //def encode[A](lst: List[A]): List[(Int,A)] = {
  //  def encode[A](lst: List[A], n:Int ,acu:List[(Int, A)]): List[(Int, A)] = lst match {
  //    case head :: Nil => acu
 //     case head :: h2 :: tail if (head == h2) => encode (h2 :: tail,1 +n,acu)
     // case head :: tail => (n,head) :: encode(tail,0,acu::n,head)

  //  }
  //  encode(lst,0,List[(1,_)])
  //}

  def duplicate[A](lst:List[A]):List[A] ={
    def duplicateAux[A](lst:List[A], accu:List[A]):List[A] = lst match{
      case head::Nil => accu:+head:+head
      case head::tail => duplicateAux(tail,if(accu==Nil) head::head::accu else accu :+ head:+head)
    }
    duplicateAux(lst,Nil)
  }

  def replicate[A](lst:List[A], n:Int):List[A] = ???

  def myLastFor[A](lst:List[A]):Option[A] ={
    def myLastForAux[A](lst:List[A]):List[A] = for {
      x <- lst
      if(x == lst(lst.length-1))
    } yield (x)
    myLastForAux(lst) match {
      case Nil => None
      case head::_ => Some(head)
    }
  }

  def myButLastFor[A](lst:List[A]):Option[A] ={
    def myButLastForAux[A](lst:List[A]):List[A] = for {
      x <- lst
      if(x == lst(lst.length-2))
    } yield (x)
    myButLastForAux(lst) match {
      case Nil => None
      case head::_ => Some(head)
    }
  }

  def elementAtFor[A](lst:List[A], n:Int):Option[A] ={
    def elementAtForAux[A](lst:List[A], n:Int):List[A] = for {
      x <- lst
      if(x == lst(n-1))
    } yield (x)
    elementAtForAux(lst,n) match {
      case Nil => None
      case head::_ => Some(head)
    }
  }

  def reverseForC[A](lst:List[A]):List[A] ={
    val a = for {
      x <- lst.length-1 to 0 by -1
    } yield lst(x)
    a.toList

  }

  def lengthFor[A](lst:List[A]):Int =(for {
    x <- lst } yield ((a:Int) => a + 1)).foldLeft(0)((e,f) => f(e))

  def lastFor[A](lst:List[A]):A =(for {
    x <- lst } yield ((a:A)=> x)).foldLeft(lst.head)((e,f) => f(e))

  def elementFor[A](lst:List[A],n:Int):Option[A] =(for {
    x <- lst } yield ((a:Int,b:Option[A])=> b match {
    case None => if (a==n) (a,Some(x)) else (a+1,None)
    case Some(l) => (a,Some(l))
  })).foldLeft((1,None:Option[A]))((e,f)=>f(e._1,e._2))._2

  def reverseFor[A](lst:List[A]) = (for {
    x <- lst
  } yield ((xs:List[A]) => xs :+ x)).foldRight(Nil:List[A])((f,e) => f(e))





  def main(args:Array[String]) ={
      val y = predAtPos(List(1,2,3,4,5,6))(List((4,_==5),(0,_%2 == 1), (5,_%2 == 1)))
      println(y)
      val z = predAtPos(List(1,2,3,4,5,6))(List((1,_<=2),(6,_%2 == 0),(0,_>0)))
      println(z)
      val d = duplicate(List(1,1,2,2,3,3,3))
      println(d)





  }



}
