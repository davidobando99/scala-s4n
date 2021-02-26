package co.s4n.talleres.inmutable

import scala.Int.{MaxValue, MinValue}

import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Const[+A](h: A, t: List[A]) extends List[A]

object List {

  def apply[A](as: A*) : List[A] = {
    if (as.isEmpty) Nil
    else Const(as.head, apply(as.tail: _*))
  }
  def length[A](lst:List[A]):Int = lst match {
    case Nil => 0
    case Const(h,t) => 1 + length(t)
  }
  def sum(ints:List[Int]):Int = ints match {
    case Nil => 0
    case Const(h, t) => h + sum(t)
  }

  //Exercise1
  val x = List(4,5,6,7,8) match {
    case Const(x, Const(5, Const(7, _))) => x
    case Nil => 1
    case Const(x, Const(y, Const(6, Const(7, _)))) => x + y
    case Const(h, t) => h + sum(t)
    case _ => 777

  }

  //Exercise2
  def tail[A](lst:List[A]) = lst match {
    case Const(_, t) => t
  }

  //Exercise3
  def head[A](lst:List[A]) = lst match {
    case Const(h, _) => h
  }

  //Exercise4
  def and(lst:List[Boolean]):Boolean = lst match {
    case Nil => true
    case Const(true,t) => and(t)
    case Const(false,_) => false
  }

  //Exercise5
  def or(lst:List[Boolean]):Boolean = lst match {
    case Nil => false
    case Const(false,t) => or(t)
    case Const(true,_) => true
  }


  //Exercise6
  def max(lst:List[Int]):Int = {
    @tailrec
    def max2(lst:List[Int], maxNum:Int):Int = lst match {
      case Nil => maxNum
      case Const(h,t) => max2(t, if (h > maxNum) h else maxNum)
    }
    max2(lst,MinValue)
  }

  //Exercise7
  def min(lst:List[Long]):Long = {
    @tailrec
    def min2(lst:List[Long], minNum:Long):Long = lst match {
      case Nil => minNum
      case Const(h,t) => min2(t, if (h < minNum) h else minNum)
    }
    min2(lst,MaxValue)
  }

  //Exercise8
  def minMax(lst:List[Double]):(Double,Double) = {
    @tailrec
    def minMax2(lst:List[Double], minNum:Double, maxNum:Double):(Double,Double) = lst match {
      case Nil => (minNum,maxNum)
      case Const(h,t) => (minMax2(t, if (h < minNum) h else minNum, if (h > maxNum) h else maxNum))
    }
    minMax2(lst,MaxValue,MinValue)
  }

  //ClaseM3
  def const[A](h:A,t:List[A]):List[A] = Const(h,t)

  def addEnd[A](lst:List[A], elem:A):List[A] = lst match{
    case Nil => Const(elem,Nil)
    case Const(h,t) => Const(h,addEnd( t,elem))
  }

  def append[A](lst1:List[A], lst2:List[A]):List[A] =  (lst1,lst2) match {
    case (Nil, Nil) => Nil
    case (l1, Nil) => l1
    case (Nil, l2) => l2
    case (Const(h, t), l2) => Const(h, append(t, l2))

  }

  def drop[A](n:Int,lst:List[A]):List[A] =n match {
    case 0 => lst
    case n => drop(n-1,tail(lst))
  }

  def split[A](n:Int,lst:List[A]):(List[A],List[A]) = {
    @tailrec
    def split2(n:Int,lst:List[A], acum:List[A]):(List[A],List[A]) = (n,lst) match {
      case (0,lst) => (acum,lst)
      case (n,Nil) => (Nil,Nil)
      case (n,Const(h,t)) => split2(n-1,t,append(acum,Const(h,Nil)))
    }
    split2(n,lst,Nil)
  }






}