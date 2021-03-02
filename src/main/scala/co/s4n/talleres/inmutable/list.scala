package co.s4n.talleres.inmutable

import scala.Int.{MaxValue, MinValue}

import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Const[+A](h: A, t: List[A]) extends List[A]

object List {

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Const(as.head, apply(as.tail: _*))
  }

  /**
   * This function gets the List's size
   *
   * @param lst  , List of values, any kind
   * @return Int, List's length
   */
  def length[A](lst: List[A]): Int = lst match {
    case Nil => 0
    case Const(h, t) => 1 + length(t)
  }

  /**
   * This function sum all the elements from an Int List
   *
   * @param ints  , List of values, Int
   * @return Int, sum result
   */
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Const(h, t) => h + sum(t)
  }

  //Exercise1
  val x = List(4, 5, 6, 7, 8) match {
    case Const(x, Const(5, Const(7, _))) => x
    case Nil => 1
    case Const(x, Const(y, Const(6, Const(7, _)))) => x + y
    case Const(h, t) => h + sum(t)
    case _ => 777

  }

  /**
   * This function return the tail from a List
   *
   * @param lst  , List of values, any kind
   * @return List[A], List's tail
   */
  def tail[A](lst: List[A]) = lst match {
    case Const(_, t) => t
  }

  /**
   * This function return the first element from a List
   *
   * @param lst  , List of values, any kind
   * @return A, first element from the List
   */
  def head[A](lst: List[A]) = lst match {
    case Const(h, _) => h
  }

  /**
   * This function gets the result of the logical operation AND, from a List
   *
   * @param lst  , List of values, Boolean
   * @return Boolean, AND result. True if all values are true, false otherwise
   */
  def and(lst: List[Boolean]): Boolean = lst match {
    case Nil => true
    case Const(true, t) => and(t)
    case Const(false, _) => false
  }

  /**
   * This function gets the result of the logical operation OR, from a List
   *
   * @param lst  , List of values, Boolean
   * @return Boolean, OR result. False if all values are false, true otherwise
   */
  def or(lst: List[Boolean]): Boolean = lst match {
    case Nil => false
    case Const(false, t) => or(t)
    case Const(true, _) => true
  }

  /**
   * This function gets the maximum value from an Int List
   *
   * @param lst  , List of values, Int
   * @return Int, max value from the List
   */
  def max(lst: List[Int]): Int = {
    @tailrec
    def max2(lst: List[Int], maxNum: Int): Int = lst match {
      case Nil => maxNum
      case Const(h, t) => max2(t, if (h > maxNum) h else maxNum)
    }

    max2(lst, MinValue)
  }

  /**
   * This function gets the minimum value from a Long List
   *
   * @param lst  , List of values, Long
   * @return Long, min value from the List
   */
  def min(lst: List[Long]): Long = {
    @tailrec
    def min2(lst: List[Long], minNum: Long): Long = lst match {
      case Nil => minNum
      case Const(h, t) => min2(t, if (h < minNum) h else minNum)
    }

    min2(lst, MaxValue)
  }

  /**
   * This function return a tuple with the minimum and maximum value from a Double List
   *
   * @param lst  , List of values, Double
   * @return (Double,Double), Tuple with the min and max value
   */
  def minMax(lst: List[Double]): (Double, Double) = {
    @tailrec
    def minMax2(lst: List[Double], minNum: Double, maxNum: Double): (Double, Double) = lst match {
      case Nil => (minNum, maxNum)
      case Const(h, t) => (minMax2(t, if (h < minNum) h else minNum, if (h > maxNum) h else maxNum))
    }

    minMax2(lst, MaxValue, MinValue)
  }

  /**
   * Class M3 Functions
   */


  /**
   * This function add an element to the List's tail
   *
   * @param lst    , List of values, any kind
   * @param elem  , Element to be added, any kind
   * @return List[A], Updated List
   */
  def const[A](h: A, t: List[A]): List[A] = Const(h, t)

  /**
   * This function add an element to the List's tail
   *
   * @param lst    , List of values, any kind
   * @param elem  , Element to be added, any kind
   * @return List[A], Updated List
   */
  def addEnd[A](lst: List[A], elem: A): List[A] = lst match {
    case Nil => Const(elem, Nil)
    case Const(h, t) => Const(h, addEnd(t, elem))
  }

  /**
   * This function concat two Lists
   *
   * @param lst1  , List of values, any kind
   * @param lst2  , List of values, any kind
   * @return List[A], Concatenated List
   */
  def append[A](lst1: List[A], lst2: List[A]): List[A] = (lst1, lst2) match {
    case (Nil, Nil) => Nil
    case (l1, Nil) => l1
    case (Nil, l2) => l2
    case (Const(h, t), l2) => Const(h, append(t, l2))

  }

  /**
   * This function drop the first n values of a List
   *
   * @param n      :Int, Number of values
   * @param lst  , List of values, any kind
   * @return List[A], List
   */
  def drop[A](n: Int, lst: List[A]): List[A] = (n, lst) match {
    case (0, lst) => lst
    case (n, Nil) => Nil
    case (n, Const(h, t)) => drop(n - 1, t)
  }

  /**
   * Exercises Workshop M3
   */

  /**
   * This function take the first n values from the List (if they exist)
   *
   * @param n      :Int, Number of values
   * @param lst  , List of values, any kind
   * @return List[A], List of first n values
   */
  def take[A](n: Int, lst: List[A]): List[A] = {
    @tailrec
    def takeAux(n: Int, lst: List[A], acum: List[A]): List[A] = (n, lst) match {
      case (0, lst) => acum
      case (n, Nil) => acum
      case (n, Const(h, t)) => takeAux(n - 1, t, addEnd(acum, h))
    }

    takeAux(n, lst, Nil)
  }

  /**
   * This function take the first values except the last one
   *
   * @param lst  , List of values, any kind. Cant be Nil
   * @return List[A], List of first values except the last one
   */
  def init[A](lst: List[A]): List[A] = {
    @tailrec
    def initAux(lst: List[A], acum: List[A]): List[A] = lst match {
      case Const(h, Nil) => acum
      case Const(h, t) => initAux(t, addEnd(acum, h))
    }

    initAux(lst, Nil)

  }

  /**
   * This function divide a list in two. The first list with n elements, the rest go to the second one
   *
   * @param n      :Int, Number of elements to split
   * @param lst  , List of values, any kind
   * @return (List[A],List[A]), Tuple with the two divided Lists
   */
  def split[A](n: Int, lst: List[A]): (List[A], List[A]) = {
    @tailrec
    def splitAux(n: Int, lst: List[A], acum: List[A]): (List[A], List[A]) = (n, lst) match {
      case (0, lst) => (acum, lst)
      case (n, Nil) => (acum, Nil)
      case (n, Const(h, t)) => splitAux(n - 1, t, addEnd(acum, h))
    }

    splitAux(n, lst, Nil)
  }

  /**
   * This function fuse two different types Lists, in a Tuple-pair List
   *
   * @param lst1  , List of values, any kind
   * @param lst2  , List of values, any kind
   * @return List[(A,B)], List of tuples
   */
  def zip[A, B](lst1: List[A], lst2: List[B]): List[(A, B)] = {
    @tailrec
    def zipAux(lst1: List[A], lst2: List[B], acum: List[(A, B)]): List[(A, B)] = (lst1, lst2) match {
      case (Nil, Nil) => acum
      case (lst1, Nil) => acum
      case (Nil, lst2) => acum
      case (Const(h1, t1), Const(h2, t2)) => zipAux(t1, t2, addEnd(acum, (h1, h2)))
    }

    zipAux(lst1, lst2, Nil)
  }

  /**
   * This function separate a List of tuples in two different Lists
   *
   * @param lst  , List of tuples, any kind
   * @return (List[A],List[B]), Tuple with the two Lists
   */
  def unzip[A, B](lst: List[(A, B)]): (List[A], List[B]) = {
    @tailrec
    def unzipAux(lst: List[(A, B)], acum: (List[A], List[B])): (List[A], List[B]) = lst match {
      case Nil => acum
      case Const(h, t) => unzipAux(t, (addEnd(acum._1, h._1), addEnd(acum._2, h._2)))
    }

    unzipAux(lst, (Nil, Nil))
  }

  /**
   * This function return the last element from a List
   *
   * @param lst  , List of values, any kind
   * @return A, Last element
   */
  def lastElement[A](lst: List[A]): A = lst match {
    case Const(h, Nil) => h
    case Const(h, t) => lastElement(t)
  }

  /**
   * This function take a List and return an inverted List
   *
   * @param lst  , List of values, any kind
   * @return List[A], Inverted List
   */
  def reverse[A](lst: List[A]): List[A] = {
    @tailrec
    def reverseAux(lst: List[A], acum: List[A]): List[A] = lst match {
      case Nil => acum
      case Const(h, t) => reverseAux(init(lst), addEnd(acum, if (t != Nil) lastElement(t) else h))
    }

    reverseAux(lst, Nil)
  }

  /**
   * This function intermix a value between the original elements from the List
   *
   * @param elem  , Element to be intermix, any kind
   * @param lst    , List of values, any kind
   * @return List[A], Intermixed List
   */
  def intersperse[A](elem: A, lst: List[A]): List[A] = {
    @tailrec
    def intersperseAux(elem: A, lst: List[A], acum: List[A]): List[A] = lst match {
      case Nil => acum
      case Const(h, t) => intersperseAux(elem, t, if (t != Nil) addEnd(addEnd(acum, h), elem) else addEnd(acum, h))
    }

    intersperseAux(elem, lst, Nil)
  }

  /**
   * This function take a List of Lists and return a List with the Lists values
   *
   * @param lst  , List of Lists, any kind
   * @return List[A], List with the Lists values
   */
  def concat[A](lst: List[List[A]]): List[A] = {
    @tailrec
    def concatAux(lst: List[List[A]], acum: List[A]): List[A] = lst match {
      case Nil => acum
      case Const(h, t) => concatAux(t, if (t != Nil) append(acum, append(h, head(t))) else append(acum, Nil))
    }

    concatAux(lst, Nil)
  }

  /**
   * Funciones de alto orden
   * Toda Funcion que reciba como parametro otra funcion, es una funcion de alto orden
   */

  def dropWhile[A](lst: List[A], f: A => Boolean): List[A] = lst match {
    case Const(h, t) if f(h) => dropWhile(t, f)
    case _ => lst
  }

  def dropWhileCurry[A](lst: List[A])(f: A => Boolean): List[A] = lst match {
    case Const(h, t) if f(h) => dropWhileCurry(t)(f)
    case _ => lst
  }

  def reduce(lst: List[Int], num: Int)(f: (Int, Int) => Int): Int = lst match {
    case Nil => num
    case Const(h, t) => f(h, reduce(t, num)(f))
  }

  def sumR(lst: List[Int]) = reduce(lst, 0)((x, y) => x + y)

  def mulR(lst: List[Int]) = reduce(lst, 1)((x, y) => x * y)

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Const(h, t) => f(h, foldRight(t, z)(f))
  }

  def sumF(lst: List[Int]) = foldRight(lst, 0)((x, y) => x + y)

  def mulF(lst: List[Int]) = foldRight(lst, 1)((x, y) => x * y)


  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Const(h, t) => foldLeft(t, f(z, h))(f)
  }

  def sumL(lst: List[Int]) = foldLeft(lst, 0)(_ + _)

  def mulL(lst: List[Int]) = foldLeft(lst, 1)(_ * _)

  def sumarUno(lst: List[Int]): List[Int] = foldRight(lst, Nil: List[Int])((elem, lst) => Const(elem + 1, lst))

  /**
   * Exercises High order functions
   */

  def lengthF[A](lst: List[A]) = foldRight(lst, 0)((x, y) => 1 + y)

  def andF(lst: List[Boolean]): Boolean = foldRight(lst, true)((x, y) => x && y) //Tambien se puede poner _&&_

  def takeWhile[A](lst: List[A])(p: A => Boolean): List[A] = lst match {
    case Const(h, t) if p(h) => Const(h, takeWhile(t)(p))
    case _ => Nil
  }

  def filter[A](lst: List[A])(p: A => Boolean): List[A] = foldRight(lst, Nil: List[A])((h, t) => if (p(h)) Const(h, t) else t)

  def unzipF[A, B](lst: List[(A, B)]): (List[A], List[B]) = foldRight(lst, (Nil, Nil): (List[A], List[B]))((h, t) => (Const(h._1, t._1), Const(h._2, t._2)))

  def lengthL[A](lst: List[A]): Int = foldLeft(lst, 0)((y, x) => 1 + y)

  def andL(lst: List[Boolean]): Boolean = foldLeft(lst, true)((y, x) => y && x)

  def takeWhileF[A](lst: List[A])(p: A => Boolean): List[A] = foldRight(lst, Nil: List[A])((h,t) => if (p(h)) Const(h, t) else Nil)

  def filterL[A](lst: List[A])(p: A => Boolean): List[A] = foldLeft(lst, Nil: List[A])((lst, elem) => if (p(elem)) addEnd(lst,elem) else lst)

  def unzipL[A, B](lst: List[(A, B)]): (List[A], List[B]) = foldLeft(lst, (Nil, Nil): (List[A], List[B]))((lst, elem) => (addEnd(lst._1,elem._1), addEnd(lst._2, elem._2)))

  def takeWhileL[A](lst: List[A])(p: A => Boolean): List[A] = foldLeft(lst, Nil: List[A])((lst,elem) => if (p(elem)) addEnd(lst, elem) else Nil)

  /**
   * Class M4 Terminando funciones de alto orden
   */

  /**
   * This function transforms a List of type Int to a List of Strings
   * @param lst
   * @return
   */
  def lstInt2Str(lst:List[Int]):List[String] = lst match {
    case Nil => Nil
    case Const(h, t) => Const(h.toString,lstInt2Str(t))
  }

  def mapGen[A,B](lst:List[A])(f:A=>B):List[B] = lst match { //PERMITE GENERALIZAR LA FUNCION
    case Nil => Nil
    case Const(h,t) => Const(f(h),mapGen(t)(f))
  }

  def map[A,B](lst:List[A])(f:A=>B):List[B] = foldRight(lst,Nil:List[B])((x,y) => Const(f(x),y))

  def sumarUnoMap(lst:List[Int]):List[Int] = mapGen(lst)(_+1)

  def lstInt2StrMap(lst:List[Int]):List[String] = mapGen(lst)(_.toString)

}