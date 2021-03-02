package co.s4n.talleres.inmutable

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ListSpec extends AnyFlatSpec with Matchers {

  "From the test list, the function tail" should "return tail List(1,2,4)" in {
    val test = List(9,1,2,4)
    List.tail(test) shouldEqual Const(1,Const(2,Const(4,Nil)))
  }

  "From a list 1,5,6,2 the function head" should "return head 1" in {
    val test = List(1,5,6,2)
    List.head(test) shouldEqual 1
  }

  "From the test list the function head" should "return head Hola" in {
    val test = List("Hola", "Soy", "David")
    List.head(test) shouldEqual "Hola"
  }

  "From the list true,false; the function and" should "return false" in {
    val test = List(true,false)
    List.and(test) shouldEqual false
  }

  "From the list true,true; the function and" should "return true" in {
    val test = List(true,true)
    List.and(test) shouldEqual true
  }

  "From the list false,true; the function or" should "return true" in {
    val test = List(false,true)
    List.or(test) shouldEqual true
  }

  "From the list false,false; the function or" should "return false" in {
    val test = List(false,false)
    List.or(test) shouldEqual false
  }

  "From the list 1,5,3,9,10,5; the function max" should "return 10" in {
    val test = List(1,5,3,9,10,5)
    List.max(test) shouldEqual 10
  }

  "From the list -1,-5,-3,-9,-10,-5; the function max" should "return -1" in {
    val test = List(-1,-5,-3,-9,-10,-5)
    List.max(test) shouldEqual -1
  }

  "From the list 1L,5L,3L,9L,10L,5L; the function min" should "return 1L" in {
    val test = List(1L,5L,3L,9L,10L,5L)
    List.min(test) shouldEqual 1L
  }

  "From the list -1L,-5L,-3L,-9L,-10L,-5L; the function min" should "return -10L" in {
    val test = List(-1L,-5L,-3L,-9L,-10L,-5L)
    List.min(test) shouldEqual -10L
  }

  "From the list -1.5,-5.45,-3.54,-9.52,-1.2,-9.33; the function minMax" should "return (1.2,9.52)" in {
    val test = List(1.5,5.45,3.54,9.52,1.2,9.33)
    List.minMax(test) shouldEqual (1.2,9.52)
  }

  "From the list -1.5,-5.45,-3.54,-9.52,-1.2,-9.33; the function minMax" should "return (-9.52,-1.2)" in {
    val test = List(-1.5,-5.45,-3.54,-9.52,-1.2,-9.33)
    List.minMax(test) shouldEqual (-9.52,-1.2)
  }

  "From the list a,b,c,d,e; the function take" should "return List(a,b,c)" in {
    val test = List("a","b","c","d","e")
    List.take(3,test) shouldEqual List("a","b","c")
  }

  "From the list 1,2,3,4; the function take" should "return Nil" in {
    val test = List(1,2,3,4)
    List.take(0,test) shouldEqual Nil
  }

  "From the list 1,2,3,4,5,6 the function init" should "return List(1,2,3,4,5)" in {
    val test = List(1,2,3,4,5,6)
    List.init(test) shouldEqual List(1,2,3,4,5)
  }

  "From the list test the function init" should "return Nil" in {
    val test = List(1)
    List.init(test) shouldEqual Nil
  }

  "From the list test the function split" should "return (List(1,2,3), List(4,5,6,7))" in {
    val test = List(1,2,3,4,5,6,7)
    List.split(3,test) shouldEqual (List(1,2,3), List(4,5,6,7))
  }

  "From the list test the function split" should "return (Nil, List(1,2,3,4,5,6,7))" in {
    val test = List(1,2,3,4,5,6,7)
    List.split(0,test) shouldEqual (Nil, List(1,2,3,4,5,6,7))
  }

  "From the two lists the function zip" should "return List((1,true),(2,false),(3,true))" in {
    val test1 = List(1,2,3)
    val test2 = List(true,false,true,true)
    List.zip(test1,test2) shouldEqual List((1,true),(2,false),(3,true))
  }

  "From the two lists the function zip" should "return List((1,false),(2,true),(3,false)" in {
    val test1 = List(1,2,3,4)
    val test2 = List(false,true,false)
    List.zip(test1,test2) shouldEqual List((1,false),(2,true),(3,false))
  }

  "From the list test the function unzip" should "return (List(1,2,3),List(\"a\",\"b\",\"c\"))" in {
    val test = List((1,"a"),(2,"b"),(3,"c"))
    List.unzip(test) shouldEqual (List(1,2,3),List("a","b","c"))
  }

  "From the list test the function reverse" should "return List(\"c\", \"b\", \"a\")" in {
    val test = List("a","b","c")
    List.reverse(test) shouldEqual List("c", "b", "a")
  }

  "From the list test the function intersperse" should "return List(2,1,3,1,4,1,5)" in {
    val test = List(2,3,4,5)
    List.intersperse(1,test) shouldEqual List(2,1,3,1,4,1,5)
  }

  //HIGH ORDER FUNCTIONS

  "From the list 2,3,4,5 the function dropWhileCurry" should "return List(4,5)" in {
    val test = List(2,3,4,5)
    List.dropWhileCurry(test)(_<4) shouldEqual List(4,5)
  }

  "From the list 2,3,4,5 the function sumR" should "return 14" in {
    val test = List(2,3,4,5)
    List.sumR(test) shouldEqual 14
  }

  "From the list 2,3,4,5 the function mulR" should "return 120" in {
    val test = List(2,3,4,5)
    List.mulR(test) shouldEqual 120
  }

  "From the list 2,3,4,5 the function sumF" should "return 14" in {
    val test = List(2,3,4,5)
    List.sumF(test) shouldEqual 14
  }

  "From the list 2,3,4,5 the function mulF" should "return 120" in {
    val test = List(2,3,4,5)
    List.mulF(test) shouldEqual 120
  }

  "From the list 2,3,4,5 the function sumL" should "return 14" in {
    val test = List(2,3,4,5)
    List.sumL(test) shouldEqual 14
  }

  "From the list 2,3,4,5 the function mulL" should "return 120" in {
    val test = List(2,3,4,5)
    List.mulL(test) shouldEqual 120
  }

  "From the list 2,3,4,5 the function sumarUno" should "return List(3,4,5,6)" in {
    val test = List(2,3,4,5)
    List.sumarUno(test) shouldEqual List(3,4,5,6)
  }

  "From the list 2,3,4,5 the function lengthF" should "return 5" in {
    val test = List(2,3,4,5)
    List.lengthF(test) shouldEqual 4
  }

  "From the list true,true,false the function andF" should "return false" in {
    val test = List(true,true,false)
    List.andF(test) shouldEqual false
  }

  "From the list true,true,true the function andF" should "return true" in {
    val test = List(true,true,true)
    List.andF(test) shouldEqual true
  }

  "From the list 2,3,4,5 the function filter" should "return List(2,4)" in {
    val test = List(2,3,4,5)
    List.filter(test)(_%2==0) shouldEqual List(2,4)
  }

  "From the list (1,false),(2,true),(3,false) the function unzipF" should "return List(1,2,3),List(false,true,false)" in {
    val test = List((1,false),(2,true),(3,false))
    List.unzipF(test) shouldEqual (List(1,2,3),List(false,true,false))
  }

  "From the list 2,3,4,5 the function lengthL" should "return 5" in {
    val test = List(2,3,4,5)
    List.lengthL(test) shouldEqual 4
  }

  "From the list true,true,true the function andL" should "return true" in {
    val test = List(true,true,true)
    List.andL(test) shouldEqual true
  }

  "From the list 2,3,4,5 the function takeWhileF" should "return List(2,3)" in {
    val test = List(2,3,4,5)
    List.takeWhileF(test)(_<4) shouldEqual List(2,3)
  }

  "From the list 2,3,4,5 the function filterL" should "return List(2,4)" in {
    val test = List(2,3,4,5)
    List.filterL(test)(_%2==0) shouldEqual List(2,4)
  }

  "From the list (1,false),(2,true),(3,false) the function unzipL" should "return List(1,2,3),List(false,true,false)" in {
    val test = List((1,false),(2,true),(3,false))
    List.unzipL(test) shouldEqual (List(1,2,3),List(false,true,false))
  }

  "From the list 2,3,4,5 the function sumarUnoMap" should "return List(3,4,5,6)" in {
    val test = List(2,3,4,5)
    List.sumarUnoMap(test) shouldEqual List(3,4,5,6)
  }

  "From the list 2,3,4,5 the function lstInt2StrMap" should "return List(\"2\",\"3\",\"4\",\"5\")" in {
    val test = List(2,3,4,5)
    List.lstInt2StrMap(test) shouldEqual List("2","3","4","5")
  }












}


