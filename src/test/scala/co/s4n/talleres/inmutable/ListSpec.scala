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






}


