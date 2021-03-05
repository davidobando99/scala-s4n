package co.s4n.talleres.lists

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ListaSpec extends AnyFlatSpec with Matchers {

  "From the list 1,2 the function subs" should "return List(List(),List(1),List(2),List(1,2))" in {
    val test = List(1,2)
    Lista.subs(test) shouldEqual List(List(),List(1),List(2),List(1,2))
  }

  "From the list 2,3,4 the function permute" should "return List(List(2,3,4),List(3,2,4),List(3,4,2),List(2,4,3),List(4,2,3),List(4,3,2))" in {
    val test = List(2,3,4)
    Lista.permute(test) shouldEqual List(List(2,3,4),List(3,2,4),List(3,4,2),List(2,4,3), List(4,2,3),List(4,3,2))
  }

  "From the list 2,3,4 the function perms" should "return List(List(2,3,4),List(3,2,4),List(3,4,2),List(2,4,3),List(4,2,3),List(4,3,2))" in {
    val test = List(2,3,4)
    Lista.perms(test) shouldEqual List(List(2,3,4),List(3,2,4),List(3,4,2),List(2,4,3), List(4,2,3),List(4,3,2))
  }

  "From the list 1,2,3,4,5,6, applying functions list, the function predAtPos" should "return List(List(true),List(true),List(false))" in {
    val test = List(1,2,3,4,5,6)
    Lista.predAtPos(test)(List((4,_==5),(0,_%2 == 1), (5,_%2 == 1))) shouldEqual List(List(true),List(true),List(false))
  }

  "From the list 1,2,3,4,5,6, applying functions list, the function predAtPos" should "return List(List(true),List(),List(true))" in {
    val test = List(1,2,3,4,5,6)
    Lista.predAtPos(test)(List((1,_<=2),(6,_%2 == 0),(0,_>0))) shouldEqual List(List(true),List(),List(true))
  }

  "From the list 1,2,3,4,5,6, the function myLast" should "return 6" in {
    val test = List(1,2,3,4,5,6)
    Lista.myLast(test) shouldEqual 6
  }

  "From the list 1,2,3,4,5,6, the function myButLast" should "return 5" in {
    val test = List(1,2,3,4,5,6)
    Lista.myButLast(test) shouldEqual 5
  }

  "From the list 1,2,3,4,5,6, the function myButLast" should "return List(5,6)" in {
    val test = List(1,2,3,4,5,6)
    Lista.my2Last(test) shouldEqual List(5,6)
  }

  "From the list h,b,c,d the function elementAt" should "return c" in {
    val test = List("h","b","c","d")
    Lista.elementAt(test,3) shouldEqual "c"
  }

  "From the list 1,2,3,4 the function myLength" should "return 6" in {
    val test = List(1,2,3,4)
    Lista.myLength(test) shouldEqual 4
  }

  "From the list 1,2,3,4 the function myLengthFoldR" should "return 4" in {
    val test = List(1,2,3,4)
    Lista.myLengthFoldR(test) shouldEqual 4
  }

  "From the list 1,2,3,4 the function myLengthFoldL" should "return 4" in {
    val test = List(1,2,3,4)
    Lista.myLengthFoldR(test) shouldEqual 4
  }

  "From the list 1,2,3,4 the function myReverse" should "return List(4,3,2,1)" in {
    val test = List(1,2,3,4)
    Lista.myReverse(test) shouldEqual List(4,3,2,1)
  }

  "From the list 1,2,3 the function compress" should "return List(1,2,3)" in {
    val test = List(1,1,2,3,3,4)
    Lista.compress(test) shouldEqual List(1,2,3,4)
  }
}