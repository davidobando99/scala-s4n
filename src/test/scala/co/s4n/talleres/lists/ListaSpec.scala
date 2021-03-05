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
}