package co.s4n.talleres.inmutable

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class NatSpec extends AnyFlatSpec with Matchers {

  "Nat can be built" should "as Cero" in {
    val cero = Cero
    cero shouldEqual Cero
  }

  "Nat can be built" should "as Suc(Cero)" in {
    val uno = Suc(Cero)
    uno shouldEqual Suc(Cero)
  }

  "Nat can be built" should "as Suc(Suc(Cero))" in {
    val dos = Suc(Suc(Cero))
    dos shouldEqual Suc(Suc(Cero))
  }

  "From the test Nat Cero, the function fromNatToInt" should "return 0" in {
    val cero = Cero
    Nat.fromNatToInt(cero) shouldEqual 0
  }

  "From the test Nat Suc(Cero), the function fromNatToInt" should "return 1" in {
    val uno = Suc(Cero)
    Nat.fromNatToInt(uno) shouldEqual 1
  }

  "From the test Nat Suc(Suc(Suc(Suc(Suc(Cero))))), the function fromNatToInt" should "return 5" in {
    val cinco = Suc(Suc(Suc(Suc(Suc(Cero)))))
    Nat.fromNatToInt(cinco) shouldEqual 5
  }

  "From the test number 5 , the function fromNatToInt" should "return Suc(Suc(Suc(Suc(Suc(Cero)))))" in {
    val cinco = 5
    Nat.fromIntToNat(cinco) shouldEqual Suc(Suc(Suc(Suc(Suc(Cero)))))
  }

  "From the test number 1 , the function fromNatToInt" should "return Suc(Cero)" in {
    val uno = 1
    Nat.fromIntToNat(uno) shouldEqual Suc(Cero)
  }

  "From the test number 0 , the function fromNatToInt" should "return Cero" in {
    val cero = 0
    Nat.fromIntToNat(cero) shouldEqual Cero
  }



}