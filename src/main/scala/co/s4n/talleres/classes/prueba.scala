package co.s4n.talleres.classes

object prueba {

  def x = {
    println("x")
    1
  }

  val y = {
    println("y")
    x +2
  }

  def z = {
    println("z")
    x
    x + "c"
  }
}
