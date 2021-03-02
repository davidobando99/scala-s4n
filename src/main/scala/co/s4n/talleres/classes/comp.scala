package co.s4n.talleres.classes

object comp {

  def cuadrado(value:Float): Float ={
      value*value
  }

  def cubo(value:Double):Double = {
    cuadrado(value.toFloat) * value
  }

}
