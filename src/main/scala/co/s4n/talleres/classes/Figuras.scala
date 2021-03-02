package co.s4n.talleres.classes

class Forma {
  def area: Double = 0.0

}

class Rectangulo(val ancho:Double, val altura:Double) extends Forma {
  override def area:Double = ancho * altura
}

class Circulo(val radio:Double) extends Forma {
  override def area: Double = math.Pi * radio * radio
}
