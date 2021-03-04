package co.s4n.talleres.traits

trait Forma {

  def tamaño:Double
  def perimetro:Double
  def area:Double

}

case class Circulo(r:Double) extends Forma {
  override def tamaño: Double = 0.0
  override def perimetro:Double = 2*math.Pi *r
  override def area: Double = math.Pi * math.pow(r,2)
}

case class Rectangulo(base:Double, altura:Double, numLados:Int) extends Forma {
  override def tamaño: Double = numLados
  override def perimetro:Double = 2*(base + altura)
  override def area: Double = base * altura
}

case class Cuadraro(lado:Double, numLados:Int) extends Forma {
  override def tamaño: Double = numLados
  override def perimetro:Double = 4*lado
  override def area: Double = lado * lado
}
