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

case class Cuadrado(lado:Double, numLados:Int) extends Forma {
  override def tamaño: Double = numLados
  override def perimetro:Double = 4*lado
  override def area: Double = lado * lado
}


//Exercise 3 y 4
sealed trait FormaS {
  def tamaño:Int
  def perimetro:Double
  def area:Double
}
trait Rectangular extends FormaS{
  def longitud:Double
  def altura:Double
}

case class CirculoR(radio:Double) extends FormaS {
  override def tamaño: Int = 0
  override def perimetro:Double = 2*math.Pi *radio
  override def area: Double = math.Pi * math.pow(radio,2)
}

case class RectanguloR(longitud:Double,altura:Double) extends Rectangular{
  override def tamaño: Int = 4
  override def perimetro: Double = 2*(longitud + altura)
  override def area: Double = longitud * altura
}

case class CuadradoR(longitud:Double,altura:Double) extends Rectangular{
  override def tamaño: Int = 4
  override def perimetro: Double = 4 * longitud
  override def area: Double = longitud * longitud
}

object Draw {
  def apply(forma:FormaS):String = forma match {
    case CirculoR(radio) =>  s"Un circulo de radio $radio cm"
    case RectanguloR(longitud,altura) =>  s"Un rectangulo de ancho $longitud cm y largo $altura cm"
    case CuadradoR(longitud,altura) =>  s"Un cuadrado de lado $longitud cm"
  }
}
