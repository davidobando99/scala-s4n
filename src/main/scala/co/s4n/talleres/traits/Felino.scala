package co.s4n.talleres.traits

trait Felino{
  def color:String
  def sonido:String



}

case class Leon(color: String,sonido: String, tamañoMelena:Int) extends Felino {

}
case class Tigre(color: String,sonido: String) extends Felino {

}
case class Jaguar(color: String,sonido: String) extends Felino{

}
case class Gato(color: String,sonido: String, comidaFav:String) extends Felino{

}

