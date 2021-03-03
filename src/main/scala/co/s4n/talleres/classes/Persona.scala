package co.s4n.talleres.classes

class Persona(val nombre:String, val apellido:String) {
  def nombrec = s"$nombre $apellido"

}

object Persona{
  def apply(nombreCompleto:String) = {
    val nombres = nombreCompleto.split(" ")
    new Persona(nombres(0),nombres(1))
  }

  def main(args:Array[String]) ={
    val p1 = Persona("David Obando")
    println(p1.nombrec)
  }
}
