package co.s4n.talleres.classes

class Director(val nombre:String, val apellido:String, val nacimiento: Int){

  def nombrec:String = s"$nombre $apellido"

  def copy(nombre:String = this.nombre, apellido:String =this.apellido, nacimiento:Int = this.nacimiento):Director =
    new Director(nombre,apellido,nacimiento)

}

object Director{

  def apply(nombre:String, apellido:String, nacimiento:Int) = new Director(nombre, apellido,nacimiento)

  def esMayor(director1:Director, director2:Director):Director = {
    if (director1.nacimiento >= director2.nacimiento) director1 else director2
  }

  def main(args:Array[String]) ={
    val d1 = Director("David", "Obando", 22)
    val d2 = Director("Luisa", "Muñoz", 25)
    println(d1.nombrec)
    println(esMayor(d1,d2).nombrec)
  }

}