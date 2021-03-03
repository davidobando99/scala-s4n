package co.s4n.talleres.classes

class Pelicula(val nombre:String, val presentacion:Int, val rangoIMDB:Double, val director: Director){

  def directorEdad = presentacion - director.nacimiento

  def esDirigidaPor(director:Director)= this.director == director

  def copy(nombre:String = this.nombre, presentacion:Int = this.presentacion,
           rangoIMDB:Double = this.rangoIMDB, director: Director = this.director):Pelicula = new Pelicula(nombre,presentacion,rangoIMDB,director)

}

object Pelicula{
  def apply(nombre:String, presentacion:Int, rangoIMDB:Double, director: Director) = new Pelicula(nombre,presentacion,rangoIMDB,director)

  def mejorCalificada(pe1: Pelicula, pe2:Pelicula):Pelicula = {
    if (pe1.rangoIMDB >= pe2.rangoIMDB) pe1 else pe2
  }

  def mayorDirectorEnElTiempo(pe1: Pelicula, pe2:Pelicula):Director = {
    if(pe1.directorEdad >= pe2.directorEdad) pe1.director else pe2.director
  }

  def main(args:Array[String]) ={
    val d1 = Director("David", "Obando", 22)
    val d2 = Director("Luisa", "Muñoz", 25)
    val pe1 = Pelicula("Maze Runner", 103, 23.4,d1)
    val pe2 = Pelicula("Joker", 98, 15.4,d2)
    println(mejorCalificada(pe1,pe2).nombre)
    println(mayorDirectorEnElTiempo(pe1,pe2).nombrec)
  }
}
