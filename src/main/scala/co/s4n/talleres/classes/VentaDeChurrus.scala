package co.s4n.talleres.classes

object VentaDeChurrus {

  def despachar(cat: Gato) = if(cat.comida.equals("Churrus")) true else false
}
