package co.s4n.talleres.classes

class Contador(val contador:Int){

  def incr(c:Int = 1):Contador = {
    new Contador(this.contador+c)
  }

  def decr(c:Int = 1):Contador = {
    new Contador(this.contador-c)
  }

  def ajuste(sumador: Sumador):Contador = {
    val sum = sumador.adicionar(this.contador)
    new Contador(sum)
  }

}
