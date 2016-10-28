package com.pourtois.symbolette.expr

/**
  * Created by manu on 28.10.16.
  */


trait  Symbol extends Expr {

  val name : String
  val depend : List[Symbol]
  override def derivative(d: Expr): Expr = if (d == this) Expr.ONE
  else {
    d match {
      case s: Symbol => if (depend.contains(d)) Symbol(name, depend, s :: derivate) else Expr.ZERO
      case _ => Expr.ZERO
    }
  }




  override val optimized: List[Expr] = this :: Nil

  override def toString: String = {

    name +(depend match {
      case Nil => ""
      case _ =>  '(' + depend.map(_.name).mkString(",") + ')'
    })
  }

  val cost = 3
}

trait DerivatedSymbol extends Symbol {
  val derivate: List[Symbol]

  override def toString: String = {

    derivate.foldRight(name)((s,b) => b+","+s.name)+(depend match {
      case Nil => ""
      case _ =>  '(' + depend.map(_.name).mkString(",") + ')'
    })
  }
}

object Symbol {
  def apply(name: String, depend: List[Symbol] = Nil, derivate: List[Symbol] = Nil) :Symbol = ???
}




class Coordinates(val symbols: List[Symbol]) {

  def indice(name : String) = Indice(name,this)

}
case class IndiceDefinition(map : Map[Indice,Symbol]) {
  def symbol(indice : Indice): Symbol = map(indice)
}


case class Indice( name : String, coordinates : Coordinates) {

}
case class IndicedSymbol(name:String, indice:Indice) extends Symbol {
  val depend = Nil
  override val optimized: List[Expr] = optimize

  def optimize()(implicit definition: IndiceDefinition) :List[Expr] = definition.symbol(indice)::Nil
}

